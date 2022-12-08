mod builtin;
pub mod error;
mod st;

use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate,
};

use crate::{
    lexer::token::TokenKind,
    parser::{
        expr::{ArrayAccess, BinaryExpr, Block, Expr, FnCall, UnaryExpr},
        stmt::{Function, Prototype, Stmt},
        structs::{FieldAccess, FieldAccessField, Struct, StructExpr},
        ArrayType, LiteralType, Type,
    },
};

use self::{error::CodegenError, st::SymbolTable};

type CodegenResult<T> = Result<T, CodegenError>;

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    variables: SymbolTable<'ctx>,
    structs: HashMap<String, Struct>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
    ) -> Self {
        Self {
            context,
            builder,
            module,
            variables: SymbolTable::new(),
            structs: HashMap::new(),
            fn_value_opt: None,
        }
    }

    pub fn compile_builtin(&self) {
        self.build_printf();
    }

    // Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca<T: BasicType<'ctx>>(
        &self,
        name: &str,
        var_type: T,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value_opt.unwrap().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(var_type, name)
    }

    fn fn_value(&self, stmt_name: String) -> CodegenResult<FunctionValue<'ctx>> {
        self.fn_value_opt
            .ok_or(CodegenError::OutsideOfFuncion(stmt_name))
    }

    pub fn compile_fn(&mut self, function: Function) -> CodegenResult<FunctionValue<'ctx>> {
        self.variables.new_context();

        let proto = &function.prototype;
        let compiled_func = self.compile_prototype(proto)?;

        if function.is_builtin {
            return Ok(compiled_func);
        }

        let entry = self.context.append_basic_block(compiled_func, "entry");

        self.builder.position_at_end(entry);

        self.fn_value_opt = Some(compiled_func);

        for (i, arg) in compiled_func.get_param_iter().enumerate() {
            let alloca =
                self.create_entry_block_alloca(proto.params[i].identifier.as_str(), arg.get_type());
            self.builder.build_store(alloca, arg);
            self.variables
                .insert(proto.params[i].identifier.clone(), alloca);
        }

        match self.compile_block(&function.body, function.return_expr.clone())? {
            Some(ret) => self.builder.build_return(Some(&ret)),
            None => self.builder.build_return(None),
        };

        if std::env::args().any(|x| x == "-cc") {
            compiled_func.print_to_stderr();
        }

        self.variables.pop_context();

        if compiled_func.verify(true) {
            Ok(compiled_func)
        } else {
            unsafe {
                compiled_func.delete();
            }
            Err(CodegenError::InvalidGeneratedFunction())
        }
    }

    pub fn compile_struct(&mut self, struct_: &Struct) -> CodegenResult<StructType> {
        let struct_type = self.context.opaque_struct_type(&struct_.identifier);

        let field_types = struct_
            .fields
            .iter()
            .map(|(_, metadata)| self.get_llvm_type(&metadata.type_))
            .collect::<CodegenResult<Vec<BasicTypeEnum>>>()?;

        struct_type.set_body(&field_types, false);
        self.structs
            .insert(struct_.identifier.clone(), struct_.clone());
        Ok(struct_type)
    }

    fn compile_prototype(&self, proto: &Prototype) -> CodegenResult<FunctionValue<'ctx>> {
        let param_types = proto
            .params
            .iter()
            .map(|field| Ok(self.get_llvm_type(&field.type_)?.into()))
            .collect::<CodegenResult<Vec<BasicMetadataTypeEnum>>>()?;

        let fn_type = match &proto.return_type {
            Type::F64 => self.context.f64_type().fn_type(&param_types, false),
            Type::I64 => self.context.i64_type().fn_type(&param_types, false),
            Type::Void => self.context.void_type().fn_type(&param_types, false),
            Type::String => self
                .context
                .i8_type()
                .ptr_type(AddressSpace::Generic)
                .fn_type(&param_types, false),
            Type::Array(arr) => self
                .get_llvm_type(&arr.type_)?
                .array_type(arr.size)
                .fn_type(&param_types, false),
            invalid_type => return Err(CodegenError::InvalidType(invalid_type.clone())),
        };

        // Create the function
        // Add function to module symbol table
        let fn_val = self.module.add_function(proto.name.as_str(), fn_type, None);

        // Set names for all params.
        for (i, param) in fn_val.get_param_iter().enumerate() {
            param.set_name(proto.params[i].identifier.as_str());
        }

        Ok(fn_val)
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> CodegenResult<()> {
        match stmt {
            Stmt::Var { token, initializer } => {
                self.compile_var_declaration(token.lexeme.clone(), initializer)?;
            }
            Stmt::Expr(expr) => {
                self.compile_expr(expr)?;
            }
            Stmt::While { cond, body } => {
                self.compile_while(cond, body)?;
            }
            Stmt::Printf { fmt_string, args } => {
                self.compile_printf(fmt_string.clone(), args.clone())?;
            }
            _ => todo!(),
        };
        Ok(())
    }

    fn compile_var_declaration(&mut self, name: String, initializer: &Expr) -> CodegenResult<()> {
        let compiled_expr = self.compile_expr(initializer)?;
        let alloca = self.create_entry_block_alloca(name.as_str(), compiled_expr.get_type());
        self.builder.build_store(alloca, compiled_expr);
        self.variables.insert(name, alloca);
        Ok(())
    }

    fn compile_var_assignment(
        &mut self,
        name: &str,
        rhs: &Expr,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let val = self.compile_expr(rhs)?;

        let var = self.variables.get(name)?;

        self.builder.build_store(var, val);
        Ok(val)
    }

    fn compile_struct_expr(
        &mut self,
        struct_expr: &StructExpr,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let struct_type = self
            .module
            .get_struct_type(&struct_expr.identifier)
            .ok_or_else(|| CodegenError::UndefinedStruct(struct_expr.identifier.clone()))?;

        let struct_alloca = self.create_entry_block_alloca(
            &format!("tmp.{}", struct_expr.identifier),
            struct_type.as_basic_type_enum(),
        );

        let struct_fields = self
            .structs
            .get(&struct_expr.identifier)
            .ok_or_else(|| CodegenError::UndefinedStruct(struct_expr.identifier.clone()))?
            .fields
            .clone();

        if struct_expr.fields.len() != struct_fields.len() {
            Err(CodegenError::AllFieldsMustBeInitialized(
                struct_expr.identifier.clone(),
            ))?;
        }

        for (id, rhs) in struct_expr.fields.iter() {
            let index = struct_fields
                .get(id)
                .ok_or_else(|| CodegenError::UndefinedStructField(id.clone()))?
                .index;

            let field_ptr = self
                .builder
                .build_struct_gep(struct_alloca, index, &format!("tmp.{}", id))
                .map_err(|_| CodegenError::BuildStructGepFailed())?;
            let value = self.compile_expr(rhs)?;
            self.builder.build_store(field_ptr, value);
        }

        Ok(self.builder.build_load(struct_alloca, "tmp.deref"))
    }

    fn compile_field_access(
        &self,
        field_access: &FieldAccess,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let variable_ptr = self.variables.get(&field_access.variable_id)?;

        let field_ptr = self.get_field_ptr(field_access, &variable_ptr)?;
        Ok(self.builder.build_load(field_ptr, "tmp.deref"))
    }

    fn get_field_ptr(
        &self,
        field_access: &FieldAccess,
        variable_ptr: &PointerValue<'ctx>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let struct_type = match variable_ptr.get_type().get_element_type() {
            AnyTypeEnum::StructType(struct_type) => struct_type,
            unexpected_type => Err(CodegenError::ExpectedStruct(unexpected_type.to_string()))?,
        };

        let struct_name = struct_type.get_name().unwrap().to_str().unwrap();
        match *field_access.field.clone() {
            FieldAccessField::PrimitiveField(id) => {
                let index = self.get_field_index(id, struct_name.to_string())?;
                self.build_struct_gep(variable_ptr, index)
            }
            FieldAccessField::StructField(field_access) => {
                let field_access = match field_access {
                    Expr::FieldAccess(field_access) => field_access,
                    _ => panic!("Expected field access"),
                };

                let index = self
                    .get_field_index(field_access.variable_id.clone(), struct_name.to_string())?;

                let field_ptr = self.build_struct_gep(variable_ptr, index)?;

                Ok(self.get_field_ptr(&field_access, &field_ptr)?)
            }
        }
    }

    fn build_struct_gep(
        &self,
        variable_ptr: &PointerValue<'ctx>,
        index: u32,
    ) -> CodegenResult<PointerValue<'ctx>> {
        self.builder
            .build_struct_gep(*variable_ptr, index, "tmp.access")
            .map_err(|_| CodegenError::BuildStructGepFailed())
    }

    fn get_field_index(&self, identifier: String, struct_name: String) -> CodegenResult<u32> {
        Ok(self
            .structs
            .get(&struct_name)
            .ok_or(CodegenError::UndefinedStruct(struct_name))?
            .fields
            .get(&identifier)
            .ok_or(CodegenError::UndefinedStructField(identifier))?
            .index)
    }

    fn compile_expr(&mut self, expr: &Expr) -> CodegenResult<BasicValueEnum<'ctx>> {
        match expr {
            Expr::Literal(value) => self.compile_literal(value),
            Expr::Binary(expr) => self.compile_binary(expr),
            Expr::Unary(expr) => self.compile_unary(expr),
            Expr::Variable(var) => self.compile_variable(var.as_str()),
            Expr::FnCall(fn_call) => self.compile_fn_call(fn_call.clone()),
            Expr::If { cond, then, else_ } => {
                self.compile_conditional(*cond.clone(), *then.clone(), *else_.clone())
            }
            Expr::VariableAssignment { id, rhs } => self.compile_var_assignment(id, rhs),
            Expr::Block(block) => self.compile_expr_block(
                &block.body,
                *block
                    .return_expr
                    .clone()
                    .ok_or(CodegenError::ExpectedReturnExpr())?,
            ),
            Expr::Struct(struct_) => self.compile_struct_expr(struct_),
            Expr::FieldAccess(field_access) => self.compile_field_access(field_access),
            Expr::Array(type_, items) => self.compile_array(type_, items),
            Expr::ArrayAccess(access) => self.compile_array_access(access),
        }
    }

    fn compile_expr_block(
        &mut self,
        body: &[Stmt],
        return_expr: Expr,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.compile_body(body)?;
        self.compile_expr(&return_expr)
    }

    fn compile_block(
        &mut self,
        body: &[Stmt],
        return_expr: Option<Expr>,
    ) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        self.compile_body(body)?;
        let compiled_return = match return_expr {
            Some(expr) => Some(self.compile_expr(&expr)?),
            None => None,
        };
        Ok(compiled_return)
    }

    fn compile_body(&mut self, body: &[Stmt]) -> CodegenResult<()> {
        for stmt in body.iter() {
            self.compile_stmt(stmt)?;
        }
        Ok(())
    }

    fn compile_fn_call(&mut self, fn_call: FnCall) -> CodegenResult<BasicValueEnum<'ctx>> {
        match self.module.get_function(fn_call.fn_name.as_str()) {
            Some(fun) => {
                let mut compiled_args = Vec::with_capacity(fn_call.args.len());

                for arg in fn_call.args {
                    compiled_args.push(self.compile_expr(&arg)?);
                }

                let argsv: Vec<BasicMetadataValueEnum> = compiled_args
                    .iter()
                    .by_ref()
                    .map(|&val| val.into())
                    .collect();

                match self
                    .builder
                    .build_call(fun, argsv.as_slice(), "tmp")
                    .try_as_basic_value()
                    .left()
                {
                    Some(value) => Ok(value),
                    None => Err(CodegenError::InvalidCall(fn_call.fn_name.to_owned())),
                }
            }
            None => Err(CodegenError::UnkownFunction(fn_call.fn_name.to_owned())),
        }
    }

    fn compile_literal(&self, literal: &LiteralType) -> CodegenResult<BasicValueEnum<'ctx>> {
        match literal {
            LiteralType::Boolean(bool) => Ok(match bool {
                true => self
                    .context
                    .custom_width_int_type(1)
                    .const_all_ones()
                    .into(),
                false => self.context.custom_width_int_type(1).const_zero().into(),
            }),
            LiteralType::F64(number) => Ok(BasicValueEnum::FloatValue(
                self.context.f64_type().const_float(*number),
            )),
            LiteralType::I64(number) => Ok(BasicValueEnum::IntValue(
                self.context.i64_type().const_int(*number as u64, false),
            )),
            LiteralType::String(str) => Ok(self
                .builder
                .build_global_string_ptr(str, "tmp.literalstr")
                .as_basic_value_enum()),
        }
    }

    fn compile_binary(&mut self, expr: &BinaryExpr) -> CodegenResult<BasicValueEnum<'ctx>> {
        let lhs = self.compile_expr(&*expr.left)?;
        let rhs = self.compile_expr(&*expr.right)?;

        match expr.operator.kind {
            TokenKind::Plus => match (lhs, rhs) {
                (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => Ok(
                    BasicValueEnum::IntValue(self.builder.build_int_add(lhs, rhs, "addtmp")),
                ),
                (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => Ok(
                    BasicValueEnum::FloatValue(self.builder.build_float_add(lhs, rhs, "addtmp")),
                ),
                _ => Err(CodegenError::DifferentTypesBinOp()),
            },
            TokenKind::Minus => match (lhs, rhs) {
                (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => Ok(
                    BasicValueEnum::IntValue(self.builder.build_int_sub(lhs, rhs, "subtmp")),
                ),
                (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => Ok(
                    BasicValueEnum::FloatValue(self.builder.build_float_sub(lhs, rhs, "subtmp")),
                ),
                _ => Err(CodegenError::DifferentTypesBinOp()),
            },
            TokenKind::Star => match (lhs, rhs) {
                (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => Ok(
                    BasicValueEnum::IntValue(self.builder.build_int_mul(lhs, rhs, "multmp")),
                ),
                (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => Ok(
                    BasicValueEnum::FloatValue(self.builder.build_float_mul(lhs, rhs, "multmp")),
                ),
                _ => Err(CodegenError::DifferentTypesBinOp()),
            },
            TokenKind::Slash => match (lhs, rhs) {
                (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => Ok(
                    BasicValueEnum::IntValue(self.builder.build_int_signed_div(lhs, rhs, "divtmp")),
                ),
                (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => Ok(
                    BasicValueEnum::FloatValue(self.builder.build_float_div(lhs, rhs, "divtmp")),
                ),
                _ => Err(CodegenError::DifferentTypesBinOp()),
            },
            TokenKind::Less => match (lhs, rhs) {
                (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                    Ok(BasicValueEnum::IntValue(self.builder.build_int_compare(
                        IntPredicate::ULT,
                        lhs,
                        rhs,
                        "cmplttmp",
                    )))
                }
                (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                    Ok(BasicValueEnum::IntValue(self.builder.build_float_compare(
                        FloatPredicate::ULT,
                        lhs,
                        rhs,
                        "cmplttmp",
                    )))
                }
                _ => Err(CodegenError::DifferentTypesBinOp()),
            },
            TokenKind::Greater => match (lhs, rhs) {
                (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                    Ok(BasicValueEnum::IntValue(self.builder.build_int_compare(
                        IntPredicate::UGT,
                        lhs,
                        rhs,
                        "cmpgttmp",
                    )))
                }
                (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                    Ok(BasicValueEnum::IntValue(self.builder.build_float_compare(
                        FloatPredicate::UGT,
                        lhs,
                        rhs,
                        "cmpgttmp",
                    )))
                }
                _ => Err(CodegenError::DifferentTypesBinOp()),
            },
            TokenKind::LessEqual => match (lhs, rhs) {
                (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                    Ok(BasicValueEnum::IntValue(self.builder.build_int_compare(
                        IntPredicate::ULE,
                        lhs,
                        rhs,
                        "cmplttmp",
                    )))
                }
                (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                    Ok(BasicValueEnum::IntValue(self.builder.build_float_compare(
                        FloatPredicate::ULE,
                        lhs,
                        rhs,
                        "cmplttmp",
                    )))
                }
                _ => Err(CodegenError::DifferentTypesBinOp()),
            },
            TokenKind::GreaterEqual => match (lhs, rhs) {
                (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                    Ok(BasicValueEnum::IntValue(self.builder.build_int_compare(
                        IntPredicate::UGE,
                        lhs,
                        rhs,
                        "cmpgetmp",
                    )))
                }
                (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                    Ok(BasicValueEnum::IntValue(self.builder.build_float_compare(
                        FloatPredicate::UGE,
                        lhs,
                        rhs,
                        "cmpgetmp",
                    )))
                }
                _ => Err(CodegenError::DifferentTypesBinOp()),
            },
            TokenKind::EqualEqual => match (lhs, rhs) {
                (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                    Ok(BasicValueEnum::IntValue(self.builder.build_int_compare(
                        IntPredicate::EQ,
                        lhs,
                        rhs,
                        "cmpeqtmp",
                    )))
                }
                (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                    Ok(BasicValueEnum::IntValue(self.builder.build_float_compare(
                        FloatPredicate::UEQ,
                        lhs,
                        rhs,
                        "cmpeqtmp",
                    )))
                }
                _ => Err(CodegenError::DifferentTypesBinOp()),
            },
            TokenKind::BangEqual => match (lhs, rhs) {
                (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                    Ok(BasicValueEnum::IntValue(self.builder.build_int_compare(
                        IntPredicate::NE,
                        lhs,
                        rhs,
                        "cmpnetmp",
                    )))
                }
                (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                    Ok(BasicValueEnum::IntValue(self.builder.build_float_compare(
                        FloatPredicate::UNE,
                        lhs,
                        rhs,
                        "cmpnetmp",
                    )))
                }
                _ => Err(CodegenError::DifferentTypesBinOp()),
            },
            TokenKind::And => Ok(BasicValueEnum::IntValue(self.builder.build_and(
                lhs.into_int_value(),
                rhs.into_int_value(),
                "andtmp",
            ))),
            TokenKind::Or => Ok(BasicValueEnum::IntValue(self.builder.build_or(
                lhs.into_int_value(),
                rhs.into_int_value(),
                "ortmp",
            ))),
            _ => Err(CodegenError::UndefinedBinaryOperator(format!(
                "{:#?}",
                expr.operator.kind
            ))),
        }
    }

    fn compile_unary(&self, _expr: &UnaryExpr) -> CodegenResult<BasicValueEnum<'ctx>> {
        todo!()
    }

    fn compile_variable(&self, name: &str) -> CodegenResult<BasicValueEnum<'ctx>> {
        Ok(self.builder.build_load(self.variables.get(name)?, name))
    }

    fn compile_conditional(
        &mut self,
        cond: Expr,
        then: Expr,
        else_: Expr,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let cond = self.compile_expr(&cond)?;

        let cond = self.builder.build_int_compare(
            IntPredicate::NE,
            cond.into_int_value(),
            self.context.custom_width_int_type(1).const_zero(),
            "ifcond",
        );

        let parent = self.fn_value("Conditional".to_string())?;
        let then_bb = self.context.append_basic_block(parent, "then");
        let else_bb = self.context.append_basic_block(parent, "else");
        let cont_bb = self.context.append_basic_block(parent, "ifcont");

        self.builder
            .build_conditional_branch(cond, then_bb, else_bb);

        self.builder.position_at_end(then_bb);
        let then_val = self.compile_expr(&then)?;
        self.builder.build_unconditional_branch(cont_bb);

        let then_bb = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(else_bb);
        let else_val = self.compile_expr(&else_)?;
        self.builder.build_unconditional_branch(cont_bb);

        let else_bb = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(cont_bb);

        let phi = match (then_val, else_val) {
            (BasicValueEnum::IntValue(_), BasicValueEnum::IntValue(_)) => {
                self.builder.build_phi(self.context.i64_type(), "iftmp")
            }
            (BasicValueEnum::FloatValue(_), BasicValueEnum::FloatValue(_)) => {
                self.builder.build_phi(self.context.f64_type(), "iftmp")
            }
            _ => return Err(CodegenError::DifferentReturnTypesBranch()),
        };

        phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

        Ok(phi.as_basic_value())
    }

    fn compile_while(&mut self, cond: &Expr, body: &Block) -> CodegenResult<()> {
        let parent = self.fn_value("while loop".to_string())?;

        let while_condition_block = self.context.append_basic_block(parent, "while_condition");
        let while_block = self.context.append_basic_block(parent, "while");
        let after_while_block = self.context.append_basic_block(parent, "afterwhile");

        self.builder
            .build_unconditional_branch(while_condition_block);
        self.builder.position_at_end(while_condition_block);

        let end_cond = self.compile_expr(cond)?;
        let end_cond = self.builder.build_int_compare(
            IntPredicate::NE,
            end_cond.into_int_value(),
            self.context.custom_width_int_type(1).const_zero(),
            "endcond",
        );

        self.builder
            .build_conditional_branch(end_cond, while_block, after_while_block);

        self.builder.position_at_end(while_block);

        self.compile_block(&body.body, body.return_expr.clone().map(|x| *x))?;

        self.builder
            .build_unconditional_branch(while_condition_block);

        self.builder.position_at_end(after_while_block);

        Ok(())
    }

    fn get_llvm_type(&self, type_: &Type) -> CodegenResult<BasicTypeEnum<'ctx>> {
        match type_ {
            Type::F64 => Ok(self.context.f64_type().into()),
            Type::I64 => Ok(self.context.i64_type().into()),
            Type::Struct(id) => Ok(self
                .module
                .get_struct_type(id)
                .ok_or_else(|| CodegenError::UndefinedStruct(id.clone()))?
                .into()),
            Type::String => Ok(self
                .context
                .i8_type()
                .ptr_type(AddressSpace::Generic)
                .into()),
            Type::Array(arr) => Ok(self.get_llvm_type(&arr.type_)?.array_type(arr.size).into()),
            invalid_type => Err(CodegenError::InvalidType(invalid_type.clone())),
        }
    }

    fn compile_printf(
        &mut self,
        fmt_string: String,
        args: Vec<Expr>,
    ) -> CodegenResult<BasicValueEnum> {
        match self.module.get_function("printf") {
            Some(func) => {
                let mut printf_args = Vec::new();

                printf_args.push(
                    self.builder
                        .build_global_string_ptr(&fmt_string, "fmt_string")
                        .as_basic_value_enum(),
                );

                for arg in args {
                    printf_args.push(self.compile_expr(&arg)?)
                }

                let argsv: Vec<BasicMetadataValueEnum> =
                    printf_args.iter().by_ref().map(|&val| val.into()).collect();

                match self
                    .builder
                    .build_call(func, &argsv, "tmp.printf_call")
                    .try_as_basic_value()
                    .left()
                {
                    Some(value) => Ok(value),
                    None => Err(CodegenError::InvalidCall("printf".to_string())),
                }
            }
            None => Err(CodegenError::UnkownFunction("printf".to_string())),
        }
    }

    fn compile_array(
        &mut self,
        type_: &Type,
        items: &Vec<Expr>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let type_ = self.get_llvm_type(&Type::Array(ArrayType {
            type_: Box::new(type_.clone()),
            size: items.len() as u32,
        }))?;

        let ptr = self.create_entry_block_alloca("tmp.arr", type_);

        let zero = self.context.i64_type().const_zero();

        for (i, item) in items.iter().enumerate() {
            let compiled_expr = self.compile_expr(item)?;

            let index = self.context.i64_type().const_int(i as u64, false);

            let item_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(ptr, &[zero, index], "arr.gep")
            };

            self.builder.build_store(item_ptr, compiled_expr);
        }

        Ok(ptr.into())
    }

    fn compile_array_access(
        &mut self,
        access: &ArrayAccess,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let zero = self.context.i32_type().const_zero();

        let index = self.compile_expr(&*access.index)?.into_int_value();

        let variable_ptr = self.variables.get(&access.variable_id)?;

        let variable_ptr = self
            .builder
            .build_load(variable_ptr, "deref")
            .into_pointer_value();

        let item_ptr = unsafe {
            self.builder
                .build_in_bounds_gep(variable_ptr, &[zero, index], "indx.expr")
        };

        Ok(self.builder.build_load(item_ptr, "tmp.deref.arrayindex"))
    }
}
