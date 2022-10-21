pub mod error;

use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    FloatPredicate, IntPredicate,
};

use crate::{
    lexer::token::TokenKind,
    parser::{
        expr::{BinaryExpr, Expr, UnaryExpr},
        stmt::{Function, Prototype, Stmt},
        LiteralType,
    },
};

use self::error::CodegenError;

type CodegenResult<T> = Result<T, CodegenError>;

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub function: &'a Function,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        function: &'a Function,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        let mut compiler = Self {
            context: &context,
            builder: &builder,
            module: &module,
            function,
            variables: HashMap::new(),
            fn_value_opt: None,
        };
        compiler.compile_fn()
    }

    // Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(
        &self,
        name: &str,
        value_type: BasicValueEnum,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value_opt.unwrap().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        match value_type {
            BasicValueEnum::IntValue(_) => builder.build_alloca(self.context.i64_type(), name),
            BasicValueEnum::FloatValue(_) => builder.build_alloca(self.context.f64_type(), name),
            _ => unimplemented!("Only int and float are supported at the moment"),
        }
    }

    fn fn_value(&self, stmt_name: String) -> CodegenResult<FunctionValue<'ctx>> {
        self.fn_value_opt
            .ok_or(CodegenError::OutsideOfFuncion(stmt_name))
    }

    fn compile_fn(&mut self) -> CodegenResult<FunctionValue<'ctx>> {
        let proto = &self.function.prototype;
        let function = self.compile_prototype(proto)?;

        if self.function.is_builtin {
            return Ok(function);
        }

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        self.fn_value_opt = Some(function);

        self.variables.reserve(proto.params.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let alloca = self.create_entry_block_alloca(proto.params[i].name.as_str(), arg);
            self.builder.build_store(alloca, arg);
            self.variables.insert(proto.params[i].name.clone(), alloca);
        }

        match self.compile_block(&self.function.body, self.function.return_expr.clone())? {
            Some(ret) => self.builder.build_return(Some(&ret)),
            None => self.builder.build_return(None),
        };

        if std::env::args().any(|x| x == "-c".to_string()) {
            function.print_to_stderr();
        }

        if function.verify(true) {
            Ok(function)
        } else {
            unsafe {
                function.delete();
            }
            Err(CodegenError::InvalidGeneratedFunction())
        }
    }

    fn compile_prototype(&self, proto: &Prototype) -> CodegenResult<FunctionValue<'ctx>> {
        // Creates n 64 bit floats, since, for the moment, only floats can be used as args (being n the number of args)
        let param_types = proto
            .params
            .iter()
            .map(|param| match param.type_.as_str() {
                "f64" => Ok(self.context.f64_type().into()),
                "i64" => Ok(self.context.i64_type().into()),
                invalid_type => return Err(CodegenError::InvalidType(invalid_type.to_string())),
            })
            .collect::<CodegenResult<Vec<BasicMetadataTypeEnum>>>()?;

        let fn_type = match proto.return_type.as_str() {
            "f64" => self.context.f64_type().fn_type(&param_types, false),
            "i64" => self.context.i64_type().fn_type(&param_types, false),
            "void" => self.context.void_type().fn_type(&param_types, false),
            invalid_type => return Err(CodegenError::InvalidType(invalid_type.to_string())),
        };

        // Create the function
        // Add function to module symbol table
        let fn_val = self.module.add_function(proto.name.as_str(), fn_type, None);

        // Set names for all params.
        for (i, param) in fn_val.get_param_iter().enumerate() {
            param.set_name(proto.params[i].name.as_str());
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
            _ => todo!(),
        };
        Ok(())
    }

    fn compile_var_declaration(&mut self, name: String, initializer: &Expr) -> CodegenResult<()> {
        let compiled_expr = self.compile_expr(initializer)?;
        let alloca = self.create_entry_block_alloca(name.as_str(), compiled_expr);
        self.builder.build_store(alloca, compiled_expr);
        self.variables.remove(&name);
        self.variables.insert(name, alloca);
        Ok(())
    }

    fn compile_var_assignment(
        &mut self,
        name: &String,
        rhs: &Expr,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let val = self.compile_expr(rhs)?;
        let var = self
            .variables
            .get(name)
            .ok_or(CodegenError::UndeclaredVariableOrOutOfScope(name.clone()))?;
        self.builder.build_store(*var, val);
        Ok(val)
    }

    fn compile_expr(&mut self, expr: &Expr) -> CodegenResult<BasicValueEnum<'ctx>> {
        match expr {
            Expr::Literal { value } => self.compile_literal(value),
            Expr::Binary(expr) => self.compile_binary(expr),
            Expr::Unary(expr) => self.compile_unary(expr),
            Expr::Variable(var) => self.compile_variable(var.as_str()),
            Expr::FnCall { fn_name, args } => self.compile_fn_call(fn_name, args),
            Expr::Conditional { cond, then, else_ } => {
                self.compile_conditional(*cond.clone(), *then.clone(), *else_.clone())
            }
            Expr::VariableAssignment { id, rhs } => self.compile_var_assignment(id, &*rhs),
            Expr::Block(block) => self.compile_expr_block(
                &block.body,
                *block
                    .return_expr
                    .clone()
                    .ok_or(CodegenError::ExpectedReturnExpr())?,
            ),
        }
    }

    fn compile_expr_block(
        &mut self,
        body: &Vec<Stmt>,
        return_expr: Expr,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.compile_body(body)?;
        Ok(self.compile_expr(&return_expr)?)
    }

    fn compile_block(
        &mut self,
        body: &Vec<Stmt>,
        return_expr: Option<Expr>,
    ) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        self.compile_body(body)?;
        let compiled_return = match return_expr {
            Some(expr) => Some(self.compile_expr(&expr)?),
            None => None,
        };
        Ok(compiled_return)
    }

    fn compile_body(&mut self, body: &Vec<Stmt>) -> CodegenResult<()> {
        for stmt in body.iter() {
            self.compile_stmt(stmt)?;
        }
        Ok(())
    }

    fn compile_fn_call(
        &mut self,
        fn_name: &String,
        args: &Vec<Expr>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match self.module.get_function(fn_name.as_str()) {
            Some(fun) => {
                let mut compiled_args = Vec::with_capacity(args.len());

                for arg in args {
                    compiled_args.push(self.compile_expr(arg)?);
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
                    None => Err(CodegenError::InvalidCall(fn_name.to_owned())),
                }
            }
            None => Err(CodegenError::UnkownFunction(fn_name.to_owned())),
        }
    }

    fn compile_literal(&self, literal: &LiteralType) -> CodegenResult<BasicValueEnum<'ctx>> {
        match literal {
            LiteralType::Boolean(_) => todo!(),
            LiteralType::F64(number) => Ok(BasicValueEnum::FloatValue(
                self.context.f64_type().const_float(*number),
            )),
            LiteralType::I64(number) => Ok(BasicValueEnum::IntValue(
                self.context.i64_type().const_int(*number as u64, false),
            )),
            LiteralType::String(_) => todo!(),
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
        match self.variables.get(name) {
            Some(var) => Ok(self.builder.build_load(*var, name)),
            None => Err(CodegenError::UndeclaredVariableOrOutOfScope(
                name.to_string(),
            )),
        }
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

        let phi = self.builder.build_phi(self.context.f64_type(), "iftmp");

        phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

        Ok(phi.as_basic_value())
    }

    fn compile_while(&mut self, cond: &Expr, body: &Expr) -> CodegenResult<()> {
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

        self.compile_expr(body)?;

        self.builder
            .build_unconditional_branch(while_condition_block);

        self.builder.position_at_end(after_while_block);

        Ok(())
    }
}
