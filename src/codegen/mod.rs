pub mod error;

use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, BasicValue, FloatValue, FunctionValue, PointerValue},
    FloatPredicate,
};

use crate::{
    lex::TokenKind,
    parser::{
        expr::{BinaryExpr, Expr, UnaryExpr},
        stmt::{Function, Prototype, Stmt},
        LiteralValue,
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
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value_opt.unwrap().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.f64_type(), name)
    }

    fn fn_value(&self, stmt_name: String) -> CodegenResult<FunctionValue<'ctx>> {
        match self.fn_value_opt {
            Some(fn_value) => Ok(fn_value),
            None => Err(CodegenError::OutsideOfFuncion(stmt_name)),
        }
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

        self.variables.reserve(proto.args.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let alloca = self.create_entry_block_alloca(proto.args[i].as_str());
            self.builder.build_store(alloca, arg);
            self.variables.insert(proto.args[i].clone(), alloca);
        }

        match self.compile_block(&self.function.body, self.function.return_expr.clone())? {
            Some(ret) => self.builder.build_return(Some(&ret)),
            None => self.builder.build_return(None),
        };

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
        let args_types = std::iter::repeat(self.context.f64_type())
            .take(proto.args.len())
            .map(|f| f.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();

        let args_types = args_types.as_slice();

        // Create the function
        let fn_type = self.context.f64_type().fn_type(args_types, false);
        // Add function to module symbol table
        let fn_val = self.module.add_function(proto.name.as_str(), fn_type, None);

        // Set names for all arguments.
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_float_value().set_name(proto.args[i].as_str());
        }

        Ok(fn_val)
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> CodegenResult<Option<FloatValue<'ctx>>> {
        match stmt {
            Stmt::Var { token, initializer } => {
                self.compile_var_declaration(token.lexeme.clone(), initializer)?;
                Ok(None)
            }
            Stmt::Expr(expr) => Ok(Some(self.compile_expr(expr)?)),
            _ => todo!(),
        }
    }

    fn compile_var_declaration(&mut self, name: String, initializer: &Expr) -> CodegenResult<()> {
        let alloca = self.create_entry_block_alloca(name.as_str());
        let compiled_expr = self.compile_expr(initializer)?;
        self.builder.build_store(alloca, compiled_expr);
        self.variables.remove(&name);
        self.variables.insert(name, alloca);
        Ok(())
    }

    fn compile_var_assignment(
        &mut self,
        name: &String,
        rhs: &Expr,
    ) -> CodegenResult<FloatValue<'ctx>> {
        let val = self.compile_expr(rhs)?;
        let var = self
            .variables
            .get(name)
            .ok_or(CodegenError::UndeclaredVariableOrOutOfScope(name.clone()))?;
        self.builder.build_store(*var, val);
        Ok(val)
    }

    fn compile_expr(&mut self, expr: &Expr) -> CodegenResult<FloatValue<'ctx>> {
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
            Expr::Block { body, return_expr } => self.compile_expr_block(
                body,
                *return_expr
                    .clone()
                    .ok_or(CodegenError::ExpectedReturnExpr())?,
            ),
        }
    }

    fn compile_expr_block(
        &mut self,
        body: &Vec<Stmt>,
        return_expr: Expr,
    ) -> CodegenResult<FloatValue<'ctx>> {
        self.compile_body(body)?;
        Ok(self.compile_expr(&return_expr)?)
    }

    fn compile_block(
        &mut self,
        body: &Vec<Stmt>,
        return_expr: Option<Expr>,
    ) -> CodegenResult<Option<FloatValue<'ctx>>> {
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
    ) -> CodegenResult<FloatValue<'ctx>> {
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
                    Some(value) => Ok(value.into_float_value()),
                    None => Err(CodegenError::InvalidCall(fn_name.to_owned())),
                }
            }
            None => Err(CodegenError::UnkownFunction(fn_name.to_owned())),
        }
    }

    fn compile_literal(&self, literal: &LiteralValue) -> CodegenResult<FloatValue<'ctx>> {
        match literal {
            LiteralValue::Boolean(_) => todo!(),
            LiteralValue::Number(number) => Ok(self.context.f64_type().const_float(*number)),
            LiteralValue::String(_) => todo!(),
        }
    }

    fn compile_binary(&mut self, expr: &BinaryExpr) -> CodegenResult<FloatValue<'ctx>> {
        let lhs = self.compile_expr(&*expr.left)?;
        let rhs = self.compile_expr(&*expr.right)?;

        match expr.operator.kind {
            TokenKind::Plus => Ok(self.builder.build_float_add(lhs, rhs, "addtmp")),
            TokenKind::Minus => Ok(self.builder.build_float_sub(lhs, rhs, "subtmp")),
            TokenKind::Star => Ok(self.builder.build_float_mul(lhs, rhs, "multmp")),
            TokenKind::Slash => Ok(self.builder.build_float_div(lhs, rhs, "divtmp")),
            TokenKind::Less => Ok({
                let cmp = self
                    .builder
                    .build_float_compare(FloatPredicate::ULT, lhs, rhs, "cmplttmp");

                self.builder
                    .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
            }),
            TokenKind::Greater => Ok({
                let cmp = self
                    .builder
                    .build_float_compare(FloatPredicate::UGT, lhs, rhs, "cmpgttmp");

                self.builder
                    .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
            }),
            TokenKind::LessEqual => Ok({
                let cmp = self
                    .builder
                    .build_float_compare(FloatPredicate::ULE, lhs, rhs, "cmpletmp");

                self.builder
                    .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
            }),
            TokenKind::GreaterEqual => Ok({
                let cmp = self
                    .builder
                    .build_float_compare(FloatPredicate::UGE, lhs, rhs, "cmpgetmp");

                self.builder
                    .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
            }),
            TokenKind::EqualEqual => Ok({
                let cmp = self
                    .builder
                    .build_float_compare(FloatPredicate::UEQ, lhs, rhs, "cmpeqtmp");

                self.builder
                    .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
            }),
            TokenKind::BangEqual => Ok({
                let cmp = self
                    .builder
                    .build_float_compare(FloatPredicate::UNE, lhs, rhs, "cmpnetmp");

                self.builder
                    .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
            }),
            _ => Err(CodegenError::UndefinedBinaryOperator(format!("{:#?}", expr.operator.kind))),
        }
    }

    fn compile_unary(&self, _expr: &UnaryExpr) -> CodegenResult<FloatValue<'ctx>> {
        todo!()
    }

    fn compile_variable(&self, name: &str) -> CodegenResult<FloatValue<'ctx>> {
        match self.variables.get(name) {
            Some(var) => Ok(self.builder.build_load(*var, name).into_float_value()),
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
    ) -> CodegenResult<FloatValue<'ctx>> {
        let cond = self.compile_expr(&cond)?;
        let cond = self.builder.build_float_compare(
            FloatPredicate::ONE,
            cond,
            self.context.f64_type().const_float(0.0),
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

        Ok(phi.as_basic_value().into_float_value())
    }
}
