#![allow(dead_code, unused_variables)]

pub mod error;

use std::collections::HashMap;

use llvm::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{BasicValue, FloatValue, FunctionValue, PointerValue},
    FloatPredicate,
};

use crate::{
    lex::TokenType,
    parser::parser::{BinaryExpr, Expr, Function, LiteralValue, Prototype, UnaryExpr},
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

    fn compile_fn(&mut self) -> CodegenResult<FunctionValue<'ctx>> {
        let proto = &self.function.prototype;
        let function = self.compile_prototype(proto)?;

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        self.fn_value_opt = Some(function);

        self.variables.reserve(proto.args.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let alloca = self.create_entry_block_alloca(proto.args[i].as_str());
            self.builder.build_store(alloca, arg);
            self.variables.insert(proto.args[i].clone(), alloca);
        }

        match &self.function.body {
            Some(body) => {
                let mut res = self.compile_expr(&body[0])?;

                for expr in body.iter().skip(1) {
                    res = self.compile_expr(expr)?;
                }

                self.builder.build_return(Some(&res));
            }
            None => {
                self.builder.build_return(None);
                return Ok(function);
            }
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

    fn compile_expr(&self, expr: &Expr) -> CodegenResult<FloatValue<'ctx>> {
        match expr {
            Expr::Literal { value } => self.compile_literal(value),
            Expr::Binary(expr) => self.compile_binary(expr),
            Expr::Unary(expr) => self.compile_unary(expr),
            Expr::Grouping { expression } => self.compile_grouping(),
            Expr::Variable(name) => self.compile_variable(name.as_str()),
        }
    }

    fn compile_literal(&self, literal: &LiteralValue) -> CodegenResult<FloatValue<'ctx>> {
        match literal {
            LiteralValue::Boolean(_) => todo!(),
            LiteralValue::Number(number) => Ok(self.context.f64_type().const_float(*number)),
            LiteralValue::String(_) => todo!(),
        }
    }

    fn compile_binary(&self, expr: &BinaryExpr) -> CodegenResult<FloatValue<'ctx>> {
        let lhs = self.compile_expr(&*expr.left)?;
        let rhs = self.compile_expr(&*expr.right)?;

        match expr.operator.type_ {
            TokenType::Plus => Ok(self.builder.build_float_add(lhs, rhs, "addtmp")),
            TokenType::Minus => Ok(self.builder.build_float_sub(lhs, rhs, "subtmp")),
            TokenType::Star => Ok(self.builder.build_float_mul(lhs, rhs, "multmp")),
            TokenType::Slash => Ok(self.builder.build_float_div(lhs, rhs, "divtmp")),
            TokenType::Less => Ok({
                let cmp = self
                    .builder
                    .build_float_compare(FloatPredicate::ULT, lhs, rhs, "cmptmp");

                self.builder
                    .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
            }),
            TokenType::Greater => Ok({
                let cmp = self
                    .builder
                    .build_float_compare(FloatPredicate::ULT, rhs, lhs, "cmptmp");

                self.builder
                    .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
            }),
            _ => Err(CodegenError::UndefinedBinaryOperator()),
        }
    }

    fn compile_unary(&self, expr: &UnaryExpr) -> CodegenResult<FloatValue<'ctx>> {
        todo!()
    }

    fn compile_grouping(&self) -> CodegenResult<FloatValue<'ctx>> {
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
}
