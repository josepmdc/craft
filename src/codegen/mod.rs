#![allow(dead_code, unused_variables)]

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

type CompilerResult<'ctx> = Result<FloatValue<'ctx>, &'static str>;

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
    ) -> Result<FunctionValue<'ctx>, &'static str> {
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

    fn compile_fn(&mut self) -> Result<FunctionValue<'ctx>, &'static str> {
        let proto = &self.function.prototype;
        let function = self.compile_prototype(proto)?;

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        self.fn_value_opt = Some(function);

        self.variables.reserve(proto.args.len());
        for (i, arg) in function.get_param_iter().enumerate() {
            self.variables
                .insert(proto.args[i].clone(), arg.into_pointer_value());
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
            Err("Invalid generated function.")
        }
    }

    fn compile_prototype(&self, proto: &Prototype) -> Result<FunctionValue<'ctx>, &'static str> {
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

    fn compile_expr(&self, expr: &Expr) -> CompilerResult {
        match expr {
            Expr::Literal { value } => self.compile_literal(value),
            Expr::Binary(expr) => self.compile_binary(expr),
            Expr::Unary(expr) => self.compile_unary(expr),
            Expr::Grouping { expression } => self.compile_grouping(),
        }
    }

    fn compile_literal(&self, literal: &LiteralValue) -> CompilerResult {
        match literal {
            LiteralValue::Boolean(_) => todo!(),
            LiteralValue::Number(number) => Ok(self.context.f64_type().const_float(*number)),
            LiteralValue::String(_) => todo!(),
        }
    }

    fn compile_binary(&self, expr: &BinaryExpr) -> CompilerResult {
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
            _ => Err("Undefined binary operator"),
        }
    }

    fn compile_unary(&self, expr: &UnaryExpr) -> CompilerResult {
        todo!()
    }

    fn compile_grouping(&self) -> CompilerResult {
        todo!()
    }
}
