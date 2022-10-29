mod codegen;
mod error;
mod external;
mod lexer;
mod parser;

use std::{env, fs};

use error::CompilerError;
use inkwell::{context::Context, module::Module, OptimizationLevel};
use lexer::lex::Scanner;

use crate::{
    codegen::Compiler,
    parser::{stmt::Stmt, Parser},
};

#[macro_use]
extern crate log;

pub const PROGRAM_STARTING_POINT: &str = "main";

fn main() {
    env_logger::init();
    external::shim_builtin_functions();

    let args: Vec<String> = env::args().collect();
    run_file(&args[1]);
}

fn run_file(path: &String) {
    let source = fs::read_to_string(path).expect("Could not read file");
    if let Err(err) = run(source) {
        println!("{}", err);
    }
}

fn run(source: String) -> Result<(), CompilerError> {
    let mut display_lexer_output = false;
    let mut display_parser_output = false;
    let mut display_compiler_output = false;

    for arg in env::args() {
        match arg.as_str() {
            "-l" => display_lexer_output = true,
            "-p" => display_parser_output = true,
            "-c" => display_compiler_output = true,
            _ => (),
        }
    }

    trace!("\n{}", source);
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();

    if display_lexer_output {
        println!("----------------------------------");
        println!("{:#?}", tokens);
        println!("----------------------------------");
    }

    let mut parser = Parser::new(tokens.to_vec());
    let mut ast = external::builtin_funtions();
    ast.append(&mut parser.parse()?);

    if display_parser_output {
        println!("----------------------------------");
        println!("{:#?}", ast);
        println!("----------------------------------");
    }

    let context = Context::create();
    let module = context.create_module("repl");
    let builder = context.create_builder();

    let mut compiler = Compiler::new(&context, &builder, &module);

    for node in ast {
        match node {
            Stmt::Function(func) => {
                compiler.compile_fn(func)?;
            }
            Stmt::Struc(struct_) => {
                compiler.compile_struct(struct_)?;
            }
            _ => panic!("Unexpected statement, {:#?}", node),
        };
    }

    if display_compiler_output {
        println!("---------------------------------------");
        module.print_to_stderr();
        println!("---------------------------------------");
    }

    run_jit(&module);

    Ok(())
}

fn run_jit(module: &Module) {
    let ee = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let maybe_fn =
        unsafe { ee.get_function::<unsafe extern "C" fn() -> f64>(PROGRAM_STARTING_POINT) };

    let compiled_fn = match maybe_fn {
        Ok(f) => f,
        Err(err) => {
            println!("!> Error during execution: {:?}", err);
            return;
        }
    };

    unsafe {
        println!("=> {}", compiled_fn.call());
    }
}
