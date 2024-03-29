mod codegen;
mod error;
mod lexer;
mod parser;

use std::{env, fs, process};

use error::CompilerError;
use inkwell::{context::Context, module::Module, passes::PassManager, OptimizationLevel};
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

    let args: Vec<String> = env::args().collect();

    if args.len() < 2 || args[1] == "--help" {
        display_help();
    } else {
        run_file(&args[1]);
    }
}

fn display_help() {
    println!("The Craft Programming Language\n");
    println!("craft <file> [options]");
    println!("Options:");
    println!("  -l  display lexer output");
    println!("  -p  display AST");
    println!("  -c  display generated code");
}

fn run_file(path: &String) {
    let source = fs::read_to_string(path).expect("Could not read file");
    if let Err(err) = run(source) {
        eprintln!("{err}");
        process::exit(1);
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
        println!("{tokens:#?}");
        println!("----------------------------------");
    }

    let mut parser = Parser::new(tokens.to_vec());
    let ast = parser.parse()?;

    if display_parser_output {
        println!("----------------------------------");
        println!("{ast:#?}");
        println!("----------------------------------");
    }

    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();
    let fpm = PassManager::create(&module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();

    fpm.initialize();

    let mut compiler = Compiler::new(&context, &builder, &module, &fpm);

    compiler.compile_builtin();

    for node in ast {
        match node {
            Stmt::Function(func) => {
                compiler.compile_fn(func)?;
            }
            Stmt::Struct(struct_) => {
                compiler.compile_struct(&struct_)?;
            }
            _ => panic!("Unexpected statement, {node:#?}"),
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
        .create_jit_execution_engine(OptimizationLevel::Default)
        .unwrap();

    let maybe_fn = unsafe { ee.get_function::<unsafe extern "C" fn()>(PROGRAM_STARTING_POINT) };

    let compiled_fn = match maybe_fn {
        Ok(f) => f,
        Err(err) => {
            println!("!> Error during execution: {err:?}");
            return;
        }
    };

    unsafe {
        compiled_fn.call();
    }
}
