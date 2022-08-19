mod codegen;
mod error;
mod lex;
mod parser;

use std::{env, fs, io};

use error::CompileError;
use lex::Scanner;
use llvm::context::Context;
use parser::parser::Parser;

use crate::codegen::Compiler;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: wombat [script]");
    } else if args.len() == 2 {
        run_file(&args[1]);
    } else {
        run_repl();
    }
}

fn run_file(path: &String) {
    let source = fs::read_to_string(path).expect("Could not read file");
    run(source).unwrap();
}

fn run_repl() {
    loop {
        let mut buffer = String::new();
        io::stdin()
            .read_line(&mut buffer)
            .expect("Could not read line");
        run(buffer).unwrap();
    }
}

fn run(source: String) -> Result<(), CompileError> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens.to_vec());

    let context = Context::create();
    let module = context.create_module("repl");
    let builder = context.create_builder();

    match parser.parse() {
        Ok(func) => {
            match Compiler::compile(&context, &builder, &module, &func) {
                Ok(function) => {
                    println!("-> Expression compiled to IR:");
                    function.print_to_stderr();
                }
                Err(err) => {
                    println!("!> Error compiling function: {}", err);
                }
            }
        }
        Err(e) => return Err(e.into()),
    }
    return Ok(());
}
