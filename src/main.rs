mod codegen;
mod error;
mod lex;
mod parser;

use std::{
    env, fs,
    io::{self, Write},
};

use error::CompilerError;
use lex::Scanner;
use llvm::{context::Context, module::Module, OptimizationLevel};
use parser::parser::{Function, Parser, ANONYMOUS_FUNCTION_NAME};

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
    if let Err(err) = run(source) {
        println!("{}", err);
    }
}

fn run_repl() {
    loop {
        let mut input = String::new();

        print_prompt();

        io::stdin()
            .read_line(&mut input)
            .expect("Could not read line");

        if input.starts_with("exit") || input.starts_with("quit") {
            break;
        } else if input.chars().all(char::is_whitespace) {
            continue;
        }
        
        if let Err(err) = run(input) {
            println!("{}", err);
        }
    }
}

fn run(source: String) -> Result<(), CompilerError> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens.to_vec());
    let func = parser.parse()?;
    compile(func)?;
    Ok(())
}

fn compile(func: Function) -> Result<(), CompilerError> {
    let context = Context::create();
    let module = context.create_module("repl");
    let builder = context.create_builder();

    match Compiler::compile(&context, &builder, &module, &func) {
        Ok(function) => {
            println!("--------------------------------");
            function.print_to_stderr();
            println!("--------------------------------");
        }
        Err(err) => return Err(CompilerError::CompileError(err.to_string())),
    }

    run_jit(&module);
    Ok(())
}

fn run_jit(module: &Module) {
    let ee = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let maybe_fn =
        unsafe { ee.get_function::<unsafe extern "C" fn() -> f64>(ANONYMOUS_FUNCTION_NAME) };

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

fn print_prompt() {
    print!(">> ");
    io::stdout().flush().unwrap();
}
