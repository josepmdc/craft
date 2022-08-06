pub mod error;
pub mod lex;
pub mod parser;

use std::{env, fs, io};

use lex::Scanner;
use parser::parser::Parser;

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
    run(source);
}

fn run_repl() {
    loop {
        let mut buffer = String::new();
        io::stdin()
            .read_line(&mut buffer)
            .expect("Could not read line");
        run(buffer);
    }
}

fn run(source: String) {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens.to_vec());
    let expr = parser.parse();
    println!("{:#?}", expr);
}
