pub mod error;
pub mod lex;

use std::{env, fs, io};

use lex::Scanner;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: wombat [script]");
    } else if args.len() == 2 {
        run_file(&args[1]);
    } else {
        run_prompt();
    }
}

fn run_file(path: &String) {
    let source = fs::read_to_string(path).expect("Could not read file");
    run(source);
}

fn run_prompt() {
    loop {
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer).expect("Could not read line");
        run(buffer);
    }
}

fn run(source: String) {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();

    for token in tokens {
        println!("{}", token);
    }
}
