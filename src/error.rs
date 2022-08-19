use thiserror::Error;

use crate::parser::error::ParseError;

pub fn report(line: i32, col: i32, message: String) {
    println!("[line {}, col {}] Error: {}", line, col, message);
}

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Parsing Error: {0}")]
    ParseError(#[from] ParseError)
}
