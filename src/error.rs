use thiserror::Error;

use crate::{parser::error::ParseError, codegen::error::CodegenError};

pub fn report(line: i32, col: i32, message: String) {
    println!("[line {}, col {}] Error: {}", line, col, message);
}

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Parsing Error: {0}")]
    ParseError(#[from] ParseError),
    #[error("Compiling Error: {0}")]
    CodegenError(#[from] CodegenError),
}