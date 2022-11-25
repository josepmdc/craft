use thiserror::Error;

use crate::{codegen::error::CodegenError, parser::error::ParseError};

pub fn report(line: i32, col: i32, message: String) {
    println!("[line {}, col {}] Error: {}", line, col, message);
}

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Parse Error: {0}")]
    ParseError(#[from] ParseError),
    #[error("Compile Error: {0}")]
    CodegenError(#[from] CodegenError),
}
