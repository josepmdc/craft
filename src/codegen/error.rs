use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("Undeclared variable {0} or out of scope")]
    UndeclaredVariableOrOutOfScope(String),
    #[error("Undefined binary operator")]
    UndefinedBinaryOperator(),
    #[error("Invalid generated function.")]
    InvalidGeneratedFunction(),
}

