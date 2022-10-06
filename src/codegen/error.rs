use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("Undeclared variable {0} or out of scope")]
    UndeclaredVariableOrOutOfScope(String),
    #[error("Undefined binary operator")]
    UndefinedBinaryOperator(),
    #[error("Invalid generated function.")]
    InvalidGeneratedFunction(),
    #[error("Invalid call to function {0}")]
    InvalidCall(String),
    #[error("Unkown function {0}")]
    UnkownFunction(String),
    #[error("{0} must be declared inside a function")]
    OutsideOfFuncion(String),
}
