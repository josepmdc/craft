use thiserror::Error;

use crate::parser::Type;

#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("Undeclared variable {0} or out of scope")]
    UndeclaredVariableOrOutOfScope(String),
    #[error("Undefined binary operator {0}")]
    UndefinedBinaryOperator(String),
    #[error("Invalid generated function.")]
    InvalidGeneratedFunction(),
    #[error("Invalid call to function {0}")]
    InvalidCall(String),
    #[error("Unkown function {0}")]
    UnkownFunction(String),
    #[error("{0} must be declared inside a function")]
    OutsideOfFuncion(String),
    #[error("A return expression was expected")]
    ExpectedReturnExpr(),
    #[error("Operands in binary operation must be of the same type")]
    DifferentTypesBinOp(),
    #[error("Invalid type {0:#?}")]
    InvalidType(Type),
    #[error("Undefined struct {0}")]
    UndefinedStruct(String),
}
