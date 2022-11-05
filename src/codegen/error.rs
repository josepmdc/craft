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
    #[error("Return types in branch expression must be of the same type")]
    DifferentReturnTypesBranch(),
    #[error("Invalid type {0:#?}")]
    InvalidType(Type),
    #[error("Undefined struct {0}")]
    UndefinedStruct(String),
    #[error("Undefined struct field {0}")]
    UndefinedStructField(String),
    #[error("Not a struct or index is out of bounds")]
    BuildStructGepFailed(),
    #[error("Expected struct but found {0}")]
    ExpectedStruct(String),
    #[error("All fields of struct {0} must be initialized")]
    AllFieldsMustBeInitialized(String),
}
