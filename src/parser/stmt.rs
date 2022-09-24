use crate::lex::{Token, TokenKind};

use super::{
    expr::Expr,
    parser::{ParseResult, Parser},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Vec<Expr>,
    pub is_anon: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Function(Function),
    Var { name: Token, initializer: Expr },
    Expr(Expr),
}

impl Parser {
    fn parse_var_declaration(&mut self) -> ParseResult<Stmt> {
        self.advance()?;
        match &self.current().kind {
            TokenKind::Identifier(_) => Ok(Stmt::Var {
                name: self.current().clone(),
                initializer: self.parse_expr()?,
            }),
            _ => todo!("Return error"),
        }
    }

    fn parse_expr_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.parse_expr()?;

        Ok(Stmt::Expr(expr))
    }
}
