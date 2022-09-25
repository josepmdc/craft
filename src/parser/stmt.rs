use crate::lex::{Token, TokenKind};

use super::{error::ParseError, expr::Expr, func::Function, ParseResult, Parser};

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
        self.consume(TokenKind::Semicolon, ParseError::MissingSemicolon())?;
        Ok(Stmt::Expr(expr))
    }
}
