use crate::lexer::token::{Token, TokenKind};

use super::{error::ParseError, expr::Expr, ParseResult, Parser, Type, Variable};

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Function(Function),
    Var { token: Token, initializer: Expr },
    Expr(Expr),
    While { cond: Expr, body: Expr },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Prototype {
    pub name: String,
    pub params: Vec<Variable>,
    pub return_type: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Vec<Stmt>,
    pub return_expr: Option<Expr>,
    pub is_builtin: bool,
}

impl Parser {
    pub fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = match self.current().kind {
            TokenKind::VarDeclaration => self.parse_var_declaration()?,
            TokenKind::While => self.parse_while()?,
            _ => self.parse_expr_stmt()?,
        };
        Ok(expr)
    }

    pub fn parse_fn(&mut self) -> ParseResult<Stmt> {
        trace!("Parsing fn");
        self.advance()?;

        let prototype = self.parse_prototype()?;
        let (body, return_expr) = match self.parse_block()? {
            Expr::Block(block) => (block.body, block.return_expr),
            _ => panic!("parse_block should always return a block!"),
        };

        let func = Function {
            prototype,
            body,
            return_expr: return_expr.map(|x| *x),
            is_builtin: false,
        };

        trace!("Parsed fn: {:#?}", func);

        Ok(Stmt::Function(func))
    }

    fn parse_var_declaration(&mut self) -> ParseResult<Stmt> {
        self.advance()?;
        let stmt = match &self.current().kind {
            TokenKind::Identifier(_) => {
                let identifier = self.advance()?.clone();
                self.consume(TokenKind::Equal, ParseError::MissingLeftParen())?;
                Stmt::Var {
                    token: identifier,
                    initializer: self.parse_expr()?,
                }
            }
            _ => todo!("Return error"),
        };
        Ok(stmt)
    }

    fn parse_expr_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.parse_expr()?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_prototype(&mut self) -> ParseResult<Prototype> {
        trace!("Parsing prototype");
        let name = match &self.current().kind {
            TokenKind::Identifier(identifier) => identifier.clone(),
            _ => return Err(ParseError::PrototypeMissingIdentifier()),
        };

        self.advance()?;

        let params = self.parse_prototype_params()?;

        let return_type = match self.current().kind.clone() {
            TokenKind::Identifier(id) => {
                self.advance()?;
                self.parse_type(id)
            }
            _ => Type::Void,
        };

        let proto = Prototype {
            name,
            params,
            return_type,
        };

        trace!("Parsed prototype: {:#?}", proto);

        Ok(proto)
    }

    fn parse_prototype_params(&mut self) -> ParseResult<Vec<Variable>> {
        let mut params = vec![];

        self.consume(TokenKind::LeftParen, ParseError::MissingLeftParen())?;

        if self.current().kind == TokenKind::RightParen {
            self.advance()?;
            return Ok(vec![]);
        }

        loop {
            let mut param = Variable::default();
            match &self.current().kind {
                TokenKind::Identifier(id) => {
                    param.name = id.clone();
                    self.advance()?;
                }
                _ => return Err(ParseError::PrototypeMissingIdentifier()),
            }

            self.consume(
                TokenKind::Colon,
                ParseError::MissingColon(param.name.clone()),
            )?;

            match &self.current().kind {
                TokenKind::Identifier(id) => {
                    param.type_ = self.parse_type(id.clone());
                    self.advance()?;
                }
                _ => return Err(ParseError::PrototypeMissingIdentifier()),
            }

            params.push(param);

            match self.current().kind {
                TokenKind::RightParen => {
                    self.advance()?;
                    break;
                }
                TokenKind::Comma => self.advance()?,
                _ => return Err(ParseError::PrototypeMissingRightParenOrComma()),
            };
        }

        trace!("Parsed args: {:#?}", params);

        Ok(params)
    }

    fn parse_while(&mut self) -> ParseResult<Stmt> {
        self.advance()?; // skip 'wile'
        let cond = self.parse_expr()?;
        let body = self.parse_block()?;
        Ok(Stmt::While { cond, body })
    }
}
