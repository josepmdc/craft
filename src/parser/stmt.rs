use crate::lexer::token::{Token, TokenKind};

use super::{
    error::ParseError,
    expr::{Block, Expr},
    structs::Struct,
    ParseResult, Parser, Type, Variable,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Function(Function),
    Struct(Struct),
    Var {
        token: Token,
        initializer: Expr,
        mutable: bool,
    },
    Expr(Expr),
    While {
        cond: Expr,
        body: Block,
    },
    Printf {
        fmt_string: String,
        args: Vec<Expr>,
    },
    Return(Expr),
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
            TokenKind::Printf => self.parse_printf()?,
            TokenKind::Return => self.parse_return()?,
            _ => self.parse_expr_stmt()?,
        };
        Ok(expr)
    }

    pub fn parse_fn(&mut self) -> ParseResult<Stmt> {
        trace!("Parsing fn");
        self.advance()?;

        let prototype = self.parse_prototype()?;
        let (body, return_expr) = match self.parse_block_expr()? {
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

        let mutable = if let TokenKind::Mut = self.current().kind {
            self.advance()?;
            true
        } else {
            false
        };

        let stmt = match &self.current().kind {
            TokenKind::Identifier(_) => {
                let identifier = self.advance()?.clone();
                self.consume(
                    TokenKind::Equal,
                    ParseError::ExpectedEquals(identifier.lexeme.clone()),
                )?;
                Stmt::Var {
                    token: identifier,
                    initializer: self.parse_expr()?,
                    mutable,
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
        let name = self.consume_identifier()?;

        let params = self.parse_prototype_params()?;

        let return_type = match self.current().kind.clone() {
            TokenKind::Identifier(id) => {
                self.advance()?;
                self.parse_type(id)
            }
            TokenKind::LeftBracket => self.parse_array_type()?,
            TokenKind::LeftBrace => Type::Void,
            _ => return Err(ParseError::UnexpectedToken(self.current().lexeme.clone())),
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
                    param.identifier = id.clone();
                    self.advance()?;
                }
                _ => return Err(ParseError::ExpectedIdentifier()),
            }

            self.consume(
                TokenKind::Colon,
                ParseError::ExpectedColon(param.identifier.clone()),
            )?;

            match &self.current().kind {
                TokenKind::LeftBracket => {
                    param.type_ = self.parse_array_type()?;
                }
                TokenKind::Identifier(id) => {
                    param.type_ = self.parse_type(id.clone());
                    self.advance()?;
                }
                _ => return Err(ParseError::ExpectedIdentifier()),
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

    pub fn parse_block(&mut self) -> ParseResult<Block> {
        trace!("Parsing block");
        self.consume(TokenKind::LeftBrace, ParseError::MissingLeftBrace())?;

        let mut body = vec![];

        while !self.current_is(TokenKind::RightBrace) {
            let stmt = self.parse_stmt()?;

            match self.current().kind {
                TokenKind::Semicolon => {
                    self.advance()?; // skip ;
                    body.push(stmt)
                }
                TokenKind::RightBrace => {
                    // if the last line of the block is an expression, it'll be the return
                    let expr = match stmt {
                        Stmt::Expr(expr) => expr,
                        _ => return Err(ParseError::MissingSemicolon()),
                    };

                    let block = Block {
                        body,
                        return_expr: Some(Box::new(expr)),
                    };

                    trace!("Parsed block: {:#?}", block);

                    self.advance()?; // Skip '}'

                    return Ok(block);
                }
                _ => match &stmt {
                    // if and while statments don't need semicolons at the end
                    Stmt::While { .. } | Stmt::Expr(Expr::If { .. }) => body.push(stmt),
                    _ => return Err(ParseError::MissingSemicolon()),
                },
            };

            if self.is_at_end() {
                return Err(ParseError::UnexpectedEndOfSource());
            }
        }

        self.advance()?; // Skip '}'

        let block = Block {
            body,
            return_expr: None,
        };

        trace!("Parsed block: {:#?}", block);

        Ok(block)
    }

    fn parse_printf(&mut self) -> ParseResult<Stmt> {
        trace!("Parsing printf");
        self.consume(
            TokenKind::Printf,
            ParseError::UnexpectedToken(self.current().lexeme.clone()),
        )?;

        self.consume(TokenKind::LeftParen, ParseError::MissingLeftParen())?;

        let fmt_string = self.consume_string()?;

        let mut args = vec![];
        match self.current().kind {
            TokenKind::RightParen => (),
            TokenKind::Comma => {
                self.advance()?;
                while self.current().kind != TokenKind::RightParen {
                    args.push(self.parse_expr()?);

                    match self.current().kind {
                        TokenKind::Comma => {
                            self.advance()?;
                        }
                        TokenKind::RightParen => (),
                        _ => return Err(ParseError::MissingCommaOrRightParen()),
                    };
                }
            }
            _ => return Err(ParseError::MissingCommaOrRightParen()),
        };

        self.advance()?;

        let printf = Stmt::Printf { fmt_string, args };

        trace!("Parsed printf: {:#?}", printf);

        Ok(printf)
    }

    fn parse_return(&mut self) -> ParseResult<Stmt> {
        trace!("Parsing return stmt");
        self.consume(TokenKind::Return, ParseError::ExpectedReturn())?;
        let ret = Stmt::Return(self.parse_expr()?);
        trace!("Parsed return: {:#?}", ret);
        Ok(ret)
    }
}
