use crate::{
    lexer::token::{Token, TokenKind},
    parser::error::{self, ParseError},
};

use super::{
    stmt::Stmt,
    structs::{FieldAccess, StructExpr},
    LiteralType, ParseResult, Parser, Type,
};

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub body: Vec<Stmt>,
    pub return_expr: Option<Box<Expr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Literal(LiteralType),
    Variable(String),
    VariableAssignment {
        id: String,
        rhs: Box<Expr>,
    },
    FnCall(FnCall),
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
    },
    Block(Block),
    Struct(StructExpr),
    FieldAccess(FieldAccess),
    Array(Type, Vec<Expr>),
    ArrayAccess(ArrayAccess),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayAccess {
    pub variable_id: String,
    pub index: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub fn_name: String,
    pub args: Vec<Expr>,
}

impl Parser {
    pub fn parse_toplevel_expr(&mut self) -> ParseResult<Stmt> {
        match self.parse_expr() {
            Ok(expr) => Ok(Stmt::Expr(expr)),
            Err(err) => Err(err),
        }
    }

    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        trace!("Parsing expr. Starting at {:#?}", self.current());
        let expr = self.parse_logical();
        trace!("Parsed expr: {:#?}", expr);
        expr
    }

    fn parse_logical(&mut self) -> ParseResult<Expr> {
        trace!("Parsing logical");
        let mut expr = self.parse_equality()?;
        while self.match_any([TokenKind::And, TokenKind::Or]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_equality()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        trace!("Parsed logical: {:#?}", expr);
        Ok(expr)
    }

    fn parse_equality(&mut self) -> ParseResult<Expr> {
        trace!("Parsing equality");
        let mut expr = self.parse_comparison()?;
        while self.match_any([TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_comparison()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        trace!("Parsed equality: {:#?}", expr);
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> ParseResult<Expr> {
        trace!("Parsing comparison");
        let mut expr = self.parse_term()?;

        while self.match_any([
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
        ]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_term()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        trace!("Parsed comparison: {:#?}", expr);
        Ok(expr)
    }

    fn parse_term(&mut self) -> ParseResult<Expr> {
        trace!("Parsing term");
        let mut expr = self.parse_factor()?;
        while self.match_any([TokenKind::Minus, TokenKind::Plus]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_factor()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        trace!("Parsed term: {:#?}", expr);
        Ok(expr)
    }

    fn parse_factor(&mut self) -> ParseResult<Expr> {
        trace!("Parsing factor");
        let mut expr = self.parse_unary()?;
        while self.match_any([TokenKind::Slash, TokenKind::Star]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_unary()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        trace!("Parsed factor: {:#?}", expr);
        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParseResult<Expr> {
        trace!("Parsing unary");
        if self.match_any([TokenKind::Bang, TokenKind::Minus]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_primary()?;
            let expr = Expr::Unary(UnaryExpr {
                operator,
                right: Box::new(right),
            });
            trace!("Parsed unary: {:#?}", expr);
            return Ok(expr);
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        trace!("Parsing primary");
        let expr = match self.current().to_owned().kind {
            TokenKind::False => {
                self.advance()?;
                Expr::Literal(LiteralType::Boolean(false))
            }
            TokenKind::True => {
                self.advance()?;
                Expr::Literal(LiteralType::Boolean(true))
            }
            TokenKind::String(literal) => {
                self.advance()?;
                Expr::Literal(LiteralType::String(literal))
            }
            TokenKind::F64(literal) => {
                self.advance()?;
                Expr::Literal(LiteralType::F64(literal))
            }
            TokenKind::I64(literal) => {
                self.advance()?;
                Expr::Literal(LiteralType::I64(literal))
            }
            TokenKind::LeftParen => self.parse_grouping()?,
            TokenKind::LeftBrace => self.parse_block_expr()?,
            TokenKind::LeftBracket => self.parse_array_declaration()?,
            TokenKind::Identifier(_) => self.parse_id_expr()?,
            TokenKind::If => self.parse_if()?,
            _ => {
                let token = self.current();
                return Err(error::report(
                    token,
                    ParseError::UnexpectedToken(token.lexeme.clone()),
                ));
            }
        };

        trace!("Parsed primary: {:#?}", expr);
        Ok(expr)
    }

    fn parse_id_expr(&mut self) -> ParseResult<Expr> {
        trace!("Parsing identifier expr");
        match self.peek().kind {
            TokenKind::LeftParen => Ok(Expr::FnCall(self.parse_fn_call()?)),
            TokenKind::Equal => self.parse_var_assignment(),
            TokenKind::Bang => self.parse_struct_expr(),
            TokenKind::Dot => self.parse_field_access(),
            TokenKind::LeftBracket => self.parse_array_access(),
            _ => {
                let name = self.current().lexeme.clone();
                self.advance()?;
                Ok(Expr::Variable(name))
            }
        }
    }

    pub fn parse_var_assignment(&mut self) -> ParseResult<Expr> {
        let id = self.current().lexeme.clone();
        self.advance()?; // skip identifier

        self.consume(TokenKind::Equal, ParseError::MissingEquals())?;

        let rhs = Box::new(self.parse_expr()?);

        Ok(Expr::VariableAssignment { id, rhs })
    }

    pub fn parse_if(&mut self) -> ParseResult<Expr> {
        trace!("Parsing if");
        self.advance()?; // skip 'if' token

        let cond = self.parse_expr()?;
        let then = self.parse_block_expr()?;

        self.consume(TokenKind::Else, ParseError::MissingElse())?;

        let else_ = match self.current().kind {
            TokenKind::LeftBrace => self.parse_block_expr()?,
            TokenKind::If => self.parse_if()?,
            _ => {
                return Err(ParseError::UnexpectedTokenVerbose {
                    expected: "{' or 'if".to_string(),
                    found: self.current().lexeme.clone(),
                })
            }
        };

        let conditional = Expr::If {
            cond: Box::new(cond),
            then: Box::new(then),
            else_: Box::new(else_),
        };
        trace!("Parsed if expr {:#?}", conditional);
        Ok(conditional)
    }

    fn parse_grouping(&mut self) -> ParseResult<Expr> {
        trace!("Parsing grouping");
        self.advance()?;
        let expr = self.parse_expr()?;
        self.consume(TokenKind::RightParen, ParseError::MissingRightParen())?;
        trace!("Parsed grouping: {:#?}", expr);
        Ok(expr)
    }

    pub fn parse_fn_call(&mut self) -> ParseResult<FnCall> {
        trace!("Parsing fn call");
        let name = self.current().lexeme.clone();
        self.advance()?; // Skip identifier

        self.consume(TokenKind::LeftParen, ParseError::MissingLeftParen())?;

        if let TokenKind::RightParen = self.current().kind {
            self.advance()?; // Skip closing ')'
            return Ok(FnCall {
                fn_name: name,
                args: vec![],
            });
        }

        let mut args = vec![];

        loop {
            args.push(self.parse_expr()?);

            match self.current().kind {
                TokenKind::Comma => self.advance()?,
                TokenKind::RightParen => {
                    self.advance()?;
                    break;
                }
                _ => return Err(ParseError::MissingCommaOrRightParen()),
            };
        }

        let expr = FnCall {
            fn_name: name,
            args,
        };

        trace!("Parsed fn call: {:#?}", expr);

        Ok(expr)
    }

    pub fn parse_block_expr(&mut self) -> ParseResult<Expr> {
        Ok(Expr::Block(self.parse_block()?))
    }

    pub fn consume_string(&mut self) -> ParseResult<String> {
        match self.current().kind.clone() {
            TokenKind::String(str) => {
                self.advance()?;
                Ok(str)
            }
            _ => Err(ParseError::ExpectedString()),
        }
    }

    fn parse_array_declaration(&mut self) -> ParseResult<Expr> {
        trace!("Parsing array decalration");
        self.consume(TokenKind::LeftBracket, ParseError::ExpectedLeftBracket())?;

        let mut items = vec![];

        loop {
            items.push(self.parse_expr()?);

            match self.current().kind {
                TokenKind::Comma => self.advance()?,
                TokenKind::RightBracket => {
                    self.advance()?;
                    break;
                }
                _ => return Err(ParseError::ExpectedRightBracket()),
            };
        }

        let id = self.consume_identifier()?;
        let type_ = self.parse_type(id);

        let arr = Expr::Array(type_, items);

        trace!("Parsed arr decalration: {:#?}", arr);

        Ok(arr)
    }

    fn parse_array_access(&mut self) -> ParseResult<Expr> {
        trace!("Parsing array access");
        let variable_id = self.consume_identifier()?;

        self.consume(TokenKind::LeftBracket, ParseError::ExpectedLeftBracket())?;

        let index = Box::new(self.parse_expr()?);

        self.consume(TokenKind::RightBracket, ParseError::ExpectedRightBracket())?;

        let access = Expr::ArrayAccess(ArrayAccess { variable_id, index });

        trace!("Parsed array access: {:#?}", access);

        Ok(access)
    }
}
