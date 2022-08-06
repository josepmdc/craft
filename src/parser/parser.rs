use crate::{
    error,
    lex::{Token, TokenType},
};

use super::error::ParseError;

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Boolean(bool),
    Null,
    Number(f64),
    String(String),
}

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Literal {
        value: LiteralValue,
    },
    Grouping {
        expression: Box<Expr>,
    },
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    /*
        expression     → equality ;
        equality       → comparison ( ( "!=" | "==" ) comparison )* ;
        comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
        term           → factor ( ( "-" | "+" ) factor )* ;
        factor         → unary ( ( "/" | "*" ) unary )* ;
        unary          → ( "!" | "-" ) unary
                       | primary ;
        primary        → NUMBER | STRING | "true" | "false" | "nil"
                       | "(" expression ")" ;
    */

    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> ParseResult<Expr> {
        self.expression()
    }

    fn expression(&mut self) -> ParseResult<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.comparison()?;
        while self.match_any(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.process_next_token().clone();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.term()?;

        while self.match_any(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.process_next_token().clone();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn term(&mut self) -> ParseResult<Expr> {
        let mut expr = self.factor()?;
        while self.match_any(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.process_next_token().clone();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn factor(&mut self) -> ParseResult<Expr> {
        let mut expr = self.unary()?;
        while self.match_any(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.process_next_token().clone();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult<Expr> {
        if self.match_any(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.process_next_token().clone();
            let right = self.primary()?;
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }
        self.primary()
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        let expr = match &self.peek().type_ {
            TokenType::False => Expr::Literal {
                value: LiteralValue::Boolean(false),
            },
            TokenType::True => Expr::Literal {
                value: LiteralValue::Boolean(true),
            },
            TokenType::String { literal } => Expr::Literal {
                value: LiteralValue::String(literal.clone()),
            },
            TokenType::Number { literal } => Expr::Literal {
                value: LiteralValue::Number(literal.clone()),
            },
            TokenType::LeftParen => {
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, ParseError::MissingRightParen())?;
                Expr::Grouping {
                    expression: Box::new(expr),
                }
            }
            _ => {
                let token = self.peek();
                return Err(
                    self.report_error(token, ParseError::UnexpectedToken(token.value.clone()))
                );
            }
        };
        self.process_next_token();
        Ok(expr)
    }

    fn match_any(&self, types: &[TokenType]) -> bool {
        types.iter().any(|t| self.check_type(t))
    }

    fn check_type(&self, type_: &TokenType) -> bool {
        !self.is_at_end() && *type_ == self.peek().type_
    }

    fn process_next_token(&mut self) -> &Token {
        self.current += 1;
        self.get_previous_token()
    }

    fn get_previous_token(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn is_at_end(&self) -> bool {
        self.peek().type_ == TokenType::EOF
    }

    fn consume(&mut self, type_: TokenType, error: ParseError) -> ParseResult<&Token> {
        match self.check_type(&type_) {
            true => Ok(self.process_next_token()),
            false => Err(self.report_error(self.peek(), error)),
        }
    }

    fn report_error(&self, token: &Token, error: ParseError) -> ParseError {
        error::report(token.loc.line, token.loc.col, error.to_string());
        error
    }

    fn sync(&mut self) {
        self.process_next_token();

        while !self.is_at_end() {
            if self.get_previous_token().type_ == TokenType::Semicolon {
                return;
            }

            match self.peek().type_ {
                TokenType::Fn => return,
                _ => self.process_next_token(),
            };
        }
    }
}
