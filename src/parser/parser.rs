use crate::{
    error,
    lex::{Token, TokenType},
};

use super::error::ParseError;

type ParseResult<T> = Result<T, ParseError>;

pub const ANONYMOUS_FUNCTION_NAME: &str = "anonymous";

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Boolean(bool),
    Number(f64),
    String(String),
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Literal { value: LiteralValue },
    Grouping { expression: Box<Expr> },
    Variable(String),
    FnCall { fn_name: String, args: Vec<Expr> },
}

#[derive(Debug)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Debug)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Option<Vec<Expr>>,
    pub is_anon: bool,
}

pub struct Parser {
    tokens: Vec<Token>,
    current_index: usize,
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
        Self {
            tokens,
            current_index: 0,
        }
    }

    pub fn parse(&mut self) -> ParseResult<Function> {
        match self.current().type_ {
            TokenType::Fn => self.parse_fn(),
            _ => self.parse_toplevel_expr(),
        }
    }

    fn parse_fn(&mut self) -> ParseResult<Function> {
        self.advance()?;

        let prototype = self.parse_prototype()?;

        match self.current().type_ {
            TokenType::LeftBrace => self.advance()?,
            _ => return Err(ParseError::MissingLeftBrace()),
        };

        if self.current().type_ == TokenType::RightBrace {
            return Ok(Function {
                prototype,
                body: None,
                is_anon: false,
            });
        }

        let mut body = vec![];
        loop {
            body.push(self.parse_expr()?);

            if self.is_at_end() {
                return Err(ParseError::UnexpectedEndOfSource());
            }

            if self.current().type_ == TokenType::RightBrace {
                self.advance()?;
                break;
            }
        }

        Ok(Function {
            prototype,
            body: Some(body),
            is_anon: false,
        })
    }

    fn parse_prototype(&mut self) -> ParseResult<Prototype> {
        let name = match &self.current().type_ {
            TokenType::Identifier(identifier) => identifier.clone(),
            _ => return Err(ParseError::PrototypeMissingIdentifier()),
        };

        self.advance()?;

        match self.current().type_ {
            TokenType::LeftParen => self.advance()?,
            _ => return Err(ParseError::MissingLeftParen()),
        }

        if self.current().type_ == TokenType::RightParen {
            self.advance()?;
            return Ok(Prototype { name, args: vec![] });
        }

        let args = self.parse_prototype_args()?;

        Ok(Prototype { name, args })
    }

    fn parse_prototype_args(&mut self) -> ParseResult<Vec<String>> {
        let mut args = vec![];

        loop {
            match &self.current().type_ {
                TokenType::Identifier(id) => {
                    args.push(id.clone());
                    self.advance()?;
                }
                _ => return Err(ParseError::PrototypeMissingRightParenOrComma()),
            }

            match self.current().type_ {
                TokenType::RightParen => {
                    self.advance()?;
                    break;
                }
                TokenType::Comma => {
                    self.advance()?;
                }
                _ => {
                    return Err(ParseError::PrototypeMissingRightParenOrComma());
                }
            };
        }

        Ok(args)
    }

    fn parse_toplevel_expr(&mut self) -> ParseResult<Function> {
        match self.parse_expr() {
            Ok(expr) => Ok(Function {
                prototype: Prototype {
                    name: ANONYMOUS_FUNCTION_NAME.to_string(),
                    args: vec![],
                },
                body: Some(vec![expr]),
                is_anon: true,
            }),

            Err(err) => Err(err),
        }
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        let expr = self.parse_equality();
        match self.current().type_ {
            TokenType::Semicolon => {
                self.advance()?;
                expr
            }
            _ => Err(ParseError::MissingSemicolon()),
        }
    }

    fn parse_equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_comparison()?;
        while self.match_any([TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_comparison()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_term()?;

        while self.match_any([
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
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
        Ok(expr)
    }

    fn parse_term(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_factor()?;
        while self.match_any([TokenType::Minus, TokenType::Plus]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_term()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_unary()?;
        while self.match_any([TokenType::Slash, TokenType::Star]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_unary()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParseResult<Expr> {
        if self.match_any([TokenType::Bang, TokenType::Minus]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_primary()?;
            return Ok(Expr::Unary(UnaryExpr {
                operator,
                right: Box::new(right),
            }));
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        let expr = match self.current().to_owned().type_ {
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
            TokenType::LeftParen => self.parse_grouping()?,
            TokenType::Identifier(id) => {
                self.advance()?;
                match self.current().type_ {
                    TokenType::LeftParen => self.parse_fn_call(id)?,
                    _ => Expr::Variable(id.to_string()),
                }
            }
            _ => {
                let token = self.current();
                return Err(
                    self.report_error(token, ParseError::UnexpectedToken(token.lexeme.clone()))
                );
            }
        };
        self.advance()?;
        Ok(expr)
    }

    fn parse_grouping(&mut self) -> ParseResult<Expr> {
        self.advance()?;
        let expr = self.parse_expr()?;
        self.assert_current_type(TokenType::RightParen, ParseError::MissingRightParen())?;
        Ok(Expr::Grouping {
            expression: Box::new(expr),
        })
    }

    fn parse_fn_call(&mut self, name: String) -> ParseResult<Expr> {
        self.advance()?; // Skip opening '('

        if let TokenType::RightParen = self.current().type_ {
            return Ok(Expr::FnCall {
                fn_name: name,
                args: vec![],
            });
        }

        let mut args = vec![];

        loop {
            args.push(self.parse_expr()?);

            match self.current().type_ {
                TokenType::Comma => self.advance()?,
                TokenType::RightParen => {
                    self.advance()?;
                    break;
                }
                _ => return Err(ParseError::MissingCommaOrRightParen()),
            }
        }

        Ok(Expr::FnCall {
            fn_name: name,
            args,
        })
    }

    fn match_any<const L: usize>(&self, types: [TokenType; L]) -> bool {
        types.iter().any(|t| self.check_current_type(t.clone()))
    }

    fn check_current_type(&self, type_: TokenType) -> bool {
        !self.is_at_end() && type_ == self.current().type_
    }

    fn assert_current_type(&mut self, type_: TokenType, error: ParseError) -> ParseResult<()> {
        match self.check_current_type(type_) {
            true => Ok(()),
            false => Err(self.report_error(self.current(), error)),
        }
    }

    fn advance(&mut self) -> ParseResult<()> {
        if self.is_at_end() {
            return Err(ParseError::UnexpectedEndOfSource());
        }
        self.current_index += 1;
        Ok(())
    }

    fn previous(&self) -> &Token {
        self.tokens.get(self.current_index - 1).unwrap()
    }

    fn current(&self) -> &Token {
        self.tokens.get(self.current_index).unwrap()
    }

    fn is_at_end(&self) -> bool {
        self.current().type_ == TokenType::EOF
    }

    fn report_error(&self, token: &Token, error: ParseError) -> ParseError {
        error::report(token.loc.line, token.loc.col, error.to_string());
        error
    }

    // TODO Use sync when there's an error
    fn sync(&mut self) -> ParseResult<()> {
        self.advance()?;

        while !self.is_at_end() {
            if self.previous().type_ == TokenType::Semicolon {
                return Ok(());
            }

            match self.current().type_ {
                TokenType::Fn => return Ok(()),
                _ => self.advance()?,
            };
        }
        Ok(())
    }
}
