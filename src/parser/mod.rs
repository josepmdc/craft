pub mod error;
pub mod expr;
pub mod func;
pub mod stmt;

use crate::lex::{Token, TokenKind};

use self::{error::ParseError, stmt::Stmt};

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Boolean(bool),
    Number(f64),
    String(String),
}

pub struct Parser {
    tokens: Vec<Token>,
    current_index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current_index: 0,
        }
    }

    pub fn parse(&mut self) -> ParseResult<Vec<Stmt>> {
        let mut statments: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            statments.push(match self.current().kind {
                TokenKind::Fn => self.parse_fn()?,
                _ => self.parse_toplevel_expr()?,
            });
        }
        Ok(statments)
    }

    pub fn match_any<const L: usize>(&self, types: [TokenKind; L]) -> bool {
        types.iter().any(|t| self.check_current_type(t.clone()))
    }

    fn check_current_type(&self, kind: TokenKind) -> bool {
        !self.is_at_end() && kind == self.current().kind
    }

    pub fn consume(&mut self, kind: TokenKind, error: ParseError) -> ParseResult<&Token> {
        match self.check_current_type(kind) {
            true => {
                self.advance()?;
                Ok(self.previous())
            }
            false => Err(error::report(self.current(), error)),
        }
    }

    pub fn advance(&mut self) -> ParseResult<()> {
        if self.is_at_end() {
            return Err(ParseError::UnexpectedEndOfSource());
        }

        trace!(
            "Advanced from \"{}\" to \"{}\"",
            self.current().lexeme,
            self.tokens[self.current_index + 1].lexeme,
        );

        self.current_index += 1;
        Ok(())
    }

    pub fn current(&self) -> &Token {
        self.tokens.get(self.current_index).unwrap()
    }

    pub fn previous(&self) -> &Token {
        self.tokens.get(self.current_index - 1).unwrap()
    }

    pub fn peek(&self) -> &Token {
        self.tokens.get(self.current_index + 1).unwrap()
    }

    pub fn is_at_end(&self) -> bool {
        self.current().kind == TokenKind::EOF
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lex::{Location, Scanner, Token, TokenKind},
        parser::{
            expr::{BinaryExpr, Expr},
            func::{Function, Prototype},
            LiteralValue,
        },
        PROGRAM_STARTING_POINT,
    };

    use super::Parser;

    #[test]
    fn parse_arithmetic() {
        let src = "2 + 2 * 3 / 2".to_string();
        let mut scanner = Scanner::new(src.clone());
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens.to_vec());
        let actual_ast = parser.parse().unwrap();
        trace!("AST for {}: \n{:#?}", src, actual_ast);

        let expected_ast = Function {
            prototype: Prototype {
                name: PROGRAM_STARTING_POINT.to_string(),
                args: vec![],
            },
            body: vec![Expr::Binary(BinaryExpr {
                left: Box::new(Expr::Literal {
                    value: LiteralValue::Number(2.0),
                }),
                operator: Token {
                    kind: TokenKind::Plus,
                    lexeme: "+".to_string(),
                    loc: Location { col: 3, line: 1 },
                },
                right: Box::new(Expr::Binary(BinaryExpr {
                    left: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::Literal {
                            value: LiteralValue::Number(2.0),
                        }),
                        operator: Token {
                            kind: TokenKind::Star,
                            lexeme: "*".to_string(),
                            loc: Location { col: 7, line: 1 },
                        },
                        right: Box::new(Expr::Literal {
                            value: LiteralValue::Number(3.0),
                        }),
                    })),
                    operator: Token {
                        kind: TokenKind::Slash,
                        lexeme: "/".to_string(),
                        loc: Location { col: 11, line: 1 },
                    },
                    right: Box::new(Expr::Literal {
                        value: LiteralValue::Number(2.0),
                    }),
                })),
            })],
            is_anon: true,
        };

        let func = match actual_ast[0].clone() {
            crate::parser::stmt::Stmt::Function(func) => func,
            _ => panic!("Expected a function"),
        };

        assert_eq!(expected_ast.body, func.body);
    }

    #[test]
    fn parse_parenthesized_arithmetic() {
        let src = "(2 + 2) * 3 / 2".to_string();
        let mut scanner = Scanner::new(src.clone());
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens.to_vec());
        let actual_ast = parser.parse().unwrap();
        trace!("AST for {}: \n{:#?}", src, actual_ast);

        let expected_ast = Function {
            prototype: Prototype {
                name: PROGRAM_STARTING_POINT.to_string(),
                args: vec![],
            },
            body: vec![Expr::Binary(BinaryExpr {
                left: Box::new(Expr::Binary(BinaryExpr {
                    left: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::Literal {
                            value: LiteralValue::Number(2.0),
                        }),
                        operator: Token {
                            kind: TokenKind::Plus,
                            lexeme: "+".to_string(),
                            loc: Location { col: 4, line: 1 },
                        },
                        right: Box::new(Expr::Literal {
                            value: LiteralValue::Number(2.0),
                        }),
                    })),
                    operator: Token {
                        kind: TokenKind::Star,
                        lexeme: "*".to_string(),
                        loc: Location { col: 9, line: 1 },
                    },
                    right: Box::new(Expr::Literal {
                        value: LiteralValue::Number(3.0),
                    }),
                })),
                operator: Token {
                    kind: TokenKind::Slash,
                    lexeme: "/".to_string(),
                    loc: Location { col: 13, line: 1 },
                },
                right: Box::new(Expr::Literal {
                    value: LiteralValue::Number(2.0),
                }),
            })],
            is_anon: true,
        };

        let func = match actual_ast[0].clone() {
            crate::parser::stmt::Stmt::Function(func) => func,
            _ => panic!("Expected a function"),
        };

        assert_eq!(expected_ast.body, func.body);
    }

    #[test]
    fn parse_function_definition() {
        let src = "fn main(a, b) { 2 + 2 }".to_string();
        let mut scanner = Scanner::new(src.clone());
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens.to_vec());
        let actual_ast = parser.parse().unwrap();
        trace!("AST for {}: \n{:#?}", src, actual_ast);

        let expected_ast = Function {
            prototype: Prototype {
                name: "main".to_string(),
                args: vec!["a".to_string(), "b".to_string()],
            },
            body: vec![Expr::Binary(BinaryExpr {
                left: Box::new(Expr::Literal {
                    value: LiteralValue::Number(2.0),
                }),
                operator: Token {
                    kind: TokenKind::Plus,
                    lexeme: "+".to_string(),
                    loc: Location { col: 19, line: 1 },
                },
                right: Box::new(Expr::Literal {
                    value: LiteralValue::Number(2.0),
                }),
            })],
            is_anon: false,
        };

        let func = match actual_ast[0].clone() {
            crate::parser::stmt::Stmt::Function(func) => func,
            _ => panic!("Expected a function"),
        };

        assert_eq!(expected_ast.body, func.body);
    }

    #[test]
    fn parse_function_call() {
        env_logger::init();
        let src = "main(a, b);".to_string();
        let mut scanner = Scanner::new(src.clone());
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens.to_vec());
        let actual_ast = parser.parse().unwrap();
        trace!("AST for {}: \n{:#?}", src, actual_ast);

        let expected_ast = Function {
            prototype: Prototype {
                name: PROGRAM_STARTING_POINT.to_string(),
                args: vec![],
            },
            body: vec![Expr::FnCall {
                fn_name: "main".to_string(),
                args: vec![
                    Expr::Variable(Token {
                        kind: TokenKind::Identifier("a".to_string()),
                        lexeme: "a".to_string(),
                        loc: Location { col: 6, line: 1 },
                    }),
                    Expr::Variable(Token {
                        kind: TokenKind::Identifier("b".to_string()),
                        lexeme: "b".to_string(),
                        loc: Location { col: 9, line: 1 },
                    }),
                ],
            }],
            is_anon: true,
        };

        let func = match actual_ast[0].clone() {
            crate::parser::stmt::Stmt::Function(func) => func,
            _ => panic!("Expected a function"),
        };

        assert_eq!(expected_ast.body, func.body);
    }
}
