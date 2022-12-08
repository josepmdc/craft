pub mod error;
pub mod expr;
pub mod stmt;
pub mod structs;

use crate::lexer::token::{Token, TokenKind};

use self::{error::ParseError, stmt::Stmt};

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralType {
    Boolean(bool),
    F64(f64),
    I64(i64),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I64,
    F64,
    String,
    Struct(String),
    Void,
    Array(ArrayType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayType {
    pub type_: Box<Type>,
    pub size: u32,
}

impl Default for Type {
    fn default() -> Self {
        Self::I64
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Variable {
    pub identifier: String,
    pub type_: Type,
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
        let mut statments = vec![];
        while !self.is_at_end() {
            statments.push(self.parse_declaration()?);
        }
        Ok(statments)
    }

    fn parse_declaration(&mut self) -> ParseResult<Stmt> {
        trace!("Parsing declaration. Current: {:#?}", self.current());
        let stmt = match self.current().kind {
            TokenKind::Fn => self.parse_fn()?,
            TokenKind::Struct => self.parse_struct()?,
            _ => self.parse_toplevel_expr()?,
        };
        Ok(stmt)
    }

    fn parse_type(&self, id: String) -> Type {
        match id.as_str() {
            "i64" => Type::I64,
            "f64" => Type::F64,
            "string" => Type::String,
            _ => Type::Struct(id),
        }
    }

    fn parse_array_type(&mut self) -> ParseResult<Type> {
        self.consume(TokenKind::LeftBracket, ParseError::ExpectedLeftBracket())?;

        let id = self.consume_identifier()?;
        self.consume(TokenKind::Semicolon, ParseError::MissingSemicolon())?;

        let size = self.consume_i64()? as u32; // TODO should check for u32 instead of casting here but only i64 is supported ATM

        let array = ArrayType {
            type_: Box::new(self.parse_type(id)),
            size,
        };

        self.consume(TokenKind::RightBracket, ParseError::ExpectedRightBracket())?;

        Ok(Type::Array(array))
    }

    fn match_any<const L: usize>(&self, types: [TokenKind; L]) -> bool {
        types.iter().any(|t| self.current_is(t.clone()))
    }

    fn current_is(&self, kind: TokenKind) -> bool {
        !self.is_at_end() && kind == self.current().kind
    }

    fn consume(&mut self, kind: TokenKind, error: ParseError) -> ParseResult<&Token> {
        trace!("Consuming {:#?}", kind);
        match self.current_is(kind) {
            true => {
                self.advance()?;
                Ok(self.previous())
            }
            false => Err(error::report(self.current(), error)),
        }
    }

    fn consume_identifier(&mut self) -> ParseResult<String> {
        match self.current().kind.clone() {
            TokenKind::Identifier(id) => {
                self.advance()?;
                Ok(id)
            }
            _ => Err(ParseError::ExpectedIdentifier()),
        }
    }

    fn consume_i64(&mut self) -> ParseResult<i64> {
        match self.current().kind {
            TokenKind::I64(int) => {
                self.advance()?;
                Ok(int)
            }
            _ => Err(ParseError::ExpectedInteger()),
        }
    }

    fn advance(&mut self) -> ParseResult<&Token> {
        if self.is_at_end() {
            return Err(ParseError::UnexpectedEndOfSource());
        }

        trace!(
            "[PARSER] Advanced from \"{}\" to \"{}\"",
            self.current().lexeme,
            self.tokens[self.current_index + 1].lexeme,
        );

        self.current_index += 1;
        Ok(self.previous())
    }

    fn current(&self) -> &Token {
        self.tokens.get(self.current_index).unwrap()
    }

    fn previous(&self) -> &Token {
        self.tokens.get(self.current_index - 1).unwrap()
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.current_index + 1).unwrap()
    }

    fn is_at_end(&self) -> bool {
        self.current().kind == TokenKind::Eof
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{
            lex::Scanner,
            token::{Location, Token, TokenKind},
        },
        parser::{
            expr::{BinaryExpr, Expr, FnCall},
            stmt::{Function, Prototype, Stmt},
            LiteralType, Type, Variable,
        },
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

        let expected_ast = Stmt::Expr(Expr::Binary(BinaryExpr {
            left: Box::new(Expr::Literal(LiteralType::I64(2))),
            operator: Token {
                kind: TokenKind::Plus,
                lexeme: "+".to_string(),
                loc: Location { col: 3, line: 1 },
            },
            right: Box::new(Expr::Binary(BinaryExpr {
                left: Box::new(Expr::Binary(BinaryExpr {
                    left: Box::new(Expr::Literal(LiteralType::I64(2))),
                    operator: Token {
                        kind: TokenKind::Star,
                        lexeme: "*".to_string(),
                        loc: Location { col: 7, line: 1 },
                    },
                    right: Box::new(Expr::Literal(LiteralType::I64(3))),
                })),
                operator: Token {
                    kind: TokenKind::Slash,
                    lexeme: "/".to_string(),
                    loc: Location { col: 11, line: 1 },
                },
                right: Box::new(Expr::Literal(LiteralType::I64(2))),
            })),
        }));

        assert_eq!(actual_ast.len(), 1);
        assert_eq!(expected_ast, actual_ast[0]);
    }

    #[test]
    fn parse_parenthesized_arithmetic() {
        let src = "(2 + 2) * 3 / 2".to_string();
        let mut scanner = Scanner::new(src.clone());
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens.to_vec());
        let actual_ast = parser.parse().unwrap();
        trace!("AST for {}: \n{:#?}", src, actual_ast);

        let expected_ast = Stmt::Expr(Expr::Binary(BinaryExpr {
            left: Box::new(Expr::Binary(BinaryExpr {
                left: Box::new(Expr::Binary(BinaryExpr {
                    left: Box::new(Expr::Literal(LiteralType::I64(2))),
                    operator: Token {
                        kind: TokenKind::Plus,
                        lexeme: "+".to_string(),
                        loc: Location { col: 4, line: 1 },
                    },
                    right: Box::new(Expr::Literal(LiteralType::I64(2))),
                })),
                operator: Token {
                    kind: TokenKind::Star,
                    lexeme: "*".to_string(),
                    loc: Location { col: 9, line: 1 },
                },
                right: Box::new(Expr::Literal(LiteralType::I64(3))),
            })),
            operator: Token {
                kind: TokenKind::Slash,
                lexeme: "/".to_string(),
                loc: Location { col: 13, line: 1 },
            },
            right: Box::new(Expr::Literal(LiteralType::I64(2))),
        }));

        assert_eq!(actual_ast.len(), 1);
        assert_eq!(expected_ast, actual_ast[0]);
    }

    #[test]
    fn parse_function_definition() {
        let src = r#"
            fn sum(a: i64, b: i64) i64 {
                2 + 2 
            }
        "#
        .to_string();

        let mut scanner = Scanner::new(src.clone());
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens.to_vec());
        let actual_ast = parser.parse().unwrap();
        trace!("AST for {}: \n{:#?}", src, actual_ast);
        println!("AST for {}: \n{:#?}", src, actual_ast);

        let expected_ast = Function {
            prototype: Prototype {
                name: "sum".to_string(),
                params: vec![
                    Variable {
                        identifier: "a".to_string(),
                        type_: Type::I64,
                    },
                    Variable {
                        identifier: "b".to_string(),
                        type_: Type::I64,
                    },
                ],
                return_type: Type::I64,
            },
            body: vec![],
            return_expr: Some(Expr::Binary(BinaryExpr {
                left: Box::new(Expr::Literal(LiteralType::I64(2))),
                operator: Token {
                    kind: TokenKind::Plus,
                    lexeme: "+".to_string(),
                    loc: Location { col: 18, line: 3 },
                },
                right: Box::new(Expr::Literal(LiteralType::I64(2))),
            })),
            is_builtin: false,
        };

        let func = match actual_ast[0].clone() {
            Stmt::Function(func) => func,
            _ => panic!("Expected a function"),
        };

        assert_eq!(expected_ast, func);
    }

    #[test]
    fn parse_function_call() {
        let src = "fn main() { some_fn(a, b); }".to_string();
        let mut scanner = Scanner::new(src.clone());
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens.to_vec());
        let actual_ast = parser.parse().unwrap();
        trace!("AST for {}: \n{:#?}", src, actual_ast);

        let expected_ast = Stmt::Function(Function {
            prototype: Prototype {
                name: "main".to_string(),
                params: vec![],
                return_type: Type::Void,
            },
            body: vec![Stmt::Expr(Expr::FnCall(FnCall {
                fn_name: "some_fn".to_string(),
                args: vec![
                    Expr::Variable("a".to_string()),
                    Expr::Variable("b".to_string()),
                ],
            }))],
            return_expr: None,
            is_builtin: false,
        });

        assert_eq!(actual_ast.len(), 1);
        assert_eq!(expected_ast, actual_ast[0]);
    }
}
