#![allow(unused)]

pub mod ast;

use crate::parser::ast::{ExprKind, LiteralTypes,PointerKind, Decl, Expr, Node, Program, Stmt, TypeExpr};
use crate::scanner::token::{Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    /// Construct a new parser instance
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    /// The main parse method on every parse instance
    pub fn parse(&mut self) -> Result<Program, String> {
        let mut program = Vec::new();

        while !self.is_at_end() {
            let decl = self.declaration()?;
            program.push(decl);
        }

        Ok(program)
    }

    fn expression(&mut self) -> Result<Expr, String> {
        self.parse_precedence(0)
    }

    fn statement(&mut self) {
        unimplemented!();
    }

    pub fn declaration(&mut self) -> Result<Node<Decl>, String> {
        if self.match_token(&[TokenType::Import]) {
            return self.import_decl();
        } else if self.match_token(&[TokenType::Const]) {
            return self.const_decl();
        } else if self.match_token(&[TokenType::Func]) {
            return self.function_decl();
        } else if self.match_token(&[TokenType::Struct]) {
            return self.struct_decl();
        } else if self.match_token(&[TokenType::Enum]) {
            return self.enum_decl();
        } else if self.match_token(&[TokenType::Construct]) {
            return self.construct_decl();
        }

        Err(format!(
            "Unexpected token {:?} at {:?}",
            self.peek().token_type,
            self.peek().pos
        ))
    }

    fn import_decl(&mut self) -> Result<Node<Decl>, String> {
        let s_pos = self.previous().pos;
        let mut path = Vec::new();

        loop {
            let t_name =
                self.consume(TokenType::Identifier, "Expected identifier after 'import'")?;
            path.push(t_name.lexeme);

            if self.match_token(&[TokenType::ColonColon]) {
                continue;
            }

            if self.check(TokenType::SemiColon) {
                break;
            }
        }

        self.consume(
            TokenType::SemiColon,
            "Expected ';' after import declaration",
        );

        Ok(Node {
            value: Decl::Import { module: path },
            pos: s_pos,
        })
    }

    fn const_decl(&mut self) -> Result<Node<Decl>, String> {
        let s_pos = self.previous().pos;
        let t_name = self.consume(TokenType::Identifier, "Expected identifier after 'const'")?;

        self.consume(TokenType::Colon, "Expected ':' after const name");

        let t_type = self.parse_type()?;

        self.consume(TokenType::Equal, "Expected '=' after token type");
        let value = self.expression()?;

        self.consume(TokenType::SemiColon, "Expected ';' after const declaration");

        Ok(Node {
            value: Decl::Const {
                name: t_name.lexeme,
                r#type: t_type,
                value: value,
            },
            pos: s_pos,
        })
    }

    fn function_decl(&mut self) -> Result<Node<Decl>, String> {
        unimplemented!();
    }

    fn struct_decl(&mut self) -> Result<Node<Decl>, String> {
        unimplemented!();
    }

    fn enum_decl(&mut self) -> Result<Node<Decl>, String> {
        unimplemented!();
    }

    fn construct_decl(&mut self) -> Result<Node<Decl>, String> {
        unimplemented!();
    }

    fn advance(&mut self) -> Token {
        let t = self.peek();
        self.current += 1;
        t
    }

    fn parse_precedence(&mut self, min_prec: u8) -> Result<Expr, String> {
        let mut left = self.parse_primary()?;

        while let Some(op) = self.current_operator() {
            let prec = self.precedence(&op);
            if prec < min_prec {
                break;
            }

            let operator = self.advance().token_type;
            let mut right = self.parse_precedence(prec + 1)?;

            left = Node {
                pos: left.pos.clone(),
                value: ExprKind::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
            };
        }

        Ok(left)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        let token = self.advance();

        let node = match token.token_type {
            TokenType::IntLiteral => {
                let value = token.lexeme.parse::<i64>().map_err(|_| {
                    format!("Invalid integer literal at {:?}", token.pos)
                })?;
                Node {
                    pos: token.pos,
                    value: ExprKind::Literal(LiteralTypes::Int64(value)),
                }
            }
            TokenType::FloatLiteral => {
                let value = token.lexeme.parse::<f64>().map_err(|_| {
                    format!("Invalid float literal at {:?}", token.pos)
                })?;
                Node {
                    pos: token.pos,
                    value: ExprKind::Literal(LiteralTypes::Float64(value)),
                }
            }
            TokenType::StringLiteral => Node {
                pos: token.pos,
                value: ExprKind::Literal(LiteralTypes::String(token.lexeme)),
            },
            TokenType::Identifier => Node {
                pos: token.pos,
                value: ExprKind::Identifier(token.lexeme),
            },
            TokenType::LParen => {
                let expr = self.expression()?;
                self.consume(TokenType::RParen, "Expected ')' after expression")?;
                Node {
                    pos: token.pos,
                    value: ExprKind::Grouping(Box::new(expr)),
                }
            }
            _ => return Err(format!("Unexpected token {:?} in expression", token)),
        };

        Ok(node)
    }

    fn current_operator(&self) -> Option<TokenType> {
        match self.peek().token_type {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Star
            | TokenType::Slash
            | TokenType::EqualEqual
            | TokenType::BangEqual
            | TokenType::Less
            | TokenType::LessEqual
            | TokenType::Greater
            | TokenType::GreaterEqual => Some(self.peek().token_type.clone()),
            _ => None,
        }
    }

    fn precedence(&self, op: &TokenType) -> u8 {
        match op {
            TokenType::Star | TokenType::Slash => 10,
            TokenType::Plus | TokenType::Minus => 9,
            TokenType::EqualEqual
            | TokenType::BangEqual
            | TokenType::Less
            | TokenType::LessEqual
            | TokenType::Greater
            | TokenType::GreaterEqual => 5,
            _ => 0,
        }
    }

    fn consume(&mut self, t_type: TokenType, msg: &str) -> Result<Token, String> {
        if self.check(t_type) {
            Ok(self.advance())
        } else {
            Err(format!("{} at {:?}", msg, self.peek().pos))
        }
    }

    fn previous(&mut self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    /// Check if a token has a specific type that is needed
    fn check(&mut self, t_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().token_type == t_type
    }

    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for t_type in types {
            if self.check(*t_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn parse_type(&mut self) -> Result<TypeExpr, String> {
        if self.match_token(&[TokenType::Star]) {
            let mutable = if self.match_token(&[TokenType::Mut]) {
                PointerKind::Mut
            } else if self.match_token(&[TokenType::Const]) {
                PointerKind::Const
            } else {
                PointerKind::Default
            };

            let target = self.parse_type()?;
            
            return Ok(TypeExpr::Pointer {
                mutable,
                target: Box::new(target),
            });
        }

        if self.match_token(&[TokenType::LSquare]) {
            let size = if self.check(TokenType::IntLiteral) {
                let size_token = self.advance();
                Some(size_token.lexeme.parse::<usize>().map_err(|_| {
                    format!("Invalid array size at {:?}", size_token.pos)
                })?)
            } else {
                None
            };

            self.consume(TokenType::RSquare, "Expected ']' after array size")?;

            let element = self.parse_type()?;
            return Ok(TypeExpr::Array {
                mutable: false,
                size,
                element: Box::new(element),
            });
        }

        let ident = self.consume(TokenType::Identifier, "Expected type name")?.lexeme;

        if self.match_token(&[TokenType::Less]) {
            let mut args = Vec::new();
            loop {
                args.push(self.parse_type()?);
                if self.match_token(&[TokenType::Comma]) {
                    continue;
                }
                self.consume(TokenType::Greater, "Expected '>' after generic args")?;
                break;
            }
            return Ok(TypeExpr::Generic(ident, args));
        }

        Ok(TypeExpr::Named(ident))
    }
}
