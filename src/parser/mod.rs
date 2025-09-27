#![allow(unused)]

pub mod ast;

use std::f64::consts::E;

use crate::parser::ast::{Decl, Expr, Node, Program, Stmt, TypeExpr};
use crate::scanner::token::{Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn parse(&mut self) -> Result<Program, String> {
        let mut program = Vec::new();

        while !self.is_at_end() {
            let decl = self.declaration()?;
            program.push(decl);
        }

        Ok(program)
    }

    fn expression(&mut self) -> Result<Expr, String> {
        unimplemented!();
    }

    fn statement(&mut self) {
        unimplemented!();
    }

    fn declaration(&mut self) -> Result<Node<Decl>, String> {
        if self.match_token(&[TokenType::Import]) {
            self.import_decl();
        } else if self.match_token(&[TokenType::Const]) {
            self.const_decl();
        } else if self.match_token(&[TokenType::Func]) {
            self.function_decl();
        } else if self.match_token(&[TokenType::Struct]) {
            self.struct_decl();
        } else if self.match_token(&[TokenType::Enum]) {
            self.enum_decl();
        } else if self.match_token(&[TokenType::Construct]) {
            self.construct_decl();
        } else {
        }

        unimplemented!();
    }

    fn import_decl(&mut self) -> Result<Node<Decl>, String> {
        let s_pos = self.previous().pos;
        let mut path = Vec::new();

        while !self.match_token(&[TokenType::SemiColon]) {
            let t_name =
                self.consume(TokenType::Identifier, "Expected identifier after 'import'")?;

            if self.match_token(&[TokenType::SemiColon]) {
                path.push(t_name.lexeme);
            } else {
                path.push(t_name.lexeme);
                self.consume(TokenType::ColonColon, "Expected '::' after import name");
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
        unimplemented!();
    }
}
