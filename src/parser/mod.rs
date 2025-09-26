#![allow(unused)]

pub mod ast;

use crate::parser::ast::*;
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
        unimplemented!();
    }

    fn expression(&mut self) {
        unimplemented!();
    }

    fn statement(&mut self) {
        unimplemented!();
    }

    fn declaration(&mut self) -> Result<Node<Decl>, String> {
        if self.match_token(&[TokenType::Import]) {

        } else if self.match_token(&[TokenType::Const]) {

        } else if self.match_token(&[TokenType::Func]) {
            
        } else if self.match_token(&[TokenType::Struct]) {
            
        } else if self.match_token(&[TokenType::Enum]) {
            
        } else if self.match_token(&[TokenType::Construct]) {
            
        } else {
            
        }

        unimplemented!();
    }

    fn function_decl(&mut self) -> FuncDecl {
        unimplemented!();
    }

    fn advance(&mut self) -> Token {
        let t = self.peek();
        self.current += 1;
        t
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn check(&mut self, t_type: TokenType) -> bool {
        if !self.is_at_end() {
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
}
