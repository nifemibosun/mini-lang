#![allow(unused)]

pub mod ast;

use crate::scanner::token::{Token, TokenType};
use crate::parser::ast::*;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn parse(&mut self) {
        
    }

    fn expression(&mut self) {
        
    }

    fn statement(&mut self) {

    }

    fn declaration(&mut self) {

    }

    fn advance(&mut self) {
        
    }

    fn is_at_end(&self) {
        
    }

    fn check(&mut self, t_type: TokenType) {
        
    }

    fn match_token(&mut self, types: Vec<TokenType>) {
        
    }

    fn peek(&self) {

    }
}
