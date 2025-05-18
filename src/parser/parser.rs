#![allow(unused)]
use crate::scanner::token::{ Token, TokenType };


pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>)-> Self {
        Parser { 
            tokens,
            current: 0,
        }
    }
}