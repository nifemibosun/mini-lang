#![allow(unused)]
use crate::scanner::token::{ Token, TokenType };
use crate::MiniState;


pub struct Parser<'a> {
    tokens: Vec<Token>,
    current: usize,
    state: &'a mut MiniState,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, state: &'a mut MiniState)-> Self {
        Parser { 
            tokens,
            current: 0,
            state,
        }
    }
}