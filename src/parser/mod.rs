#![allow(unused)]

pub mod ast;

use crate::scanner::token::{Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    current: isize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }
}
