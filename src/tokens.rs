#![allow(unused)]
#[derive(Debug, PartialEq, Eq, Hash, Clone)]

pub enum TokenType {
    // Single-character tokens.
    LParen, 
    RParen, 
    LBrace, 
    RBrace,
    Comma, 
    Dot, 
    Minus, 
    Plus, 
    SemiColon, 
    Slash, 
    Star,

    // One or two character tokens.
    Bang, 
    BangEqual,
    Equal, 
    EqualEqual,
    Greater, 
    GreaterEqual,
    Less, 
    LessEqual,

    // Literals.
    Identifier, 
    String, 
    Number,

    // Keywords.
    And, 
    Class, 
    Else, 
    False, 
    Fun, 
    For, 
    If, 
    Nil, 
    Or,
    Print, 
    Return, 
    Super, 
    SelfUpper, 
    SelfLower,
    True, 
    Let, 
    Const, 
    While,

    EOF, //End Of File
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<String>,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize, literal: Option<String>) -> Self {
        Token { 
          token_type, 
          lexeme,
          line,
          literal, 
        }
    }

    pub fn to_string(&self) -> String {
        format!("{:?} {} {:?}", self.token_type, self.lexeme, self.literal)
    }
}