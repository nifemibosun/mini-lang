#![allow(unused)]
pub mod token;

use std::collections::HashMap;
use super::MiniState;
use crate::scanner::token::{ Literal, Position, Token, TokenType };


#[derive(Debug, PartialEq)]
pub struct Scanner<'a> {
    source: Vec<char>,
    pub tokens: Vec<Token>,
    start: usize,
    current: usize,
    pos: Position,
    state: &'a mut MiniState,
    keywords: HashMap<&'static str, TokenType>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str, state: &'a mut MiniState) -> Self {
        let mut keywords = HashMap::new();

        keywords.insert("if", TokenType::If);
        keywords.insert("else", TokenType::Else);
        keywords.insert("while", TokenType::While);
        keywords.insert("func", TokenType::Func);
        keywords.insert("return", TokenType::Return);
        keywords.insert("let", TokenType::Let);
        keywords.insert("const", TokenType::Const);
        keywords.insert("mut", TokenType::Mut);
        keywords.insert("true", TokenType::True);
        keywords.insert("false", TokenType::False);
        keywords.insert("match", TokenType::Match);
        keywords.insert("import", TokenType::Import);
        keywords.insert("for", TokenType::For);
        keywords.insert("in", TokenType::In);
        keywords.insert("struct", TokenType::Struct);
        keywords.insert("enum", TokenType::Enum);
        keywords.insert("Self", TokenType::SelfUpper);
        keywords.insert("self", TokenType::SelfLower);
        keywords.insert("construct", TokenType::Construct);
        keywords.insert("loop", TokenType::Loop);
        keywords.insert("await", TokenType::Await);
        keywords.insert("async", TokenType::Async);
        keywords.insert("public", TokenType::Public);

        Scanner {
            source: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            pos: Position {
                line: 1,
                col: 1,
            },
            state,
            keywords,
        }
    }

    pub fn scan_tokens(&mut self) -> (&Vec<Token>, bool) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(TokenType::EoF, "".to_string(), None, self.pos));
        (&self.tokens, self.state.had_error)
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LParen),
            ')' => self.add_token(TokenType::RParen),
            '{' => self.add_token(TokenType::LBrace),
            '}' => self.add_token(TokenType::RBrace),
            '[' => self.add_token(TokenType::LSquare),
            ']' => self.add_token(TokenType::RSquare),
            ';' => self.add_token(TokenType::SemiColon),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '?' => self.add_token(TokenType::Question),
            '@' => self.add_token(TokenType::At),
            '#' => self.add_token(TokenType::Pound),
            '$' => self.add_token(TokenType::Dollar),
            '"' => self.string(),
            ':' => {
                if self.match_token(':') {
                    self.add_token(TokenType::ColonColon);
                } else {
                    self.add_token(TokenType::Colon);
                }
            }
            '+' => {
                if self.match_token('=') {
                    self.add_token(TokenType::PlusEqual);
                } else {
                    self.add_token(TokenType::Plus);
                }
            }
            '-' => {
                if self.match_token('=') {
                    self.add_token(TokenType::MinusEqual);
                } else {
                    self.add_token(TokenType::Minus);
                }
            }
            '*' => {
                if self.match_token('=') {
                    self.add_token(TokenType::StarEqual);
                } else {
                    self.add_token(TokenType::Star);
                }
            }
            '/' => {
                if self.match_token('=') {
                    self.add_token(TokenType::SlashEqual);
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            '=' => {
                if self.match_token('=') {
                    self.add_token(TokenType::EqualEqual);
                } else {
                    self.add_token(TokenType::Equal);
                }
            }
            '!' => {
                if self.match_token('=') {
                    self.add_token(TokenType::BangEqual);
                } else {
                    self.add_token(TokenType::Bang);
                }
            }
            '<' => {
                if self.match_token('=') {
                    self.add_token(TokenType::LessEqual);
                } else {
                    self.add_token(TokenType::Less);
                }
            }
            '>' => {
                if self.match_token('=') {
                    self.add_token(TokenType::GreaterEqual);
                } else {
                    self.add_token(TokenType::Greater);
                }
            }
            '&' => {
                if self.match_token('&') {
                    self.add_token(TokenType::And);
                } else {
                    self.add_token(TokenType::Ampersand);
                }
            }
            '|' => {
                if self.match_token('|') {
                    self.add_token(TokenType::Or);
                } else {
                    self.add_token(TokenType::Bar);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => self.pos.line += 1,
            _ => {
                if self.is_digit(c) {
                    self.number();
                } else if self.is_alpha(c)  {
                    self.identifier();
                } else {
                    self.state.error(self.pos, "Unexpected character");
                }
            }
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn match_token(&mut self, expected: char)-> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source[self.current] != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn peek(&self)-> char {
        if self.is_at_end() {
            return '\0';
        }

        self.source[self.current]
    }

    fn peek_next(&self)-> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }

        self.source[self.current + 1]
    }

    fn advance(&mut self)-> char {
        let c = self.source[self.current];
        self.current += 1;
        if c == '\n' {
            self.pos.line += 1;
            self.pos.col = 1;
        } else {
            self.pos.col += 1;
        }
        c
    }

    fn single_line_comment(&mut self) {
        while self.peek() != '\n' && !self.is_at_end() {
            self.advance();
        }
    }

    fn is_alpha(&self, c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_digit(&self, c: char) -> bool {
        c.is_numeric()
    }

    fn is_alpha_num(&self, c: char)-> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn identifier(&mut self) {
        while self.is_alpha_num(self.peek()) {
            self.advance();
        }

        let text: String = self.source[self.start..self.current].iter().collect();

        match self.keywords.get(text.as_str()) {
            Some(token_type) => self.add_token(token_type.clone()),
            None => self.add_token(TokenType::Identifier)
        }
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.pos.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            self.state.error(self.pos, "Unterminated string");
            return;
        }

        self.advance();

        let value: String = self.source[self.start + 1..self.current - 1]
            .iter()
            .collect();

        self.add_token_(TokenType::String, Some(Literal::String(value)));
    }

    fn number(&mut self) {
        let mut is_float = false;

        while self.is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            is_float = true;
            self.advance();

            while self.is_digit(self.peek()) {
                self.advance();
            }
        }

        let num_str: String = self.source[self.start..self.current].iter().collect();

        if is_float {
            match num_str.parse::<f64>() {
                Ok(value) => self.add_token_(
                    TokenType::FloatLiteral,
                    Some(Literal::Float64(value))
                ),
                Err(_) => self.state.error(self.pos, "Invalid float literal"),
            }
        } else {
            match num_str.parse::<i64>() {
                Ok(value) => self.add_token_(
                    TokenType::IntLiteral,
                    Some(Literal::Int64(value))
                ),
                Err(_) => self.state.error(self.pos, "Invalid integer literal"),
            }
        }
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_(token_type, None);
    }

    fn add_token_(&mut self, token_type: TokenType, literal: Option<Literal>) {
        let lexeme: String  = self.source[self.start..self.current].iter().collect();
        self.tokens.push(Token::new(token_type, lexeme, literal, self.pos));
    }
}