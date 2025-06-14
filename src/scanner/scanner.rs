use std::collections::HashMap;

use crate::scanner::token::{ Token, TokenType };
use crate::MiniState;


pub struct Scanner<'a> {
    source: Vec<char>,
    state: &'a mut MiniState,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    keywords: HashMap<&'static str, TokenType>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str, state: &'a mut MiniState) -> Self {
        let mut keywords = HashMap::new();

        // Language Keywords
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
        keywords.insert("trait", TokenType::Trait);
        keywords.insert("Self", TokenType::SelfUpper);
        keywords.insert("self", TokenType::SelfLower);
        keywords.insert("construct", TokenType::Construct);
        keywords.insert("loop", TokenType::Loop);
        keywords.insert("await", TokenType::Await);
        keywords.insert("async", TokenType::Async);
        keywords.insert("priv", TokenType::Private);
        keywords.insert("pub", TokenType::Public);


        // Type Keywords
        keywords.insert("bool", TokenType::Bool);
        keywords.insert("char", TokenType::Char);
        keywords.insert("int8", TokenType::Int8);
        keywords.insert("int16", TokenType::Int16);
        keywords.insert("int32", TokenType::Int32);
        keywords.insert("int64", TokenType::Int64);
        keywords.insert("int_n", TokenType::IntN);
        keywords.insert("uint8", TokenType::UInt8);
        keywords.insert("uint16", TokenType::UInt16);
        keywords.insert("uint32", TokenType::UInt32);
        keywords.insert("uint64", TokenType::UInt64);
        keywords.insert("uint_n", TokenType::UIntN);
        keywords.insert("float32", TokenType::Float32);
        keywords.insert("float64", TokenType::Float64);

        Scanner { 
            source: source.chars().collect(),
            state,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            keywords,
        }
    }

    pub fn scan_tokens(&mut self)-> (&Vec<Token>, bool) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(TokenType::EOF, "".to_string(), None, self.line));
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
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            ';' => self.add_token(TokenType::SemiColon),
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
                } else if self.match_token('/') {
                    self.single_line_comment();
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            '!' => {
                if self.match_token('=') {
                    self.add_token(TokenType::BangEqual);
                } else {
                    self.add_token(TokenType::Bang);
                }
            }
            '=' => {
                if self.match_token('=') {
                    self.add_token(TokenType::EqualEqual);
                } else {
                    self.add_token(TokenType::Equal);
                }
            }
            '<' => {
                if self.match_token('=') {
                    self.add_token(TokenType::LessEqual);
                } else {
                    self.add_token(TokenType::LessThan);
                }
            }
            '>' => {
                if self.match_token('=') {
                    self.add_token(TokenType::GreaterEqual);
                } else {
                    self.add_token(TokenType::GreaterThan);
                }
            }
            '|' => {
                if self.match_token('|') {
                    self.add_token(TokenType::Or);
                } else {
                    self.add_token(TokenType::BitOr);
                }
            }
            '&' => {
                if self.match_token('&') {
                    self.add_token(TokenType::And);
                } else {
                    self.add_token(TokenType::BitAnd);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => self.line += 1,
            _ => {
                if self.is_digit(c) {
                    self.number();
                } else if self.is_alpha(c)  {
                    self.identifier();
                } else {
                    self.state.error(self.line, "Unexpected character.");
                }
            }
        }
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

    fn single_line_comment(&mut self) {
        while self.peek() != '\n' && !self.is_at_end() {
            self.advance();
        }
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

    fn is_alpha(&self, c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_digit(&self, c: char) -> bool {
        c.is_numeric()
    }

    fn is_alpha_num(&self, c: char)-> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            self.state.error(self.line, "Unterminated string.");
            return;
        }

        self.advance();

        let value = self.source[self.start + 1..self.current - 1].iter().collect();
        self.add_token_(TokenType::String, Some(value));
    }

    fn number(&mut self) {
        while self.is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            self.advance();

            while self.is_digit(self.peek()) {
                self.advance();
            }
        }

        let num_str: String = self.source[self.start..self.current].iter().collect();

        match num_str.parse::<f64>() {
            Ok(value) => self.add_token_(TokenType::Number, Some(value.to_string())),
            Err(_) => {
                self.state.error(self.line, "Invalid number format.");
            }
        }
    }

    fn is_at_end(&self)-> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self)-> char {
        let c = self.source[self.current];
        self.current += 1;
        c
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_(token_type, None);
    }

    fn add_token_(&mut self, token_type: TokenType, literal: Option<String>) {
        let lexeme: String  = self.source[self.start..self.current].iter().collect();
        self.tokens.push(Token::new(token_type, lexeme, literal, self.line));
    }
}