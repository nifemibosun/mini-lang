use std::collections::Hashmap;

use crate::tokens::{ TokenType, Token };
use crate::mini::Mini;

#[allow(unused)]
pub struct Scanner {
    pub chars: Vec<char>,
    pub tokens: Vec<Token>,
    pub start: usize,
    pub current: usize,
    pub line: usize,
    mini: Mini,
    keywords: Hashmap<String, TokenType>
}

#[allow(unused)]
impl Scanner {
    pub fn new(source: String) -> Self {

        let mut keywords = HashMap::new();
        
        // Language keywords
        keywords.insert("struct".to_string(), TokenType::Struct);
        keywords.insert("construct".to_string(), TokenType::Construct);
        keywords.insert("if".to_string(), TokenType::If);
        keywords.insert("else".to_string(), TokenType::Else);
        keywords.insert("true".to_string(), TokenType::True);
        keywords.insert("false".to_string(), TokenType::False);
        keywords.insert("nil".to_string(), TokenType::Nil);
        keywords.insert("for".to_string(), TokenType::For);
        keywords.insert("func".to_string(), TokenType::Func);
        keywords.insert("return".to_string(), TokenType::Return);
        keywords.insert("self".to_string(), TokenType::SelfLower);
        keywords.insert("Self".to_string(), TokenType::SelfUpper);
        keywords.insert("let".to_string(), TokenType::Let);
        keywords.insert("const".to_string(), TokenType::Const);
        keywords.insert("while".to_string(), TokenType::While);
        keywords.insert("loop".to_string(), TokenType::Loop);
        keywords.insert("pub".to_string(), TokenType::Public);
        keywords.insert("enum".to_string(), TokenType::Enum);
        keywords.insert("type".to_string(), TokenType::Type);
        keywords.insert("Arr".to_string(), TokenType::Array);
        keywords.insert("arr".to_string(), TokenType::ArrayLiteral);
        keywords.insert("import".to_string(), TokenType::Import);
        
        // Type keywords
        keywords.insert("bool".to_string(), TokenType::Boolean);
        keywords.insert("char".to_string(), TokenType::Char);
        keywords.insert("string".to_string(), TokenType::Str);
        keywords.insert("int8".to_string(), TokenType::Int8);
        keywords.insert("int16".to_string(), TokenType::Int16);
        keywords.insert("int32".to_string(), TokenType::Int32);
        keywords.insert("int64".to_string(), TokenType::Int64);
        keywords.insert("int128".to_string(), TokenType::Int128);
        keywords.insert("int_n".to_string(), TokenType::IntN);
        keywords.insert("uint8".to_string(), TokenType::UInt8);
        keywords.insert("uint16".to_string(), TokenType::UInt16);
        keywords.insert("uint32".to_string(), TokenType::UInt32);
        keywords.insert("uint64".to_string(), TokenType::UInt64);
        keywords.insert("uint128".to_string(), TokenType::UInt128);
        keywords.insert("uint_n".to_string(), TokenType::UIntN);
        keywords.insert("float32".to_string(), TokenType::Float32);
        keywords.insert("float64".to_string(), TokenType::Float64);
        
        Self {
            chars: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            mini: Mini::new(),
            keywords
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(TokenType::EOF, String::new(), None, self.line));
        &self.tokens
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }

    fn scan_token(&mut self) {
        let current_char = self.advance();

        match current_char {
            '(' => self.add_token(TokenType::LParen),
            ')' => self.add_token(TokenType::RParen),
            '{' => self.add_token(TokenType::LBrace),
            '}' => self.add_token(TokenType::RBrace),
            '[' => self.add_token(TokenType::LSquare),
            ']' => self.add_token(TokenType::RSquare),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            '*' => self.add_token(TokenType::Star),
            ';' => self.add_token(TokenType::SemiColon),
            ':' => {
                if self.match_char(':') {
                    self.add_token(TokenType::ColonColon);
                } else {
                    self.add_token(TokenType::Colon);
                }
            }
            '&' => {
                if self.match_char('&') {
                    self.add_token(TokenType::And);
                } else {
                    self.add_token(TokenType::Ampisand)
                }
            }
            '!' => {
                if self.match_char('=') {
                    self.add_token(TokenType::BangEqual);
                } else {
                    self.add_token(TokenType::Bang);
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.add_token(TokenType::EqualEqual);
                } else {
                    self.add_token(TokenType::Equal);
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.add_token(TokenType::LessEqual);
                } else {
                    self.add_token(TokenType::Less);
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.add_token(TokenType::GreaterEqual);
                } else {
                    self.add_token(TokenType::Greater);
                }
            }
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_char('*') {
                    self.skip_multiline_comment();
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => self.line += 1,
            '"' => self.string(),
            '|' => {
                if self.match_char('|') {
                    self.add_token(TokenType::Or);
                }
            }
            _ => {
                if self.is_digit(current_char) {
                    self.number();
                } else if self.is_alpha(current_char) {
                    self.identifier();
                } else {
                    self.mini.error(self.line, "Unexpected character.");
                }
            }
        }
    }

    fn identifier(&mut self) {
        while self.is_alphanum(self.peek()) {
            self.advance();
        }

        let text: String = self.chars[self.start..self.current].iter().collect();
        let mut token_type = self.keywords(&text);

        if token_type == TokenType::Nil {
            token_type = TokenType::Identifier;
        }
        self.add_token(token_type);
    }

    fn skip_multiline_comment(&mut self) {
        let mut depth = 1;

        while depth > 0 && !self.is_at_end() {
            if self.peek() == '/' && self.peek_next() == '*' {
                self.advance();
                self.advance();
                depth += 1;
            } else if self.peek() == '*' && self.peek_next() == '/' {
                self.advance();
                self.advance();
                depth -= 1;
            } else {
                if self.peek() == '\n' {
                    self.line += 1;
                }
                self.advance();
            }
        }

        if depth > 0 {
            self.mini.error(self.line, "Unterminated multiline comment");
        }
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

        let text: String = self.chars[self.start..self.current].iter().collect();
        
        match text.parse::<f64> {
            Ok(value) => self.add_token_with_value(TokenType::Number, Some(value.to_string())),
            Err(_) => self.mini.error(self.line, "Invalid number format"),
        }
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.mini.error(self.line, "Unterminated string");
            return;
        }

        self.advance();

        let value: String = self.chars[self.start + 1..self.current - 1].iter().collect();
        self.add_token_with_value(TokenType::String, Some(value.to_string()));
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.chars[self.current] != expected {
            false
        }

        self.current += 1;
        
        true
    }

    fn is_alphanum(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn is_alpha(&self, c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    }

    fn is_digit(&self, c: char) -> bool {
        c >= '0' && c <= '9'
    }

    fn advance(&mut self) -> char {
        let ch = self.chars[self.current];
        self.current += 1;

        ch
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_with_value(token_type, None);
    }

    fn add_token_with_value(&mut self, token_type: TokenType, literal: Option<String>) {
        let text = self.chars[self.start..self.current].iter().collect();
        self.tokens.push(Token::new(token_type, text, literal, self.line))
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        }

        self.chars[self.current]
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.chars.len() {
            '\0'
        } 

        self.chars[self.current + 1]
    }

    fn keywords(&self, text: &str) -> TokenType {
        match self.keywords.get(text) {
            Some(token_type) => *token_type,
            None => TokenType::Identifier,
        }
    }
}
