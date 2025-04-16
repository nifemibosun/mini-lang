use crate::tokens::{ TokenType, Token };
use crate::mini::Mini;

#[allow(unused)]
pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

#[allow(unused)]
impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(TokenType::EOF, String::new(), None, self.line));
        return self.tokens.clone();
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        let mut lox = Mini::new();

        match c {
            '(' => self.add_token(TokenType::LParen),
            ')' => self.add_token(TokenType::RParen),
            '{' => self.add_token(TokenType::LBrace),
            '}' => self.add_token(TokenType::RBrace),
            '[' => self.add_token(TokenType::RSquare),
            ']' => self.add_token(TokenType::LSquare),
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
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => self.line += 1,
            '"' => self.string(),
            '|' => {
                if self.match_char('|') && self.peek() == ' ' {
                    self.add_token(TokenType::Or);
                }
            }
            _ => {
                if self.is_digit(c) {
                    self.number();
                } else if self.is_alpha(c) {
                    self.identifier();
                } else {
                    lox.error(self.line, "Unexpected character.");
                }
            }
        }
    }

    fn identifier(&mut self) {
        while self.is_alphanum(self.peek()) {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let mut token_type = self.keywords(text);

        if token_type == TokenType::Nil {
            token_type = TokenType::Identifier;
        }
        self.add_token(TokenType::Identifier);
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
        self.add_token(TokenType::Number);

        let sub = &self.source[self.start..self.current];
        let parse_sub: f64 = sub.parse().expect("Failed to parse.");
    }

    fn string(&mut self) {
        let mut mini = Mini::new();

        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            mini.error(self.line, "Unterminated string");
            return;
        }

        self.advance();

        let value = &self.source[self.start + 1..self.current - 1];
        self.add_token_with_value(TokenType::String, Some(value.to_string()));
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source.chars().nth(self.current).unwrap() != expected {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn is_alphanum(&mut self, c: char) -> bool {
        return self.is_alpha(c) || self.is_digit(c);
    }

    fn is_alpha(&mut self, c: char) -> bool {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
    }

    fn is_digit(&mut self, c: char) -> bool {
        return c >= '0' && c <= '9';
    }

    fn advance(&self) -> char {
        return self.source.chars().nth(self.current).unwrap();
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_with_value(token_type, None);
    }

    fn add_token_with_value(&mut self, token_type: TokenType, literal: Option<String>) {
        let text = self.source[self.start..self.current].to_string();
        self.tokens.push(Token::new(token_type, text, literal, self.line))
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        return self.source.chars().nth(self.current).unwrap();
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        } 

        return self.source.chars().nth(self.current + 1).unwrap();
    }

    fn keywords(&self, text: &str) -> TokenType {
        match text {
            "struct" => TokenType::Struct,
            "construct" => TokenType::Construct,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "func" => TokenType:: Func,
            "return" => TokenType::Return,
            "self" => TokenType::SelfLower,
            "Self" => TokenType::SelfUpper,
            "let" => TokenType::Let,
            "const" => TokenType::Const,
            "while" => TokenType::While,
            "loop" => TokenType::Loop,
            "pub" => TokenType::Public,
            "nil" => TokenType::Nil,
            "bool" => TokenType::Boolean,
            "char" => TokenType::Char,
            "string" => TokenType::Str,
            "int8" => TokenType::Int8,
            "int16" => TokenType::Int16,
            "int32" => TokenType::Int32,
            "int64" => TokenType::Int64,
            "int128" => TokenType::Int128,
            "intn" => TokenType::IntN,
            "uint8" => TokenType::UInt8,
            "uint16" => TokenType::UInt16,
            "uint32" => TokenType::UInt32,
            "uint64" => TokenType::UInt64,
            "uint128" => TokenType::UInt128,
            "uintn" => TokenType::UIntN,
            "float8" => TokenType::Float8,
            "float16" => TokenType::Float16,
            "float32" => TokenType::Float32,
            "float64" => TokenType::Float64,
            "float128" => TokenType::Float128,
            "floatn" => TokenType::FloatN,
            _ => TokenType::Identifier,
        }
    }
}