use crate::tokens::{ TokenType, Token };
use crate::mini::Mini;

#[allow(unused)]
pub struct Scanner {
    pub chars: Vec<char>,
    pub tokens: Vec<Token>,
    pub start: usize,
    pub current: usize,
    pub line: usize,
    mini: Mini
}

#[allow(unused)]
impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            chars: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            mini: Mini::new()
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(TokenType::EOF, String::new(), None, self.line));
        return &self.tokens;
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }

    fn scan_token(&mut self) {
        let c = self.advance();

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
                if self.is_digit(c) {
                    self.number();
                } else if self.is_alpha(c) {
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
        let value: f64 = text.parse().expect("Failed to parse.");

        self.add_token_with_value(TokenType::Number, Some(value.to_string()));
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

    fn advance(&mut self) -> char {
        let ch = self.chars[self.current];
        self.current += 1;

        return ch;
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
            return '\0';
        }

        return self.chars[self.current];
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.chars.len() {
            return '\0';
        } 

        return self.chars[self.current + 1];
    }

    fn keywords(&self, text: &str) -> TokenType {
        if let Some(token_type) = self.lang_keyword(text) {
            return token_type;
        }

        if let Some(token_type) = self.type_keyword(text)  {
            return token_type;
        }

        TokenType::Identifier
    }

    fn lang_keyword(&self, text: &str) -> Option<TokenType> {
        match text {
            "struct" => Some(TokenType::Struct),
            "construct" => Some(TokenType::Construct),
            "if" => Some(TokenType::If),
            "else" => Some(TokenType::Else),
            "true" => Some(TokenType::True),
            "false" => Some(TokenType::False),
            "nil" => Some(TokenType::Nil),
            "for" => Some(TokenType::For),
            "func" => Some(TokenType:: Func),
            "return" => Some(TokenType::Return),
            "self" => Some(TokenType::SelfLower),
            "Self" => Some(TokenType::SelfUpper),
            "let" => Some(TokenType::Let),
            "const" => Some(TokenType::Const),
            "while" => Some(TokenType::While),
            "loop" => Some(TokenType::Loop),
            "pub" => Some(TokenType::Public),
            "enum" => Some(TokenType::Enum),
            "type" => Some(TokenType::Type),
            "Arr" => Some(TokenType::Array),
            "arr" => Some(TokenType::ArrayLiteral),
            "import" => Some(TokenType::Import),
            _ => None
        }
    }

    fn type_keyword(&self, text: &str) -> Option<TokenType> {
        match text {
            "bool" => Some(TokenType::Boolean),
            "char" => Some(TokenType::Char),
            "string" => Some(TokenType::Str),
            "int8" => Some(TokenType::Int8),
            "int16" => Some(TokenType::Int16),
            "int32" => Some(TokenType::Int32),
            "int64" => Some(TokenType::Int64),
            "int128" => Some(TokenType::Int128),
            "int_n" => Some(TokenType::IntN),
            "uint8" => Some(TokenType::UInt8),
            "uint16" => Some(TokenType::UInt16),
            "uint32" => Some(TokenType::UInt32),
            "uint64" => Some(TokenType::UInt64),
            "uint128" => Some(TokenType::UInt128),
            "uint_n" => Some(TokenType::UIntN),
            "float32" => Some(TokenType::Float32),
            "float64" => Some(TokenType::Float64),
            "float128" => Some(TokenType::Float128),
            _ => None
        }
    }
}
