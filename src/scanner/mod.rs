#![allow(unused)]
pub mod token;

use super::MiniState;
use crate::scanner::token::{ Literal, Position, Token, TokenType };


#[derive(Debug, PartialEq)]
pub struct Scanner<'a> {
    source: &'a str,
    pub tokens: Vec<Token>,
    start: usize,
    current: usize,
    pos: Position,
    state: &'a mut MiniState,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str, state: &'a mut MiniState) -> Self {
        Scanner {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            pos: Position {
                line: 1,
                col: 1,
            },
            state,
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

    fn match_token(&mut self, expected: char)-> bool {
        if self.is_at_end() {
            return false;
        }

        if self.peek() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn peek(&self)-> char {
        if self.is_at_end() {
            return '\0';
        }

        self.source[self.current..].chars().next().unwrap()
    }

    fn peek_next(&self)-> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }

        self.source[self.current..].chars().nth(1).unwrap_or('\0')
    }

    fn advance(&mut self)-> char {
        let c = self.peek();
        self.current += c.len_utf8();
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

    #[inline]
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    #[inline]
    fn is_alpha(&self, c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    #[inline]
    fn is_digit(&self, c: char) -> bool {
        c.is_numeric()
    }

    #[inline]
    fn is_alpha_num(&self, c: char)-> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn identifier(&mut self) {
        while self.is_alpha_num(self.peek()) {
            self.advance();
        }

        let text = &self.source[self.start..self.current];

        match self.keywords(text) {
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

        let value = &self.source[self.start + 1..self.current - 1];

        self.add_token_(TokenType::String, Some(Literal::String(value.to_string())));
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

        let num_str = &self.source[self.start..self.current];

        if is_float {
            match num_str.parse::<f64>() {
                Ok(value) => self.add_token_(
                    TokenType::FloatLiteral,
                    Some(Literal::Float(value))
                ),
                Err(_) => self.state.error(self.pos, "Invalid float literal"),
            }
        } else {
            match num_str.parse::<isize>() {
                Ok(value) => self.add_token_(
                    TokenType::IntLiteral,
                    Some(Literal::Int(value))
                ),
                Err(_) => self.state.error(self.pos, "Invalid integer literal"),
            }
        }
    }

    fn keywords(&mut self, text: &str) -> Option<TokenType> {
        match text {
            "if" => Some(TokenType::If),
            "else" => Some(TokenType::Else),
            "while" => Some(TokenType::While),
            "func" => Some(TokenType::Func),
            "return" => Some(TokenType::Return),
            "let" => Some(TokenType::Let),
            "const" => Some(TokenType::Const),
            "mut" => Some(TokenType::Mut),
            "true" => Some(TokenType::True),
            "false" => Some(TokenType::False),
            "match" => Some(TokenType::Match),
            "import" => Some(TokenType::Import),
            "for" => Some(TokenType::For),
            "in" => Some(TokenType::In),
            "struct" => Some(TokenType::Struct),
            "enum" => Some(TokenType::Enum),
            "Self" => Some(TokenType::SelfUpper),
            "self" => Some(TokenType::SelfLower),
            "construct" => Some(TokenType::Construct),
            "loop" => Some(TokenType::Loop),
            "await" => Some(TokenType::Await),
            "async" => Some(TokenType::Async),
            "public" => Some(TokenType::Public),
            _ => None
        }
    }

    #[inline]
    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_(token_type, None);
    }

    fn add_token_(&mut self, token_type: TokenType, literal: Option<Literal>) {
        let lexeme  = &self.source[self.start..self.current];
        self.tokens.push(Token::new(token_type, lexeme.to_string(), literal, self.pos));
    }
}