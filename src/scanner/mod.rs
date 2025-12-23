// file: src/scanner/mod.rs
//! Scanner (lexer) module
//!
//! The scanner turns a source string into a stream of tokens. Each token
//! contains a TokenType, lexeme, optional literal and position information.
//! The scanner is designed to be UTF-8 safe and to report unterminated
//! strings or other lexical errors via the shared MiniState.

#![allow(unused)]

pub mod token;

use super::MiniState;

#[derive(Debug, PartialEq)]
pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<token::Token>,
    start: usize,
    current: usize,
    pos: token::Position,
    start_pos: token::Position,
    state: &'a mut MiniState,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str, state: &'a mut MiniState) -> Self {
        Scanner {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            pos: token::Position::new(),
            start_pos: token::Position::new(),
            state,
        }
    }

    pub fn scan_tokens(&mut self) -> (Vec<token::Token>, bool) {
        while !self.is_at_end() {
            self.start = self.current;
            self.start_pos = self.pos;
            self.scan_token();
        }

        self.tokens.push(token::Token::new(
            token::TokenType::EoF,
            "".to_string(),
            None,
            self.pos,
        ));
        (std::mem::take(&mut self.tokens), self.state.had_error)
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(token::TokenType::LParen),
            ')' => self.add_token(token::TokenType::RParen),
            '{' => self.add_token(token::TokenType::LBrace),
            '}' => self.add_token(token::TokenType::RBrace),
            '[' => self.add_token(token::TokenType::LSquare),
            ']' => self.add_token(token::TokenType::RSquare),
            ';' => self.add_token(token::TokenType::SemiColon),
            ',' => self.add_token(token::TokenType::Comma),
            '.' => self.add_token(token::TokenType::Dot),
            '?' => self.add_token(token::TokenType::Question),
            '@' => self.add_token(token::TokenType::At),
            '#' => self.add_token(token::TokenType::Pound),
            '$' => self.add_token(token::TokenType::Dollar),
            '%' => self.add_token(token::TokenType::Mod),
            '"' => self.string(),
            ':' => {
                if self.match_token(':') {
                    self.add_token(token::TokenType::ColonColon);
                } else {
                    self.add_token(token::TokenType::Colon);
                }
            }
            '+' => {
                if self.match_token('=') {
                    self.add_token(token::TokenType::PlusEqual);
                } else {
                    self.add_token(token::TokenType::Plus);
                }
            }
            '-' => {
                if self.match_token('=') {
                    self.add_token(token::TokenType::MinusEqual);
                } else {
                    self.add_token(token::TokenType::Minus);
                }
            }
            '*' => {
                if self.match_token('=') {
                    self.add_token(token::TokenType::StarEqual);
                } else {
                    self.add_token(token::TokenType::Star);
                }
            }
            '/' => {
                if self.match_token('=') {
                    self.add_token(token::TokenType::SlashEqual);
                } else if self.match_token('/') {
                    self.single_line_comment();
                } else {
                    self.add_token(token::TokenType::Slash);
                }
            }
            '=' => {
                if self.match_token('=') {
                    self.add_token(token::TokenType::EqualEqual);
                } else {
                    self.add_token(token::TokenType::Equal);
                }
            }
            '!' => {
                if self.match_token('=') {
                    self.add_token(token::TokenType::BangEqual);
                } else {
                    self.add_token(token::TokenType::Bang);
                }
            }
            '<' => {
                if self.match_token('=') {
                    self.add_token(token::TokenType::LessEqual);
                } else {
                    self.add_token(token::TokenType::Less);
                }
            }
            '>' => {
                if self.match_token('=') {
                    self.add_token(token::TokenType::GreaterEqual);
                } else {
                    self.add_token(token::TokenType::Greater);
                }
            }
            '&' => {
                if self.match_token('&') {
                    self.add_token(token::TokenType::And);
                } else {
                    self.add_token(token::TokenType::Ampersand);
                }
            }
            '|' => {
                if self.match_token('|') {
                    self.add_token(token::TokenType::Or);
                } else {
                    self.add_token(token::TokenType::Bar);
                }
            }
            ' ' | '\r' | '\t' | '\n' => {}
            _ => {
                if self.is_digit(c) {
                    self.number();
                } else if self.is_alpha(c) {
                    self.identifier();
                } else {
                    self.state.error(self.pos, "Unexpected character");
                }
            }
        }
    }

    fn match_token(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.peek() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.source[self.current..].chars().next().unwrap()
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }

        self.source[self.current..].chars().nth(1).unwrap_or('\0')
    }

    fn advance(&mut self) -> char {
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

    fn identifier(&mut self) {
        while self.is_alpha_num(self.peek()) {
            self.advance();
        }

        let text = &self.source[self.start..self.current];

        match self.keywords(text) {
            Some(token_type) => self.add_token(token_type),
            None => self.add_token(token::TokenType::Identifier),
        }
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            self.advance();
        }

        if self.is_at_end() {
            self.state.error(self.pos, "Unterminated string");
            return;
        }

        self.advance();

        let value = &self.source[self.start + 1..self.current - 1];

        self.add_token_(
            token::TokenType::StringLiteral,
            Some(token::LiteralTypes::String(value.to_string())),
        );
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
                    token::TokenType::FloatLiteral,
                    Some(token::LiteralTypes::Float(value)),
                ),
                Err(_) => self.state.error(self.pos, "Invalid float literal"),
            }
        } else {
            match num_str.parse::<isize>() {
                Ok(value) => self.add_token_(
                    token::TokenType::IntLiteral,
                    Some(token::LiteralTypes::Int(value)),
                ),
                Err(_) => self.state.error(self.pos, "Invalid integer literal"),
            }
        }
    }

    fn keywords(&mut self, text: &str) -> Option<token::TokenType> {
        match text {
            "if" => Some(token::TokenType::If),
            "else" => Some(token::TokenType::Else),
            "while" => Some(token::TokenType::While),
            "func" => Some(token::TokenType::Func),
            "return" => Some(token::TokenType::Return),
            "let" => Some(token::TokenType::Let),
            "const" => Some(token::TokenType::Const),
            "mut" => Some(token::TokenType::Mut),
            "true" => Some(token::TokenType::True),
            "false" => Some(token::TokenType::False),
            "match" => Some(token::TokenType::Match),
            "import" => Some(token::TokenType::Import),
            "for" => Some(token::TokenType::For),
            "in" => Some(token::TokenType::In),
            "struct" => Some(token::TokenType::Struct),
            "enum" => Some(token::TokenType::Enum),
            "Self" => Some(token::TokenType::SelfUpper),
            "self" => Some(token::TokenType::SelfLower),
            "construct" => Some(token::TokenType::Construct),
            "loop" => Some(token::TokenType::Loop),
            "await" => Some(token::TokenType::Await),
            "async" => Some(token::TokenType::Async),
            "public" => Some(token::TokenType::Public),
            "type" => Some(token::TokenType::Type),
            _ => None,
        }
    }

    fn add_token_(&mut self, token_type: token::TokenType, literal: Option<token::LiteralTypes>) {
        let lexeme = &self.source[self.start..self.current];
        let pos = self.start_pos;
        self.tokens.push(token::Token::new(
            token_type,
            lexeme.to_string(),
            literal,
            self.start_pos,
        ));
    }

    #[inline]
    fn add_token(&mut self, token_type: token::TokenType) {
        self.add_token_(token_type, None);
    }

    #[inline]
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
    fn is_alpha_num(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }
}

/// Tests for scanner
#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::*;

    fn scan_types(source: &str) -> Vec<token::TokenType> {
        let mut state = MiniState::new();
        let mut scanner = Scanner::new(source, &mut state);
        let (tokens, _) = scanner.scan_tokens();
        tokens.into_iter().map(|t| t.token_type).collect()
    }

    #[test]
    fn test_single_symbols() {
        let source = "(){},.;";
        let types = scan_types(source);
        assert_eq!(
            types,
            vec![
                token::TokenType::LParen,
                token::TokenType::RParen,
                token::TokenType::LBrace,
                token::TokenType::RBrace,
                token::TokenType::Comma,
                token::TokenType::Dot,
                token::TokenType::SemiColon,
                token::TokenType::EoF
            ]
        );
    }

    #[test]
    fn test_keywords_and_identifiers() {
        let source = "if else while func return let mini";
        let types = scan_types(source);
        assert_eq!(
            types,
            vec![
                token::TokenType::If,
                token::TokenType::Else,
                token::TokenType::While,
                token::TokenType::Func,
                token::TokenType::Return,
                token::TokenType::Let,
                token::TokenType::Identifier,
                token::TokenType::EoF
            ]
        );
    }

    #[test]
    fn test_numbers() {
        let source = "42 3.14";
        let mut state = MiniState::new();
        let mut scanner = Scanner::new(source, &mut state);
        let (tokens, _) = scanner.scan_tokens();
        assert_eq!(tokens[0].token_type, token::TokenType::IntLiteral);
        assert_eq!(tokens[1].token_type, token::TokenType::FloatLiteral);
    }

    #[test]
    fn test_string_literal() {
        let source = "\"hello world\"";
        let mut state = MiniState::new();
        let mut scanner = Scanner::new(source, &mut state);
        let (tokens, _) = scanner.scan_tokens();
        assert_eq!(tokens[0].token_type, token::TokenType::StringLiteral);
        if let Some(token::LiteralTypes::String(ref s)) = tokens[0].literal {
            assert_eq!(s, "hello world");
        } else {
            panic!("Expected string literal");
        }
    }

    #[test]
    fn test_comment_and_whitespace() {
        let source = "let // this is a comment\n x";
        let types = scan_types(source);
        assert_eq!(
            types,
            vec![
                token::TokenType::Let,
                token::TokenType::Identifier,
                token::TokenType::EoF
            ]
        );
    }

    #[test]
    fn test_unterminated_string() {
        let source = "\"unterminated";
        let mut state = MiniState::new();
        let mut scanner = Scanner::new(source, &mut state);
        let (_, had_error) = scanner.scan_tokens();
        assert!(
            had_error,
            "Scanner should set had_error for unterminated string"
        );
    }

    #[test]
    fn test_composite_operators() {
        let source = "+ += - -= * *= / /= == != < <= > >=";
        let types = scan_types(source);
        assert_eq!(
            types,
            vec![
                token::TokenType::Plus,
                token::TokenType::PlusEqual,
                token::TokenType::Minus,
                token::TokenType::MinusEqual,
                token::TokenType::Star,
                token::TokenType::StarEqual,
                token::TokenType::Slash,
                token::TokenType::SlashEqual,
                token::TokenType::EqualEqual,
                token::TokenType::BangEqual,
                token::TokenType::Less,
                token::TokenType::LessEqual,
                token::TokenType::Greater,
                token::TokenType::GreaterEqual,
                token::TokenType::EoF
            ]
        );
    }

    #[test]
    fn test_boolean_literals() {
        let source = "true false";
        let types = scan_types(source);
        assert_eq!(
            types,
            vec![
                token::TokenType::True,
                token::TokenType::False,
                token::TokenType::EoF
            ]
        );
    }
}
