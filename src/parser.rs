use crate::mini::Mini;
use crate::tokens::{ Token, TokenType };
use crate::ast::{ Expr, LiteralValue };
use std::collections::HashSet;
// use std::fs;

#[derive(Debug,Clone)]
#[allow(dead_code)]
pub enum ParseError {
    UnexpectedToken(String),
    MissingToken(String),
    InvalidExpression(String),
    EndOfInput(String),
    UnterminatedString(String),
    InvalidNumberFormat(String),
    UnexpectedCharacter(String),
    MismatchedParentheses(String),
    InvalidIdentifier(String),
    UnreachableCode(String),
    InvalidSyntax(String),
    UnexpectedInputAfterEOF(String),
    InvalidFunctionDefinition(String),
    FileNotFound(String),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            ParseError::UnexpectedToken(ref msg) => write!(f, "Unexpected Token: {}", msg),
            ParseError::MissingToken(ref msg) => write!(f, "Missing Token: {}", msg),
            ParseError::InvalidExpression(ref msg) => write!(f, "Invalid Expression: {}", msg),
            ParseError::EndOfInput(ref msg) => write!(f, "End of Input: {}", msg),
            ParseError::UnterminatedString(ref msg) => write!(f, "Unterminated String: {}", msg),
            ParseError::InvalidNumberFormat(ref msg) => write!(f, "Invalid Number Format: {}", msg),
            ParseError::UnexpectedCharacter(ref msg) => write!(f, "Unexpected Character: {}", msg),
            ParseError::MismatchedParentheses(ref msg) => write!(f, "Mismatched Parentheses: {}", msg),
            ParseError::InvalidIdentifier(ref msg) => write!(f, "Invalid Identifier: {}", msg),
            ParseError::UnreachableCode(ref msg) => write!(f, "Unreachable Code: {}", msg),
            ParseError::InvalidSyntax(ref msg) => write!(f, "Invalid Syntax: {}", msg),
            ParseError::UnexpectedInputAfterEOF(ref msg) => write!(f, "Unexpected Input After EOF: {}", msg),
            ParseError::InvalidFunctionDefinition(ref msg) => write!(f, "Invalid Function Definition: {}", msg),
            ParseError::FileNotFound(ref msg) => write!(f, "File Not Found: {}", msg),
        }
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug, Clone)]
#[allow(dead_code)]
struct Parser {
    tokens: Vec<Token>,
    current: i64,
}

#[allow(dead_code)]
impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { 
            tokens,
            current: 0
        }
    }

    // fn parse(&mut self) -> Result<(), ParseError> {
    //     while !self.is_at_end() {
    //         self.declaration()?;
    //     }
    //     Ok(())
    // }

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    // fn declaration(&mut self) -> Result<(), ParseError> {
    //     if self.match_token(vec![TokenType::Import]) {
    //         return self.import_statement();
    //     }
    //     self.statement()
    // }

    // fn import_statement(&mut self) -> Result<(), ParseError> {
    //     let token = self.advance();
    //     let file_path = match &token.literal {
    //         Some(LiteralValue::String(s)) => s.clone(),
    //         _ => return Err(ParseError::InvalidSyntax("Expected string literal".into())),
    //     };

    //     let content = fs::read_to_string(&file_path)
    //         .map_err(|_| ParseError::FileNotFound(file_path.clone()))?;

    //     let tokens = self.tokenize(&content)?;
    //     let mut parser = Parser::new(tokens);
    //     parser.parse()?;

    //     Ok(())
    // }

    fn equality(&mut self) -> Expr {
        let mut expr: Expr = self.comparison();
    
        while self.match_token(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            };
        }
    
        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr: Expr = self.term();

        while self.match_token(vec![TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
            let operator: Token = self.previous().clone();
            let right: Expr = self.term();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr: Expr = self.factor();
    
        while self.match_token(vec![TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            };
        }
    
        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr: Expr = self.unary();
    
        while self.match_token(vec![TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            };
        }
    
        expr
    }

    fn unary(&mut self) -> Expr {
        if self.match_token(vec![TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary();
            return Expr::Unary {
                operator: operator.clone(),
                right: Box::new(right),
            };
        }
      
        match self.primary() {
            Ok(expr) => expr,
            Err(err) => panic!("Error parsing primary expression: {}", err),
        }
    }

    fn primary(&mut self) -> Result<Expr, String> {
        if self.match_token(vec![TokenType::False]) {
            return Ok(Expr::Literal(LiteralValue::Boolean(false)));
        }
        if self.match_token(vec![TokenType::True]) {
            return Ok(Expr::Literal(LiteralValue::Boolean(true)));
        }
        if self.match_token(vec![TokenType::Nil]) {
            return Ok(Expr::Literal(LiteralValue::Nil));
        }
        if self.match_token(vec![TokenType::Int , TokenType::Uint, TokenType::Float, TokenType::String]) {
            if let Some(value) = &self.previous().literal {
                match self.previous().token_type {
                    TokenType::Int => {
                        if let Ok(int) = value.parse::<i64>() {
                            return Ok(Expr::Literal(LiteralValue::Int(int)));
                        } else {
                            return Err(format!("Invalid int literal: {}", value));
                        }
                    }
                    TokenType::Uint => {
                        if let Ok(uint) = value.parse::<u64>() {
                            return Ok(Expr::Literal(LiteralValue::Uint(uint)));
                        } else {
                            return Err(format!("Invalid uint literal: {}", value));
                        }
                    }
                    TokenType::Float => {
                        if let Ok(float) = value.parse::<f64>() {
                            return Ok(Expr::Literal(LiteralValue::Float(float)));
                        } else {
                            return Err(format!("Invalid float literal: {}", value));
                        }
                    }
                    TokenType::String => {
                        return Ok(Expr::Literal(LiteralValue::String(value.clone())));
                    }
                    _ => {
                        return Err("Unexpected token literal type.".to_string());
                    }
                }
            } else {
                return Err("Expected a literal value.".to_string());
            }
        }
    
        if self.match_token(vec![TokenType::LParen]) {
            let expr = self.expression();
            self.consume(TokenType::RParen, "Expect ')' after expression.")
                .map_err(|e| e.to_string())?;
            return Ok(Expr::Grouping {
                expr: Box::new(expr),
            });
        }
    
        Err(format!(
            "Expect expression. Found: {:?}",
            self.peek().token_type
        ))
    }
    

    fn match_token(&mut self, types: Vec<TokenType>) -> bool {
        let token_set: HashSet<TokenType> = types.into_iter().collect();
        if let Some(token) = self.tokens.get(self.current as usize) {
            if token_set.contains(&token.token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, expected: TokenType, message: &str) -> Result<Token, ParseError> {
        if self.check(expected) {
            Ok(self.advance())
        } else {
            Err(ParseError::UnexpectedToken(message.to_string()))
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == token_type
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous().clone()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }
    
    fn peek(&self) -> &Token {
        self.tokens
            .get(self.current as usize)
            .expect("Expected token at position.")
    }
    
    fn previous(&self) -> &Token {
        if self.current > 0 {
            self.tokens
                .get((self.current - 1) as usize)
                .expect("No previous token available.")
        } else {
            panic!("No previous token available.")
        }
    }

    fn error(token: Token, message: &str) -> ParseError {
        Mini::error(token, message);
        ParseError::UnexpectedToken(message.to_string())
    }

    fn syncronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::SemiColon {
                return;
            }
            
            match self.peek().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Let
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Int
                | TokenType::Uint
                | TokenType::Float
                | TokenType::Bool
                | TokenType::Str
                | TokenType::Object
                | TokenType::Vector
                | TokenType::Nil
                | TokenType::Return => return,
                _ => {}
            }

            self.advance();
        }
    }
}
