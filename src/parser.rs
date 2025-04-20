use crate::tokens::{ Token, TokenType };
use crate::ast::{ Expr, Stmt, Literal };

#[allow(unused)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize
}

#[allow(unused)]
impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0
        }
    }

    fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        Ok(statements)
    }

    fn is_at_end(&self) -> bool {
        self.peek() == TokenType::EOF;
    }
}