use crate::ast::{Expr, BinOp};
use crate::tokens::{Token, TokenType};

struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, current: 0 }
    }

    // Function to parse an expression
    fn parse_expr(&mut self) -> Expr {
        let mut expr = self.parse_term();  // Parse initial term (variable, number, etc.)

        while let Some(operator) = self.match_token(TokenType::Plus) {
            let right = self.parse_term();  // Parse the next term
            expr = Expr::BinaryOp(Box::new(expr), BinOp::Add, Box::new(right));
        }

        while let Some(operator) = self.match_token(TokenType::Minus) {
            let right = self.parse_term();  // Parse the next term
            expr = Expr::BinaryOp(Box::new(expr), BinOp::Subtract, Box::new(right));
        }

        expr
    }

    // Function to parse a statement
    fn parse_statement(&mut self) -> Statement {
        if let Some(token) = self.match_token(TokenType::Let) {
            // Parse a variable declaration: let <name> = <expression>
            let var_name = self.expect_token(TokenType::Identifier);
            self.expect_token(TokenType::Equal);
            let value = self.parse_expr();
            Statement::VariableDeclaration(var_name.lexeme.clone(), value)
        } else if let Some(token) = self.match_token(TokenType::Fun) {
            // Parse a function declaration: fun <name>(<params>) { <body> }
            let func_name = self.expect_token(TokenType::Identifier);
            self.expect_token(TokenType::LeftParen);
            let mut params = Vec::new();
            
            while let Some(param) = self.match_token(TokenType::Identifier) {
                params.push(param.lexeme.clone());
                if self.match_token(TokenType::Comma).is_none() {
                    break;
                }
            }
            
            self.expect_token(TokenType::RightParen);
            self.expect_token(TokenType::LeftBrace);
            
            let mut body = Vec::new();
            while let Some(stmt) = self.parse_statement() {
                body.push(stmt);
                if self.match_token(TokenType::RightBrace).is_some() {
                    break;
                }
            }
            
            Statement::FunctionDeclaration(func_name.lexeme.clone(), params, body)
        } else if let Some(token) = self.match_token(TokenType::If) {
            // Parse an if statement: if <condition> { <if_body> } else { <else_body> }
            self.expect_token(TokenType::LeftParen);
            let condition = self.parse_expr();
            self.expect_token(TokenType::RightParen);
            
            self.expect_token(TokenType::LeftBrace);
            let mut if_body = Vec::new();
            while let Some(stmt) = self.parse_statement() {
                if_body.push(stmt);
                if self.match_token(TokenType::RightBrace).is_some() {
                    break;
                }
            }
            
            let mut else_body = Vec::new();
            if self.match_token(TokenType::Else).is_some() {
                self.expect_token(TokenType::LeftBrace);
                while let Some(stmt) = self.parse_statement() {
                    else_body.push(stmt);
                    if self.match_token(TokenType::RightBrace).is_some() {
                        break;
                    }
                }
            }
    
            Statement::IfStatement(condition, if_body, else_body)
        } else if let Some(token) = self.match_token(TokenType::Return) {
            // Parse a return statement: return <expression>
            let value = self.parse_expr();
            Statement::Return(value)
        } else {
            // Parse an expression statement: any valid expression followed by a semicolon
            let expr = self.parse_expr();
            Statement::Expression(expr)
        }
    }

    // Function to parse terms (variables, numbers, etc.)
    fn parse_term(&mut self) -> Expr {
        let mut expr = self.parse_factor();  // Parse initial factor (like numbers or variables)

        // Now handle multiplication or division if present
        while let Some(operator) = self.match_token(TokenType::Star) {
            let right = self.parse_factor();  // Parse the next factor
            expr = Expr::BinaryOp(Box::new(expr), BinOp::Multiply, Box::new(right));
        }

        while let Some(operator) = self.match_token(TokenType::Slash) {
            let right = self.parse_factor();  // Parse the next factor
            expr = Expr::BinaryOp(Box::new(expr), BinOp::Divide, Box::new(right));
        }

        expr
    }

    // Function to parse factors (numbers, variables, parentheses)
    fn parse_factor(&mut self) -> Expr {
        if let Some(token) = self.match_token(TokenType::Number) {
            return Expr::Number(token.lexeme.parse::<i64>().unwrap());  // Convert the number to i64
        }

        if let Some(token) = self.match_token(TokenType::Identifier) {
            return Expr::Variable(token.lexeme.clone());  // Handle variables
        }

        if let Some(token) = self.match_token(TokenType::LeftParen) {
            let expr = self.parse_expr();  // Handle parentheses for grouping expressions
            self.expect_token(TokenType::RightParen);  // Expect closing parenthesis
            return expr;
        }

        // Handle error if no valid factor is found
        self.error("Expected number, identifier, or '('");
        Expr::Number(0)  // Returning a default value in case of error
    }

    // Enhanced error handling
    fn error(&self, message: &str) {
        let token = self.peek().unwrap_or_else(|| panic!("Unexpected end of input"));
        eprintln!("Error at '{}': {}", token.lexeme, message);
    }

    // Ensure a specific token is encountered (useful for assertions)
    fn expect_token(&mut self, token_type: TokenType) -> Token {
        if let Some(token) = self.match_token(token_type) {
            token
        } else {
            self.error(&format!("Expected {:?} but found {:?}", token_type, self.peek()));
            panic!("Unexpected token")
        }
    }

    // Function to handle end of input gracefully
    fn check_end_of_input(&self) {
        if let Some(token) = self.peek() {
            if token.token_type != TokenType::EndOfInput {
                self.error("Unexpected tokens after the end of input");
            }
        }
    }

    // Helper functions for matching specific tokens (e.g., checking if the current token is a certain type)
    fn match_token(&mut self, token_type: TokenType) -> Option<Token> {
        if self.check_token(token_type) {
            self.advance();
            Some(self.previous())
        } else {
            None
        }
    }

    fn check_token(&self, token_type: TokenType) -> bool {
        if let Some(token) = self.peek() {
            token.token_type == token_type
        } else {
            false
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}
