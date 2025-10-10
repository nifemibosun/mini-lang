#![allow(unused)]

pub mod ast;

use crate::{
    parser::ast::Node,
    scanner::token::{LiteralTypes, Token, TokenType},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    /// Construct a new parser instance
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    /// The main parse method on every parser instance
    pub fn parse(&mut self) -> Result<ast::Program, String> {
        let mut program = Vec::new();

        while !self.is_at_end() {
            let decl = self.declaration()?;
            program.push(decl);
        }

        Ok(program)
    }

    fn expression(&mut self) -> Result<ast::Expr, String> {
        self.parse_precedence(0)
    }

    fn statement(&mut self) -> Result<ast::Stmt, String> {
        if self.match_token(&[TokenType::Let]) {
            return self.let_stmt();
        } else if self.match_token(&[TokenType::Return]) {
            return self.return_stmt();
        } else if self.match_token(&[TokenType::LBrace]) {
            return self.block_stmt();
        } else if self.match_token(&[TokenType::If]) {
            return self.if_stmt();
        } else if self.match_token(&[TokenType::While]) {
            return self.while_stmt();
        } else if self.match_token(&[TokenType::For]) {
            return self.for_stmt();
        } else {
            return self.expression_stmt();
        }
    }

    fn let_stmt(&mut self) -> Result<ast::Stmt, String> {
        let s_pos = self.previous().pos;
        let mutable = self.match_token(&[TokenType::Mut]);

        let t_name = self.consume(TokenType::Identifier, "Expected variable name after 'let'")?;
        let name = t_name.lexeme;

        let r#type = if self.match_token(&[TokenType::Colon]) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let initializer = if self.match_token(&[TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        if r#type.is_none() && initializer.is_none() {
            return Err(format!(
                "'let' declaration must have an initializer or an explicit type at {:?}",
                s_pos
            ));
        }

        self.consume(
            TokenType::SemiColon,
            "Expected ';' after variable declaration",
        )?;

        Ok(ast::Node {
            value: ast::StmtKind::Let {
                name,
                mutable,
                r#type,
                initializer,
            },
            pos: s_pos,
        })
    }

    fn return_stmt(&mut self) -> Result<ast::Stmt, String> {
        let s_pos = self.previous().pos;

        let expr = if self.check(TokenType::SemiColon) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(TokenType::SemiColon, "Expected ';' after return value")?;

        Ok(ast::Node {
            value: ast::StmtKind::Return(expr),
            pos: s_pos,
        })
    }

    fn block_stmt(&mut self) -> Result<ast::Stmt, String> {
        let s_pos = self.previous().pos;
        let mut statements = Vec::new();

        while !self.check(TokenType::RBrace) && !self.is_at_end() {
            statements.push(self.statement()?);
        }

        self.consume(TokenType::RBrace, "Expected '}' after block")?;

        Ok(ast::Node {
            value: ast::StmtKind::Block(statements),
            pos: s_pos,
        })
    }

    fn if_stmt(&mut self) -> Result<ast::Stmt, String> {
        let s_pos = self.previous().pos;

        if self.check(TokenType::LParen) {
            return Err(format!(
                "'(' is not expected before if condition at {:?}",
                self.peek().pos
            ));
        }

        let condition = self.expression()?;

        if self.check(TokenType::RParen) {
            return Err(format!(
                "')' is not expected after if condition at {:?}",
                self.peek().pos
            ));
        }

        self.consume(TokenType::LBrace, "Expected '{' after if condition")?;
        let then_branch = self.block_stmt()?;

        let else_branch = if self.match_token(&[TokenType::Else]) {
            self.consume(TokenType::LBrace, "Expected '{' after 'else'")?;
            Some(self.block_stmt()?)
        } else {
            None
        };

        Ok(ast::Node {
            value: ast::StmtKind::If {
                condition,
                then_branch: Box::new(then_branch),
                else_branch: else_branch.map(Box::new),
            },
            pos: s_pos,
        })
    }

    fn while_stmt(&mut self) -> Result<ast::Stmt, String> {
        let s_pos = self.previous().pos;

        if self.check(TokenType::LParen) {
            return Err(format!(
                "'(' is not expected before while condition at {:?}",
                self.peek().pos
            ));
        }
        
        let condition = self.expression()?;

        if self.check(TokenType::RParen) {
            return Err(format!(
                "')' is not expected after while condition at {:?}",
                self.peek().pos
            ));
        }

        self.consume(TokenType::LBrace, "Expected '{' after while condition")?;
        let body = self.block_stmt()?;

        Ok(ast::Node {
            value: ast::StmtKind::While {
                condition,
                body: Box::new(body),
            },
            pos: s_pos,
        })
    }

    fn for_stmt(&mut self) -> Result<ast::Stmt, String> {
        let s_pos = self.previous().pos;

        let iterator_token = self.consume(
            TokenType::Identifier,
            "Expected iterator name in 'for' loop",
        )?;
        let iterator = iterator_token.lexeme;

        self.consume(TokenType::In, "Expected 'in' keyword in 'for' loop")?;

        let iterable = self.expression()?;

        self.consume(TokenType::LBrace, "Expected '{' before 'for' body")?;
        let body = self.block_stmt()?;

        Ok(ast::Node {
            value: ast::StmtKind::For {
                iterator,
                iterable,
                body: Box::new(body),
            },
            pos: s_pos,
        })
    }

    fn expression_stmt(&mut self) -> Result<ast::Stmt, String> {
        let expr = self.expression()?;
        let s_pos = expr.pos.clone();

        self.consume(
            TokenType::SemiColon,
            "Expected ';' after expression statement",
        )?;

        Ok(ast::Node {
            value: ast::StmtKind::ExprStmt(expr),
            pos: s_pos,
        })
    }

    pub fn declaration(&mut self) -> Result<ast::Node<ast::Decl>, String> {
        let is_public = self.match_token(&[TokenType::Public]);

        if self.match_token(&[TokenType::Import]) {
            return self.import_decl();
        } else if self.match_token(&[TokenType::Const]) {
            return self.const_decl(is_public);
        } else if self.match_token(&[TokenType::Func]) {
            return self.function_decl(is_public);
        } else if self.match_token(&[TokenType::Struct]) {
            return self.struct_decl(is_public);
        } else if self.match_token(&[TokenType::Enum]) {
            return self.enum_decl(is_public);
        } else if self.match_token(&[TokenType::Construct]) {
            return self.construct_decl();
        }

        Err(format!(
            "Unexpected token {:?} at {:?}",
            self.peek().token_type,
            self.peek().pos
        ))
    }

    fn import_decl(&mut self) -> Result<ast::Node<ast::Decl>, String> {
        let s_pos = self.previous().pos;
        let mut path = Vec::new();

        loop {
            let t_name =
                self.consume(TokenType::Identifier, "Expected identifier after 'import'")?;
            path.push(t_name.lexeme);

            if self.match_token(&[TokenType::ColonColon]) {
                continue;
            }

            if self.check(TokenType::SemiColon) {
                break;
            }
        }

        self.consume(
            TokenType::SemiColon,
            "Expected ';' after import declaration",
        );

        Ok(ast::Node {
            value: ast::Decl::Import { module: path },
            pos: s_pos,
        })
    }

    fn const_decl(&mut self, is_public: bool) -> Result<ast::Node<ast::Decl>, String> {
        let s_pos = self.previous().pos;
        let t_name = self.consume(TokenType::Identifier, "Expected identifier after 'const'")?;

        self.consume(TokenType::Colon, "Expected ':' after const name");

        let t_type = self.parse_type()?;

        self.consume(TokenType::Equal, "Expected '=' after token type");
        let value = self.expression()?;

        self.consume(TokenType::SemiColon, "Expected ';' after const declaration");

        Ok(ast::Node {
            value: ast::Decl::Const {
                is_public,
                name: t_name.lexeme,
                r#type: t_type,
                value: value,
            },
            pos: s_pos,
        })
    }

    fn function_decl(&mut self, is_public: bool) -> Result<ast::Node<ast::Decl>, String> {
        let s_pos = self.previous().pos;
        let t_name = self.consume(TokenType::Identifier, "Expected identifier after 'func'")?;

        self.consume(TokenType::LParen, "Expected '(' after function name");

        let mut params = Vec::new();

        if !self.check(TokenType::RParen) {
            loop {
                let p_name = self
                    .consume(TokenType::Identifier, "Expected parameter name")?
                    .lexeme;
                self.consume(TokenType::Colon, "Expected ':' after parameter name");
                let p_type = self.parse_type()?;

                params.push((p_name, p_type));

                if self.match_token(&[TokenType::Comma]) {
                    continue;
                } else {
                    break;
                }
            }
        }

        self.consume(TokenType::RParen, "Expected ')' after function name");

        let return_type = if self.match_token(&[TokenType::Colon]) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.consume(TokenType::LBrace, "Expected '{' before function body");
        let body_stmt = self.block_stmt()?;

        let body = match body_stmt.value {
            ast::StmtKind::Block(stmts) => stmts,
            other => {
                return Err(format!(
                    "Expected function body to be a block at {:?}, found {:?}",
                    body_stmt.pos, other
                ));
            }
        };

        let func_decl = ast::FuncDecl {
            is_public,
            name: t_name.lexeme,
            params,
            return_type,
            body,
        };

        Ok(ast::Node {
            value: ast::Decl::Func(func_decl),
            pos: s_pos,
        })
    }

    fn struct_decl(&mut self, is_public: bool) -> Result<ast::Node<ast::Decl>, String> {
        let s_pos = self.previous().pos;
        let t_name = self.consume(TokenType::Identifier, "Expected struct name after 'struct'")?;
        let name = t_name.lexeme;

        self.consume(TokenType::LBrace, "Expected '{' before struct fields")?;

        let mut fields = Vec::new();
        while !self.check(TokenType::RBrace) && !self.is_at_end() {
            let f_name = self.consume(TokenType::Identifier, "Expected field name")?;

            self.consume(TokenType::Colon, "Expected ':' after field name")?;

            let f_type = self.parse_type()?;
            fields.push((f_name.lexeme, f_type));

            self.consume(TokenType::Comma, "Expected ',' after field declaration")?;
        }

        self.consume(TokenType::RBrace, "Expected '}' after struct body")?;

        Ok(ast::Node {
            value: ast::Decl::Struct {
                is_public,
                name,
                fields,
            },
            pos: s_pos,
        })
    }

    fn enum_decl(&mut self, is_public: bool) -> Result<ast::Node<ast::Decl>, String> {
        let s_pos = self.previous().pos;
        let t_name = self.consume(TokenType::Identifier, "Expected enum name after 'enum'")?;
        let name = t_name.lexeme;

        self.consume(TokenType::LBrace, "Expected '{' before enum variants")?;

        let mut variants = Vec::new();
        while !self.check(TokenType::RBrace) && !self.is_at_end() {
            let v_name = self.consume(TokenType::Identifier, "Expected variant name")?;

            let v_type = if self.match_token(&[TokenType::LParen]) {
                let t = self.parse_type()?;
                self.consume(TokenType::RParen, "Expected ')' after variant type")?;
                Some(t)
            } else {
                None
            };

            variants.push((v_name.lexeme, v_type));

            if self.match_token(&[TokenType::Comma]) {
                continue;
            } else if self.check(TokenType::RBrace) {
                break;
            } else {
                return Err(format!(
                    "Expected ',' or '}}' after enum variant at {:?}",
                    self.peek().pos
                ));
            }
        }

        self.consume(TokenType::RBrace, "Expected '}' after enum body")?;

        Ok(ast::Node {
            value: ast::Decl::Enum {
                is_public,
                name,
                variants,
            },
            pos: s_pos,
        })
    }

    fn construct_decl(&mut self) -> Result<ast::Node<ast::Decl>, String> {
        let s_pos = self.previous().pos;
        let t_name = self.consume(
            TokenType::Identifier,
            "Expected type name after 'construct'",
        )?;
        let name = t_name.lexeme;

        self.consume(TokenType::LBrace, "Expected '{' before construct methods")?;

        let mut methods = Vec::new();
        while self.check(TokenType::Public) || self.check(TokenType::Func) {
            let is_public = self.match_token(&[TokenType::Public]);
            self.consume(TokenType::Func, "Expected 'func' after 'public'")?;
            let func_node = self.function_decl(is_public)?;

            if let ast::Decl::Func(func_decl) = func_node.value {
                methods.push(func_decl);
            } else {
                unreachable!(
                    "Internal logic error: function_decl_internal did not return a FuncDecl"
                );
            }
        }

        self.consume(TokenType::RBrace, "Expected '}' after construct body")?;

        Ok(ast::Node {
            value: ast::Decl::Construct { name, methods },
            pos: s_pos,
        })
    }

    fn advance(&mut self) -> Token {
        let t = self.peek();
        self.current += 1;
        t
    }

    fn parse_prefix(&mut self) -> Result<ast::Expr, String> {
        if self.match_token(&[TokenType::Minus, TokenType::Bang]) {
            let operator = self.previous().token_type;
            let right = self.parse_prefix()?;
            let pos = right.pos.clone();

            return Ok(ast::Node {
                value: ast::ExprKind::Unary {
                    operator,
                    right: Box::new(right),
                },
                pos,
            });
        }

        self.parse_primary()
    }

    fn parse_precedence(&mut self, min_prec: u8) -> Result<ast::Expr, String> {
        let mut left = self.parse_prefix()?;

        while let Some(op) = self.current_operator() {
            let prec = self.precedence(&op);
            if prec < min_prec {
                break;
            }

            let operator = self.advance().token_type;
            let mut right = self.parse_precedence(prec + 1)?;

            left = ast::Node {
                pos: left.pos.clone(),
                value: ast::ExprKind::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
            };
        }

        Ok(left)
    }

    fn parse_primary(&mut self) -> Result<ast::Expr, String> {
        let token = self.advance();

        let mut node = match token.token_type {
            TokenType::IntLiteral => {
                let value = token
                    .lexeme
                    .parse::<isize>()
                    .map_err(|_| format!("Invalid integer literal at {:?}", token.pos))?;
                ast::Node {
                    value: ast::ExprKind::Literal(LiteralTypes::Int(value)),
                    pos: token.pos,
                }
            }
            TokenType::FloatLiteral => {
                let value = token
                    .lexeme
                    .parse::<f64>()
                    .map_err(|_| format!("Invalid float literal at {:?}", token.pos))?;
                ast::Node {
                    value: ast::ExprKind::Literal(LiteralTypes::Float(value)),
                    pos: token.pos,
                }
            }
            TokenType::StringLiteral => ast::Node {
                value: ast::ExprKind::Literal(LiteralTypes::String(token.lexeme)),
                pos: token.pos,
            },
            TokenType::Identifier => ast::Node {
                value: ast::ExprKind::Identifier(token.lexeme),
                pos: token.pos,
            },
            TokenType::LParen => {
                let expr = self.expression()?;
                self.consume(TokenType::RParen, "Expected ')' after expression")?;
                ast::Node {
                    value: ast::ExprKind::Grouping(Box::new(expr)),
                    pos: token.pos,
                }
            }
            _ => return Err(format!("Unexpected token {:?} in expression", token)),
        };

        while !self.is_at_end() {
            if self.check(TokenType::LParen) {
                self.advance();
                node = self.parse_call(node)?;
            } else if self.check(TokenType::LSquare) {
                self.advance();
                node = self.parse_index(node)?;
            } else if self.check(TokenType::Dot) {
                self.advance();
                node = self.parse_member(node)?;
            } else {
                break;
            }
        }

        Ok(node)
    }

    fn parse_call(&mut self, callee: ast::Expr) -> Result<ast::Expr, String> {
        let s_pos = callee.pos.clone();
        let mut arguments = Vec::new();

        if !self.check(TokenType::RParen) {
            loop {
                arguments.push(self.expression()?);
                if self.match_token(&[TokenType::Comma]) {
                    continue;
                } else {
                    break;
                }
            }
        }

        let rparen = self.consume(TokenType::RParen, "Expected ')' after function arguments")?;
        let end_pos = rparen.pos;

        Ok(ast::Node {
            value: ast::ExprKind::Call {
                callee: Box::new(callee),
                arguments,
            },
            pos: s_pos,
        })
    }

    fn parse_index(&mut self, target: ast::Expr) -> Result<ast::Expr, String> {
        let s_pos = target.pos.clone();

        let index = self.expression()?;

        self.consume(TokenType::RSquare, "Expected ']' after index expression")?;

        Ok(ast::Node {
            value: ast::ExprKind::Index {
                target: Box::new(target),
                index: Box::new(index),
            },
            pos: s_pos,
        })
    }

    fn parse_member(&mut self, object: ast::Expr) -> Result<ast::Expr, String> {
        let s_pos = object.pos.clone();

        let t_property = self.consume(TokenType::Identifier, "Expected property name after '.'")?;
        let property = t_property.lexeme;

        Ok(ast::Node {
            value: ast::ExprKind::Member {
                object: Box::new(object),
                property,
            },
            pos: s_pos,
        })
    }

    fn current_operator(&self) -> Option<TokenType> {
        match self.peek().token_type {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Star
            | TokenType::Slash
            | TokenType::EqualEqual
            | TokenType::BangEqual
            | TokenType::Less
            | TokenType::LessEqual
            | TokenType::Greater
            | TokenType::GreaterEqual => Some(self.peek().token_type.clone()),
            _ => None,
        }
    }

    fn precedence(&self, op: &TokenType) -> u8 {
        match op {
            TokenType::Star | TokenType::Slash => 10,
            TokenType::Plus | TokenType::Minus => 9,
            TokenType::EqualEqual
            | TokenType::BangEqual
            | TokenType::Less
            | TokenType::LessEqual
            | TokenType::Greater
            | TokenType::GreaterEqual => 5,
            _ => 0,
        }
    }

    fn consume(&mut self, t_type: TokenType, msg: &str) -> Result<Token, String> {
        if self.check(t_type) {
            Ok(self.advance())
        } else {
            Err(format!("{} at {:?}", msg, self.peek().pos))
        }
    }

    fn previous(&mut self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EoF
    }

    /// Check if a token has a specific type that is needed
    fn check(&mut self, t_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().token_type == t_type
    }

    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for t_type in types {
            if self.check(*t_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn parse_type(&mut self) -> Result<ast::TypeExpr, String> {
        if self.match_token(&[TokenType::Star]) {
            let mutable = if self.match_token(&[TokenType::Mut]) {
                ast::PointerKind::Mut
            } else if self.match_token(&[TokenType::Const]) {
                ast::PointerKind::Const
            } else {
                ast::PointerKind::Default
            };

            let target = self.parse_type()?;

            return Ok(ast::TypeExpr::Pointer {
                mutable,
                target: Box::new(target),
            });
        }

        if self.match_token(&[TokenType::LSquare]) {
            let size = if self.check(TokenType::IntLiteral) {
                let size_token = self.advance();
                Some(
                    size_token
                        .lexeme
                        .parse::<usize>()
                        .map_err(|_| format!("Invalid array size at {:?}", size_token.pos))?,
                )
            } else {
                None
            };

            self.consume(TokenType::RSquare, "Expected ']' after array size")?;

            let element = self.parse_type()?;
            return Ok(ast::TypeExpr::Array {
                size,
                element: Box::new(element),
            });
        }

        let ident = self
            .consume(TokenType::Identifier, "Expected type name")?
            .lexeme;

        if self.match_token(&[TokenType::Less]) {
            let mut args = Vec::new();
            loop {
                args.push(self.parse_type()?);
                if self.match_token(&[TokenType::Comma]) {
                    continue;
                }
                self.consume(TokenType::Greater, "Expected '>' after generic args")?;
                break;
            }
            return Ok(ast::TypeExpr::Generic(ident, args));
        }

        Ok(ast::TypeExpr::Named(ident))
    }
}
