use crate::tokens::TokenType;

#[allow(unused)]
enum Expr {
    Binary {
        left: Box<Expr>,
        operator: TokenType,
        right: Box<Expr>
    },
    Unary {
        operator: TokenType,
        right: Box<Expr>
    }
}