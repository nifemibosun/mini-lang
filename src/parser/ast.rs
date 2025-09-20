#![allow(unused)]

use crate::scanner::{token::TokenType, Scanner};


pub enum LiteralTypes {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    IntN(isize),
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    UIntN(usize),
    Float32(f32),
    Float64(f64),
    String(String),
    Bool(bool),
    Char(char),
}

pub enum AstNode {
    Expr(Expr),
    Stmt(Stmt),
    Decl(Decl),
}


pub enum Expr {
    Literal(LiteralTypes),
    Unary {
        operator: TokenType,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: TokenType,
        right: Box<Expr>,
    }
}


pub enum Stmt {

}


pub enum Decl {

}