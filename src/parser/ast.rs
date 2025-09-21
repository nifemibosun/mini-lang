#![allow(unused)]

use crate::scanner::token::TokenType;


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

pub enum TypeExpr {
    Named(String),
    Array(Box<TypeExpr>),
    Pointer(Box<TypeExpr>),
    Reference(Box<TypeExpr>),
    Generic(String, Vec<TypeExpr>)
}

pub enum AstNode {
    Expr(Expr),
    Stmt(Stmt),
    Decl(Decl),
}

/// Expressions always return a value
pub enum Expr {
    Literal(LiteralTypes),
    Identifier(String),
    Unary {
        operator: TokenType,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: TokenType,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },
    Member {
        object: Box<Expr>,
        property: String,
    },
}

/// Statements perform actions, but do not themselves produce a value
pub enum Stmt {
    ExprStmt(Expr),
    Let {
        name: String,
        mutable: bool,
        r#type: Option<TypeExpr>,
        initializer: Option<Expr>,
    },
    Return(Option<Expr>),
    Block(Vec<Stmt>),
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    For {
        iterator: String,
        iterable: Expr,
        body: Box<Stmt>,
    },
}

/// Declarations introduce new named entities into a scope
pub enum Decl {
    Import { module: String },
    Const {
        name: String,
        r#type: TypeExpr,
        value: Expr,
    },
    Func {
        name: String,
        params: Vec<(String, TypeExpr)>,
        return_type: Option<TypeExpr>,
        body: Vec<Stmt>,
    },
    Struct {
        name: String,
        fields: Vec<(String, TypeExpr)>,
    },
    Enum {
        name: String,
        variants: Vec<String>,
    },
    Construct {
        name: String,
        methods: Vec<Decl>,
    },
}
