#![allow(unused)]

use crate::scanner::token::{ Position, TokenType };

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

pub type Program = Vec<Node<Decl>>;

pub struct Node<T> {
    pub value: T,
    pub pos: Position,
}

pub struct FuncDecl {
    name: String,
    params: Vec<(String, TypeExpr)>,
    return_type: Option<TypeExpr>,
    body: Vec<Stmt>,
}
     
pub enum TypeExpr {
    Named(String),
    Array(Box<TypeExpr>),
    Pointer {
        mutable: bool,
        target: Box<TypeExpr>,
    },
    Generic(String, Vec<TypeExpr>)
}

/// Expressions always return a value
pub type Expr = Node<ExprKind>;

pub enum ExprKind {
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
pub type Stmt = Node<StmtKind>;

pub enum StmtKind {
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
    Func(FuncDecl),
    Struct {
        name: String,
        fields: Vec<(String, TypeExpr)>,
    },
    Enum {
        name: String,
        variants: Vec<(String, Option<TypeExpr>)>,
    },
    Construct {
        name: String,
        methods: Vec<FuncDecl>,
    },
}
