#![allow(unused)]

use crate::scanner::token::{LiteralTypes, Position, TokenType};

pub type Program = Vec<Node<Decl>>;

#[derive(Debug, PartialEq, Clone)]
pub struct Node<T> {
    pub value: T,
    pub pos: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PointerKind {
    Default,
    Const,
    Mut,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeExpr {
    Named(String),
    Array {
        size: Option<usize>,
        element: Box<TypeExpr>,
    },
    Pointer {
        mutable: PointerKind,
        target: Box<TypeExpr>,
    },
    Generic(String, Vec<TypeExpr>),
}

/// Expressions always return a value
pub type Expr = Node<ExprKind>;

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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
#[derive(Debug, PartialEq, Clone)]
pub struct FuncDecl {
    pub is_public: bool,
    pub name: String,
    pub params: Vec<(String, TypeExpr)>,
    pub return_type: Option<TypeExpr>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Decl {
    Import {
        module: Vec<String>,
    },
    Const {
        is_public: bool,
        name: String,
        r#type: TypeExpr,
        value: Expr,
    },
    Func(FuncDecl),
    Struct {
        is_public: bool,
        name: String,
        fields: Vec<(String, TypeExpr)>,
    },
    Enum {
        is_public: bool,
        name: String,
        variants: Vec<(String, Option<TypeExpr>)>,
    },
    Construct {
        name: String,
        methods: Vec<FuncDecl>,
    },
}
