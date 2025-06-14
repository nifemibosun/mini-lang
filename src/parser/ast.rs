#![allow(unused)]
use crate::scanner::token::Token;

#[derive(Debug, Clone)]
pub enum Literal {
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

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Variable(String),
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Assignment {
        target_name: String,
        value: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Expr),
    VarDecl(VariableDecl),
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
    Loop {
        body: Box<Stmt>,
    },
}

#[derive(Debug, Clone)]
pub struct VariableDecl {
    pub name: String,
    pub is_mutable: bool,
    pub is_const: bool,
    pub var_type: Option<String>,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub name: String,
    pub param_type: String,
    pub is_mutable: bool,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub return_type: Option<String>,
    pub body: Stmt,
    pub is_async: bool,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub name: String,
    pub fields: Vec<StructField>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub field_type: String,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub name: String,
    pub variants: Vec<EnumVariant>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub associated_types: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct TraitDecl {
    pub name: String,
    pub methods: Vec<FunctionDecl>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct ConstructDecl {
    pub target_type: String,
    pub methods: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
pub struct ImportDecl {
    pub path_segments: Vec<String>,
    pub item_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Function(FunctionDecl),
    Variable(VariableDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
    Trait(TraitDecl),
    Construct(ConstructDecl),
    Import(ImportDecl),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}