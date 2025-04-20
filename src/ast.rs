use crate::tokens::Token;

#[derive(Debug, Clone)]
pub enum Literal {
    //Integers can be either signed or unsigned
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Int128(i128),
    IntN(isize),

    //Unsigned integers
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    UInt128(u128),
    UIntN(usize),

    //Floating point numbers 
    Float32(f32),
    Float64(f64),

    //Others
    Char(char),
    String(String),
    Boolean(bool),
    Nil,
}


#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Variable(String),
    Unary {
        operator: Token,
        right: Box<Expr>
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>
    },
    Grouping(Box<Expr>),
    Assignment {
        name: String,
        content: Box<Expr>
    },
    Call(Box<Expr>, Token, Vec<Expr>)
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Expr),
    Println(Expr),
    Let(String, Option<Expr>),
    Const(String, Option<Expr>),
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
    Function {
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>
    },
    Return {
        keyword: Token,
        value: Option<Expr>,
    },
}