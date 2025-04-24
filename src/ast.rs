use crate::tokens::Token;

//Visibility 
#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

//Advanced types
#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Type {
    Named(String),                      // int32, bool, MyType
    Pointer(Box<Type>),                // *int32
    Reference(Box<Type>),              // &int32
    MutableReference(Box<Type>),       // &mut int32
    Array(Box<Type>, usize),           // [int8; 4]
    Tuple(Vec<Type>),                  // (int32, float64)
    Function(Vec<Type>, Box<Type>),    // fn(int32, float64) -> bool
    Unit,                              // ()
}

//Literal Values
#[allow(unused)]
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

// Expressions
#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Variable(String),
    Unary {
        op: Token,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Assignment {
        target: Box<Expr>,   // variable, field, index, etc.
        value: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },
    Field {
        base: Box<Expr>,
        name: String,
    },
    Cast {
        expr: Box<Expr>,
        typ: Type,
    },
    AddressOf(Box<Expr>),
    Deref(Box<Expr>),
    Tuple(Vec<Expr>),
    Lambda {
        params: Vec<String>,
        body: Vec<Stmt>,
    },
}

// Patterns (used in match)
#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Literal(Literal),
    Variable(String),
    Tuple(Vec<Pattern>),
    Struct {
        name: String,
        fields: Vec<(String, Pattern)>,
    },
    EnumVariant {
        name: String,
        inner: Option<Box<Pattern>>,
    },
}

// Statements
#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        name: String,
        typ: Option<Type>,
        value: Option<Expr>,
        is_mutable: bool,
    },
    Const {
        name: String,
        typ: Type,
        value: Expr,
    },
    Static {
        name: String,
        typ: Type,
        value: Expr,
        mutable: bool,
    },
    Expression(Expr),
    Print(Expr),
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
        initializer: Option<Box<Stmt>>,
        condition: Option<Expr>,
        increment: Option<Expr>,
        body: Box<Stmt>,
    },
    Break,
    Continue,

    Struct {
        name: String,
        fields: Vec<(String, Type)>,
        visibility: Visibility,
    },
    Enum {
        name: String,
        variants: Vec<(String, Option<Type>)>,
        visibility: Visibility,
    },
    Match {
        expr: Expr,
        arms: Vec<(Pattern, Stmt)>,
    },
    Function {
        name: String,
        params: Vec<(String, Type)>,
        return_type: Option<Type>,
        body: Vec<Stmt>,
        visibility: Visibility,
    },
    ExternFunction {
        name: String,
        params: Vec<(String, Type)>,
        return_type: Option<Type>,
    },
    Trait {
        name: String,
        methods: Vec<Stmt>,
        visibility: Visibility,
    },
    Impl {
        trait_name: Option<String>,
        type_name: String,
        methods: Vec<Stmt>,
        visibility: Visibility,
    },
    Module {
        name: String,
        declarations: Vec<Stmt>,
        visibility: Visibility,
    },
    Import {
        path: Vec<String>,
    },
    UnsafeBlock(Vec<Stmt>),
}
