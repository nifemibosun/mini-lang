use crate::tokens::Token;

//Visibility 
#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct TypeConstraint {
    trait_name: String,
    trait_params: Vec<String>,
}

//Advanced types
#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Type {
    Named(String),
    Pointer(Box<Type>),
    Reference(Box<Type>),
    MutableReference(Box<Type>),
    Array(Box<Type>, usize),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Option(Box<Type>),
    Result {
        ok: Box<Type>,
        err: Box<Type>,
    },
    Generic {
        base: String,
        params: Vec<Type>
    },
    Unit,               
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
    TryExpr {
        expr: Box<Expr>,
    },
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
        type_params: Vec<String>,
        type_constraint: Vec<(String, Vec<TypeConstraint>)>
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
        type_params: Vec<String>,
        super_traits: Vec<String>,
        visibility: Visibility,
    },
    Construct {
        trait_name: Option<String>,
        type_name: String,
        type_params: Vec(String),
        type_constraints: Vec<(String, Vec<TypeConstraint>)>,
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
    TryCatch {
        try_block: Vec<Stmt>,
        catch_name: String,
        catch_type: Type,
        catch_block: Vec<Stmt>,
    }
    UnsafeBlock(Vec<Stmt>),
}
