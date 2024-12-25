use crate::tokens::Token;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Literal(LiteralValue),
    Grouping {
        expr: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum LiteralValue {
    Int(i64),
    Uint(u64),
    Float(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Binary { left, operator, right } => {
                write!(f, "({} {} {})", operator.lexeme, left, right)
            }
            Expr::Unary { operator, right } => {
                write!(f, "({} {})", operator.lexeme, right)
            }
            Expr::Literal(value) => match value {
                LiteralValue::Int(int) => write!(f, "{}", int),
                LiteralValue::Uint(uint) => write!(f, "{}", uint),
                LiteralValue::Float(float) => write!(f, "{}", float),
                LiteralValue::String(s) => write!(f, "\"{}\"", s),
                LiteralValue::Boolean(b) => write!(f, "{}", b),
                LiteralValue::Nil => write!(f, "nil"),
            },
            Expr::Grouping { expr } => {
                write!(f, "(group {})", expr)
            }
        }
    }
}

impl From<i64> for LiteralValue {
    fn from(val: i64) -> Self {
        LiteralValue::Int(val)
    }
}

impl From<u64> for LiteralValue {
    fn from(val: u64) -> Self {
        LiteralValue::Uint(val)
    }
}

impl From<f64> for LiteralValue {
    fn from(val: f64) -> Self {
        LiteralValue::Float(val)
    }
}

impl From<String> for LiteralValue {
    fn from(val: String) -> Self {
        LiteralValue::String(val)
    }
}

impl From<bool> for LiteralValue {
    fn from(val: bool) -> Self {
        LiteralValue::Boolean(val)
    }
}
