use crate::tokens::Token;

#[derive(Debug, Clone)]
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
pub enum LiteralValue {
    Number(f64),
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
                LiteralValue::Number(num) => write!(f, "{}", num),
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

impl From<f64> for LiteralValue {
    fn from(val: f64) -> Self {
        LiteralValue::Number(val)
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
