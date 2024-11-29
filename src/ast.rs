// use std::collections::btree_map::Values;
#![allow(dead_code)]
#[derive(Debug, Clone)]
// Enum for expressions
pub enum Expr {
    Number(i64),
    String(String),
    Variable(String),
    BinaryOp(Box<Expr>, BinOp, Box<Expr>),
    FunctionCall(String, Vec<Expr>),
}

#[derive(Debug, Clone)]
// Enum for binary operators
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone)]
// Enum for statements
pub enum Statement {
    Expression(Expr),
    VariableDeclaration(String, Expr),
    FunctionDeclaration(String, Vec<String>, Vec<Statement>),
    IfStatement(Expr, Vec<Statement>, Vec<Statement>), // If condition, if body, else body
    Return(Expr),
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(i64),
    String(String),
    Function(String, Vec<String>, Vec<Statement>),
    Return(Box<Value>), // Use Box<Value> to break the recursion
    Nil,
}



impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Number(n) => *n != 0,       // Non-zero numbers are truthy
            Value::String(s) => !s.is_empty(), // Non-empty strings are truthy
            Value::Function(_, _, _) => true,   // Functions are always truthy
            Value::Return(value) => value.is_truthy(),
            Value::Nil => false,               // Nil is falsey
        }
    }
}

