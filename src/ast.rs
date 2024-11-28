// Enum for expressions
enum Expr {
    Number(i64),
    String(String),
    Variable(String),
    BinaryOp(Box<Expr>, BinOp, Box<Expr>),
    FunctionCall(String, Vec<Expr>),
}

// Enum for binary operators
enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

// Enum for statements
enum Statement {
    Expression(Expr),
    VariableDeclaration(String, Expr),
    FunctionDeclaration(String, Vec<String>, Vec<Statement>),
    IfStatement(Expr, Vec<Statement>, Vec<Statement>), // If condition, if body, else body
    Return(Expr),
}
