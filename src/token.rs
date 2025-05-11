#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // Simple tokens
    LParen,
    RParen,
    LBrace,
    RBrace,
    LSquare,
    RSquare,
    Comma,
    Colon,
    SemiColon,
    Dot,

    // multi tokens
    ColonColon,
    

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    PlusEqual,
    MinusEqual,
    BangEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,


    // Literals
    Identifier(String),
    Number(f64),
    String(String),
    
    // Binary operators
    And,
    Or,
    Bang,

    // Keywords
    If,
    Else,
    While,
    Func,
    Return,
    Let,
    Const,
    Mut,
    True,
    False,
    PrintLn,
    Match,
    Import,
    For,
    In,
    Struct,
    Enum,
    Trait,
    SelfUpper,
    SelfLower,
    Construct,
    Loop,
    Await,




    // Type keywords
    Int8,
    Int16,
    Int32,
    Int64,
    IntN,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    UIntN,
    Float32,
    Float64,
    StringType,
    Bool,
    Char,

    // Access modifiers
    Public,
    Private,

    // Data structures
    Array,
    Tuple,
   
    //End of file
    EOF
}


#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize, column: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            column,
        }
    }

    pub fn to_string(&self) -> String {
        format!(
            "Token {{ type: {:?}, lexeme: {}, line: {}, column: {} }}",
            self.token_type, self.lexeme, self.line, self.column
        )
    }
}