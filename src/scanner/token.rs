#![allow(unused)]
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
    StarEqual,
    SlashEqual,
    BangEqual,
    EqualEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,


    // Literals
    Identifier,
    Number,
    String,
    
    // Logical operators
    And,
    Or,
    Bang,

    // Bitwise operator
    BitAnd,
    BitOr,
    XNor,

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
    Async,

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
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: &'a str,
    pub literal: Option<String>,
    pub line: usize,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType, lexeme: &'a str, literal: Option<String>, line: usize,) -> Self {
        Token {
            token_type,
            lexeme,
            literal,
            line,
        }
    }

    pub fn to_string(&self) -> String {
        format!(
            "Token {{ type: {:?}, lexeme: {}, literal: {:?}, line: {} }}",
            self.token_type, self.lexeme, self.literal, self.line,
        )
    }
}