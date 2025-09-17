#![allow(unused)]

#[derive(Debug, PartialEq, Clone)]
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


#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    /// "("
    LParen,
    ///")"
    RParen,
    ///"{"
    LBrace,
    ///"}"
    RBrace,
    ///"["
    LSquare,
    ///"]"
    RSquare,
    ///":"
    Colon,
    ///";"
    SemiColon,
    ///","
    Comma,
    ///"."
    Dot,
    ///"?"
    Question,
    ///"@"
    At,
    ///"#"
    Pound,
    ///"$"
    Dollar,

    ///"+"
    Plus,
    ///"-"
    Minus,
    ///"*"
    Star,
    ///"/"
    Slash,
    ///"="
    Equal,

    ///"+="
    PlusEqual,
    ///"-="
    MinusEqual,
    ///"*="
    StarEqual,
    ///"/="
    SlashEqual,
    ///"!="
    BangEqual,
    ///"=="
    EqualEqual,
    ///"<"
    Less,
    ///">"
    Greater,
    ///"<="
    LessEqual,
    ///">="
    GreaterEqual,
    ///"::"
    ColonColon,

    Identifier,
    IntLiteral,
    FloatLiteral,
    String,
    
    ///"&&"
    And,
    ///"||"
    Or,
    ///"!"
    Bang,

    ///"^"
    Ampersand,
    ///"|"
    Bar,

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
    Match,
    Import,
    For,
    In,
    Struct,
    Enum,
    SelfUpper,
    SelfLower,
    Construct,
    Loop,
    Await,
    Async,

    // Access modifier
    Public,

    // End of File
    EoF,
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}


#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub pos: Position,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, literal: Option<Literal>, pos: Position) -> Self {
        Token {
            token_type,
            lexeme,
            literal,
            pos,
        }
    }

    pub fn to_string(&self) -> String {
        format!(
            "Token {{ type: {:?}, lexeme: {}, literal: {:?}, position: {:?} }}",
            self.token_type, self.lexeme, self.literal, self.pos
        )
    }
}