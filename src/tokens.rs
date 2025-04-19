
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    //single-character tokens
    RBrace, LBrace,
    RSquare, LSquare,
    RParen, LParen,
    Comma, Dot,
    Slash, Star,
    Plus, Minus,
    Ampisand,
    SemiColon, Colon, ColonColon,

    // One or two character tokens.
    Equal, EqualEqual,
    Bang, BangEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals.
    Identifier, String, Number,

    //keywords
    If, Else,
    While, For, Loop,
    Struct, Construct,
    Func, Public,
    False, True, Or, And,
    Return, Let, Const,
    SelfLower, SelfUpper,
    Enum, Type,
    Array, ArrayLiteral,


    // type keywords
    Int8, Int16, Int32, Int64, Int128, IntN,
    UInt8, UInt16, UInt32, UInt64, UInt128, UIntN,  
    Float8, Float16, Float32, Float64, Float128, FloatN,
    Boolean, Char, Str, Nil,

    //End of File
    EOF
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: Option<String>,
    line: usize,
}

#[allow(unused)]
impl Token {
    pub fn new(token_type: TokenType, lexeme: String, literal: Option<String>, line: usize) -> Self {
        Self {
            token_type,
            lexeme,
            literal,
            line,
        }
    }

    pub fn to_string(&self) -> String {
        format!("{:?} {} {:?}", self.token_type, self.lexeme, self.literal)
    }
}