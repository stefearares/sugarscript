#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    Illegal,
    Eof,

    // identifiers
    Ident(String),
    Int(String),

    // Operators
    Assign,
    Eq,
    NotEq,
    Plus,
    Minus,
    Slash,
    Asterisk,
    Lt,
    Gt,
    Le,
    Ge,
    Arrow,

    // Delimiters
    Comma,
    Semicolon,
    Colon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Pipe,

    // Keywords
    Function,
    Let,
    Var,
    True,
    False,
    If,
    Match,
    Else,
    Return,

    Newline(i32),

    // Literal
    String(String),
}

pub fn lookup_identifier_type(identifier: String) -> Token {
    match identifier.as_str() {
        "fn" => Token::Function,
        "let" => Token::Let,
        "var" => Token::Var,
        "true" => Token::True,
        "false" => Token::False,
        "if" => Token::If,
        "else" => Token::Else,
        "match" => Token::Match,
        "return" => Token::Return,
        _ => Token::Ident(identifier),
    }
}
