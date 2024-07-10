use crate::lexer::Lexer;
use crate::token::Token;
use crate::token::Token::Eof;

pub mod lexer;
pub mod token;

pub fn lex(source: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(source);
    let mut token = lexer.next_token();
    let mut tokens = vec![];
    loop {
        tokens.push(token);
        token = lexer.next_token();
        if token == Eof {
            break;
        }
    }
    tokens
}
