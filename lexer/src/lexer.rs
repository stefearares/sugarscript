use crate::token;
use crate::token::Token;
use crate::token::Token::*;
use std::string::String;

pub struct Lexer {
    input: String,
    position: i32,      //current position in input (points to current char)
    read_position: i32, //current reading position in input (after current char)
    ch: char,           //current char under examination
    line: i32,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            ch: '\0',
            line: 1,
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        let tok;

        self.skip_whitespace();
        while self.ch == '#' {
            self.skip_comment();
            self.skip_whitespace();
        }

        match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    tok = Eq
                } else {
                    tok = Assign
                }
            }
            '+' => tok = Plus,
            '-' => {
                if self.peek_char() == '>' {
                    self.read_char();
                    tok = Arrow
                } else {
                    tok = Minus
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    tok = NotEq
                } else {
                    tok = Illegal
                }
            }
            '*' => tok = Asterisk,
            '/' => tok = Slash,
            '<' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    tok = Le
                } else {
                    tok = Lt
                }
            }
            '>' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    tok = Ge
                } else {
                    tok = Gt
                }
            }
            ',' => tok = Comma,
            ';' => tok = Semicolon,
            ':' => tok = Colon,
            '(' => tok = Lparen,
            ')' => tok = Rparen,
            '{' => tok = Lbrace,
            '}' => tok = Rbrace,
            '|' => tok = Pipe,
            '"' => {
                let literal = self.read_string();
                tok = String(literal.clone())
            }
            '\0' => tok = Eof,
            '\n' => {
                tok = Newline(self.line);
                self.line += 1;
            }
            '\r' => {
                tok = Newline(self.line);
                if self.peek_char() == '\n' {
                    self.read_char();
                }
                self.line += 1;
            }
            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    tok = token::lookup_identifier_type(literal);
                    return tok;
                } else if is_digit(self.ch) {
                    let number = self.read_number();
                    tok = Int(number);
                    return tok;
                } else {
                    tok = Illegal;
                }
            }
        }
        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() as i32 {
            self.ch = '\0'; // Null character to represent EOF
        } else {
            self.ch = self.input.chars().nth(self.read_position as usize).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_string(&mut self) -> String {
        let pos = (self.position + 1) as usize;
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == '\0' {
                break;
            }
        }
        self.input[pos..self.position as usize].to_string()
    }

    fn read_number(&mut self) -> String {
        let pos = self.position as usize;
        while is_digit(self.ch) {
            self.read_char();
        }
        self.input[pos..self.position as usize].to_string()
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position as usize;
        while is_letter(self.ch) {
            self.read_char();
        }
        self.input[pos..self.position as usize].to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' {
            self.read_char();
        }
    }

    fn skip_comment(&mut self) {
        if self.ch == '#' {
            while self.ch != '\n' && self.ch != '\r' && self.ch != '\0' {
                self.read_char();
            }
        }
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() as i32 {
            return '\0';
        }
        self.input.chars().nth(self.read_position as usize).unwrap()
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use crate::lex;
    use crate::lexer::Lexer;
    use crate::token::Token::*;

    #[test]
    fn test_assignment_string() {
        let string_assignment = r#"let binding = "something""#;
        let mut lexer = Lexer::new(string_assignment);
        let mut actual = Vec::new();
        let expected = vec![
            Let,
            Ident("binding".to_string()),
            Assign,
            String("something".to_string()),
        ];

        let mut token = lexer.next_token();
        loop {
            actual.push(token);
            token = lexer.next_token();
            if token == Eof {
                break;
            }
        }
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_assignment_integer() {
        let int_assignment = "let x = 5 + 5";
        let mut lexer = Lexer::new(int_assignment);
        let mut actual = Vec::new();
        let expected = vec![
            Let,
            Ident("x".to_string()),
            Assign,
            Int("5".into()),
            Plus,
            Int("5".into()),
        ];

        let mut token = lexer.next_token();
        loop {
            actual.push(token);
            token = lexer.next_token();
            if token == Eof {
                break;
            }
        }
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_if_condition() {
        let int_assignment = "if 5 == 5: 2";
        let mut lexer = Lexer::new(int_assignment);
        let mut actual = Vec::new();
        let expected = vec![
            If,
            Int("5".into()),
            Eq,
            Int("5".into()),
            Colon,
            Int("2".into()),
        ];

        let mut token = lexer.next_token();
        loop {
            actual.push(token);
            token = lexer.next_token();
            if token == Eof {
                break;
            }
        }
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_def_and_call() {
        let function_def_and_call = r#"
        let five = 5
        let ten = 10
        let add = fn x, y ->
            x + y
        let result = add(five, ten)"#;
        let mut lexer = Lexer::new(function_def_and_call);
        let mut actual = Vec::new();
        let expected = vec![
            Newline(1),
            Let,
            Ident("five".to_string()),
            Assign,
            Int("5".into()),
            Newline(2),
            Let,
            Ident("ten".to_string()),
            Assign,
            Int("10".into()),
            Newline(3),
            Let,
            Ident("add".to_string()),
            Assign,
            Function,
            Ident("x".to_string()),
            Comma,
            Ident("y".to_string()),
            Arrow,
            Newline(4),
            Ident("x".to_string()),
            Plus,
            Ident("y".to_string()),
            Newline(5),
            Let,
            Ident("result".to_string()),
            Assign,
            Ident("add".to_string()),
            Lparen,
            Ident("five".to_string()),
            Comma,
            Ident("ten".to_string()),
            Rparen,
        ];

        let mut token = lexer.next_token();
        loop {
            actual.push(token);
            token = lexer.next_token();
            if token == Eof {
                break;
            }
        }
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_lex_function() {
        let tokens = lex("if 5 == 5: 2");
        println!("{:?}", tokens);
    }

    #[test]
    fn test_long_numbers() {
        let test = vec![
            (Int("10000".into()), "10000"),
            (Int("2147483647".into()), "2147483647"),
            (Int("1000000000000000000".into()), "1000000000000000000"),
            (
                Int("170141183460469231731687303715884105727".into()),
                "170141183460469231731687303715884105727",
            ),
        ];

        for (expected, actual) in test {
            assert_eq!(vec![expected], lex(actual));
        }
    }

    #[test]
    fn test_igonore_comments() {
        let function_def_and_call = r#"
        #comment
        #let five = 5
        let ten = 10"#;
        let mut lexer = Lexer::new(function_def_and_call);
        let mut actual = Vec::new();
        let expected = vec![
            Newline(1),
            Newline(2),
            Newline(3),
            Let,
            Ident("ten".to_string()),
            Assign,
            Int("10".into()),
        ];

        let mut token = lexer.next_token();
        loop {
            actual.push(token);
            token = lexer.next_token();
            if token == Eof {
                break;
            }
        }
        assert_eq!(expected, actual);
    }
}
