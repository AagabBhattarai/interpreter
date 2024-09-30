use crate::error;
use std::io::{self, Write};
use std::{fmt, fs};

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum TokenType {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,
    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    EOF,
}
// impl From<&str> for TokenType {
//     fn from(value: &str) -> Self {
//         let keyword_offset = TokenType::AND;
//         for (i, keyword) in KEYWORDS.iter().enumerate() {
//             if keyword == value {
//                 return (keyword_offset + i)
//             }
//         }
//     }
// }
impl From<&str> for TokenType {
    fn from(value: &str) -> Self {
        match value {
            "and" => TokenType::AND,
            "class" => TokenType::CLASS,
            "else" => TokenType::ELSE,
            "false" => TokenType::FALSE,
            "fun" => TokenType::FUN,
            "for" => TokenType::FOR,
            "if" => TokenType::IF,
            "nil" => TokenType::NIL,
            "or" => TokenType::OR,
            "print" => TokenType::PRINT,
            "return" => TokenType::RETURN,
            "super" => TokenType::SUPER,
            "this" => TokenType::THIS,
            "true" => TokenType::TRUE,
            "var" => TokenType::VAR,
            "while" => TokenType::WHILE,
            _ => TokenType::IDENTIFIER,  // Default to IDENTIFIER for unknown values
        }
    }
}
const KEYWORDS: [&str; 16] = [
    "and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print", "return", "super",
    "this", "true", "var", "while",
];

#[derive(Debug)]
enum Literal {
    Text(String),
    Number(f64),
}
impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Text(t) => {
                write!(f, "{}", t)
            }
            Literal::Number(n) => {
                if n.fract().abs() < f64::EPSILON {
                    write!(f, "{:.1}", n)
                } else {
                    write!(f, "{n}")
                }
            }
        }
    }
}
#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: Literal,
    line: usize,
}

impl Token {
    fn new(token_type: TokenType, lexeme: String, literal: Literal, line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            literal,
            line,
        }
    }

    fn print_info(&self) {
        println!("{:?}", self);
    }
}

pub struct Location {
    start_token: usize,
    token_offset: usize, //token_offset is same as current as per Robert Nystorm
    line: usize,
}
pub struct Scanner {
    instruction_stream: String,
    token_stream: Vec<Token>,
    had_error: bool,
    location: Location,
    error_stream: Vec<error::Errors>,
}

impl Scanner {
    pub fn new(filename: &str) -> Self {
        let file_content = fs::read_to_string(filename).unwrap_or_else(|_| {
            writeln!(
                io::stderr(),
                "Error in accessing the file in path {}",
                filename
            )
            .unwrap();
            String::new()
        });

        let location = Location {
            start_token: 0,
            token_offset: 0,
            line: 1,
        };
        // let ts: Vec<Token> = Vec::new();
        Self {
            instruction_stream: file_content,
            token_stream: Vec::new(),
            had_error: false,
            location,
            error_stream: Vec::new(),
        }
    }

    pub fn run_scan(&mut self) {
        while !self.is_at_stream_end() {
            self.location.start_token = self.location.token_offset;
            self.scan_token();
        }
        let eof_token = Token::new(
            TokenType::EOF,
            String::new(),
            Literal::Text("null".to_string()),
            self.location.line,
        );
        self.token_stream.push(eof_token);
    }

    fn scan_token(&mut self) {
        let char = self.read_char();
        match char {
            b'(' => self.add_token(TokenType::LEFT_PAREN, Literal::Text("null".to_string())),
            b')' => self.add_token(TokenType::RIGHT_PAREN, Literal::Text("null".to_string())),
            b'{' => self.add_token(TokenType::LEFT_BRACE, Literal::Text("null".to_string())),
            b'}' => self.add_token(TokenType::RIGHT_BRACE, Literal::Text("null".to_string())),
            b',' => self.add_token(TokenType::COMMA, Literal::Text("null".to_string())),
            b'.' => self.add_token(TokenType::DOT, Literal::Text("null".to_string())),
            b'-' => self.add_token(TokenType::MINUS, Literal::Text("null".to_string())),
            b'+' => self.add_token(TokenType::PLUS, Literal::Text("null".to_string())),
            b';' => self.add_token(TokenType::SEMICOLON, Literal::Text("null".to_string())),
            b'*' => self.add_token(TokenType::STAR, Literal::Text("null".to_string())),
            b'=' => match self.expected_char(b'=') {
                true => self.add_token(TokenType::EQUAL_EQUAL, Literal::Text("null".to_string())),
                false => self.add_token(TokenType::EQUAL, Literal::Text("null".to_string())),
            },
            b'!' => match self.expected_char(b'=') {
                true => self.add_token(TokenType::BANG_EQUAL, Literal::Text("null".to_string())),
                false => self.add_token(TokenType::BANG, Literal::Text("null".to_string())),
            },
            b'<' => match self.expected_char(b'=') {
                true => self.add_token(TokenType::LESS_EQUAL, Literal::Text("null".to_string())),
                false => self.add_token(TokenType::LESS, Literal::Text("null".to_string())),
            },
            b'>' => match self.expected_char(b'=') {
                true => self.add_token(TokenType::GREATER_EQUAL, Literal::Text("null".to_string())),
                false => self.add_token(TokenType::GREATER, Literal::Text("null".to_string())),
            },
            b'/' => match self.expected_char(b'/') {
                true => {
                    while self.peek() != b'\n' && !self.is_at_stream_end() {
                        _ = self.read_char();
                    }
                }
                false => self.add_token(TokenType::SLASH, Literal::Text("null".to_string())),
            },
            b'"' => {
                let mut literal = String::new();
                while self.peek() != b'\n' && !self.is_at_stream_end() && self.peek() != b'"' {
                    literal.push(self.read_char() as char);
                }
                if self.peek() != b'"' {
                    let a = format!("[line {0}] Error: Unterminated string.", self.location.line,);
                    self.error_stream.push(error::Errors::LexicalError(a, 65));
                    self.had_error = true;
                } else {
                    _ = self.read_char();
                    self.add_token(TokenType::STRING, Literal::Text(literal));
                }
            }
            b'0'..=b'9' => {
                while self.peek().is_ascii_digit() {
                    _ = self.read_char();
                }
                if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
                    _ = self.read_char();
                }
                while self.peek().is_ascii_digit() {
                    _ = self.read_char();
                }

                let lexume = std::str::from_utf8(
                    &self.instruction_stream.as_bytes()
                        [self.location.start_token..self.location.token_offset],
                )
                .unwrap_or("Can't convert bytes to str");
                self.add_token(
                    TokenType::NUMBER,
                    Literal::Number(
                        lexume
                            .parse::<f64>()
                            .expect("Can't convert str to floating-point"),
                    ),
                );
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                while self.peek().is_ascii_alphabetic()
                    || self.peek().is_ascii_digit()
                    || self.peek() == b'_'
                {
                    _ = self.read_char();
                }

                let lexume = std::str::from_utf8(
                    &self.instruction_stream.as_bytes()
                        [self.location.start_token..self.location.token_offset],
                )
                .unwrap_or("Can't convert bytes to str");

                if KEYWORDS.contains(&lexume) {
                    let token_type = TokenType::from(lexume);
                    self.add_token(token_type, Literal::Text("null".to_string()));
                    return
                }
                self.add_token(TokenType::IDENTIFIER, Literal::Text("null".to_string()));
            }
            b' ' | b'\r' | b'\t' => (),

            b'\n' => self.get_to_next_line(),
            k => {
                let a = format!(
                    "[line {0}] Error: Unexpected character: {1}",
                    self.location.line,
                    std::str::from_utf8(&[k]).unwrap()
                );
                self.error_stream.push(error::Errors::LexicalError(a, 65));
                self.had_error = true;
            }
        }
    }
    fn get_to_next_line(&mut self) {
        self.location.line += 1;
    }
    fn peek(&self) -> u8 {
        if self.is_at_stream_end() {
            return b'\0';
        }
        let offset = self.location.token_offset;
        let current_char = self.instruction_stream.as_bytes()[offset];
        current_char
    }
    fn peek_next(&self) -> u8 {
        if self.location.token_offset + 1 >= self.instruction_stream.len() {
            return b'\0';
        }
        let offset = self.location.token_offset + 1;
        let current_char = self.instruction_stream.as_bytes()[offset];
        current_char
    }
    fn expected_char(&mut self, ec: u8) -> bool {
        if self.is_at_stream_end() {
            return false;
        }
        let offset = self.location.token_offset;
        let current_char = self.instruction_stream.as_bytes()[offset];
        if current_char != ec {
            return false;
        }

        self.location.token_offset += 1;
        true
    }
    fn add_token(&mut self, t: TokenType, literal: Literal) {
        let start = self.location.start_token;
        let eater_offset = self.location.token_offset;
        let line = self.location.line;

        let lexeme_ref = &self.instruction_stream.as_bytes()[start..eater_offset];
        let lexeme = std::str::from_utf8(lexeme_ref).unwrap().to_string();

        let token = Token::new(t, lexeme, literal, line);
        self.token_stream.push(token);
    }

    fn read_char(&mut self) -> u8 {
        let offset = self.location.token_offset;
        let current_char = self.instruction_stream.as_bytes()[offset];
        self.location.token_offset += 1;
        current_char
    }

    fn is_at_stream_end(&self) -> bool {
        self.location.token_offset >= self.instruction_stream.len()
    }

    fn print_streams(&self) {
        println!("Instruction Stream:\n {}", self.instruction_stream)
    }

    pub fn print_token(&self) {
        for token in self.token_stream.iter() {
            println!(
                "{0:?} {1} {2}",
                token.token_type, token.lexeme, token.literal
            );
        }
    }
    pub fn had_error(&self) -> bool {
        self.had_error
    }
    pub fn print_error_stream(&self) -> u8 {
        let mut e_code: u8 = 0;
        for error in self.error_stream.iter() {
            match error {
                error::Errors::LexicalError(msg, code) => {
                    writeln!(io::stderr(), "{}", msg).unwrap();
                    e_code = *code;
                }
            }
        }
        e_code
    }
}

// fn main() {
//     let a = Scanner::new("test.lox");
//     a.print_streams();
// }
