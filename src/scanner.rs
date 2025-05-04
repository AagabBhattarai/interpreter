use crate::error;
use std::io::{self, Write};
use std::{fmt, fs};

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
            _ => TokenType::IDENTIFIER, // won't happen
        }
    }
}
const KEYWORDS: [&str; 16] = [
    "and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print", "return", "super",
    "this", "true", "var", "while",
];

#[derive(Debug, Clone)]
pub enum Literal {
    Text(String),
    Number(f64),
}
impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Text(t) => write!(f, "{}", t),
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
#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    lexeme: String,
    pub literal: Option<Literal>,
}
impl Token {
    fn new(token_type: TokenType, lexeme: String, literal: Option<Literal>) -> Self {
        Token {
            token_type,
            lexeme,
            literal,
        }
    }
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{0:?} {1} {2}",
            self.token_type,
            self.lexeme,
            match &self.literal {
                Some(l) => l.to_string(),
                None => "null".to_string(),
            }
        )
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

    pub fn run_scan(&mut self) -> Vec<Token> {
        while !self.is_at_stream_end() {
            self.location.start_token = self.location.token_offset;
            self.scan_token();
        }
        let eof_token = Token::new(TokenType::EOF, String::new(), None);
        self.token_stream.push(eof_token);
        self.token_stream.clone()
    }

    fn scan_token(&mut self) {
        let char = self.read_char();
        match char {
            b'(' => self.add_token(TokenType::LEFT_PAREN, None),
            b')' => self.add_token(TokenType::RIGHT_PAREN, None),
            b'{' => self.add_token(TokenType::LEFT_BRACE, None),
            b'}' => self.add_token(TokenType::RIGHT_BRACE, None),
            b',' => self.add_token(TokenType::COMMA, None),
            b'.' => self.add_token(TokenType::DOT, None),
            b'-' => self.add_token(TokenType::MINUS, None),
            b'+' => self.add_token(TokenType::PLUS, None),
            b';' => self.add_token(TokenType::SEMICOLON, None),
            b'*' => self.add_token(TokenType::STAR, None),
            b'=' => match self.expected_char(b'=') {
                true => self.add_token(TokenType::EQUAL_EQUAL, None),
                false => self.add_token(TokenType::EQUAL, None),
            },
            b'!' => match self.expected_char(b'=') {
                true => self.add_token(TokenType::BANG_EQUAL, None),
                false => self.add_token(TokenType::BANG, None),
            },
            b'<' => match self.expected_char(b'=') {
                true => self.add_token(TokenType::LESS_EQUAL, None),
                false => self.add_token(TokenType::LESS, None),
            },
            b'>' => match self.expected_char(b'=') {
                true => self.add_token(TokenType::GREATER_EQUAL, None),
                false => self.add_token(TokenType::GREATER, None),
            },
            b'/' => match self.expected_char(b'/') {
                true => {
                    while self.peek() != b'\n' && !self.is_at_stream_end() {
                        _ = self.read_char();
                    }
                }
                false => self.add_token(TokenType::SLASH, None),
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
                    self.add_token(TokenType::STRING, Some(Literal::Text(literal)));
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
                    Some(Literal::Number(
                        lexume
                            .parse::<f64>()
                            .expect("Can't convert str to floating-point"),
                    )),
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
                    self.add_token(token_type, None);
                    return;
                }
                self.add_token(TokenType::IDENTIFIER, None);
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
        self.instruction_stream
            .as_bytes()
            .get(self.location.token_offset + 1)
            .copied()
            .unwrap_or(b'\0')
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
    fn add_token(&mut self, t: TokenType, literal: Option<Literal>) {
        let start = self.location.start_token;
        let eater_offset = self.location.token_offset;

        let lexeme_ref = &self.instruction_stream.as_bytes()[start..eater_offset];
        let lexeme = std::str::from_utf8(lexeme_ref).unwrap().to_string();

        let token = Token::new(t, lexeme, literal);
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

    // fn print_streams(&self) {
    //     println!("Instruction Stream:\n {}", self.instruction_stream)
    // }

    pub fn print_token(&self) {
        for token in self.token_stream.iter() {
            println!("{}", token);
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