use crate::error;
use std::fs;
use std::io::{self, Write};

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

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: String,
    line: usize,
}

impl Token {
    fn new(token_type: TokenType, lexeme: String, literal: String, line: usize) -> Self {
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
            "null".to_string(),
            self.location.line,
        );
        self.token_stream.push(eof_token);
    }

    fn scan_token(&mut self) {
        let char = self.read_char();
        match char {
            b'(' => self.add_token(TokenType::LEFT_PAREN),
            b')' => self.add_token(TokenType::RIGHT_PAREN),
            b'{' => self.add_token(TokenType::LEFT_BRACE),
            b'}' => self.add_token(TokenType::RIGHT_BRACE),
            b',' => self.add_token(TokenType::COMMA),
            b'.' => self.add_token(TokenType::DOT),
            b'-' => self.add_token(TokenType::MINUS),
            b'+' => self.add_token(TokenType::PLUS),
            b';' => self.add_token(TokenType::SEMICOLON),
            b'*' => self.add_token(TokenType::STAR),
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

    fn add_token(&mut self, t: TokenType) {
        let start = self.location.start_token;
        let eater_offset = self.location.token_offset;
        let line = self.location.line;

        let lexeme_ref = &self.instruction_stream.as_bytes()[start..eater_offset];
        let lexeme = std::str::from_utf8(lexeme_ref).unwrap().to_string();

        let token = Token::new(t, lexeme, "null".to_string(), line);
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
        let mut e_code:u8 = 0;
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
