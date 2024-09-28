use std::fs;
use std::io::{self, Write};
use std::thread::current;

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
    fn new(token_type: TokenType, lexeme: String,literal: String, line: usize) -> Self {
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
}

impl Scanner {
    fn new(filename: &str) -> Self {
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
            line: 0,
        };
        // let ts: Vec<Token> = Vec::new();
        Self {
            instruction_stream: file_content,
            token_stream: Vec::new(),
            had_error: false,
            location,
        }
    }

    fn run_scan(&mut self) {
        while !self.is_at_stream_end() {
            self.location.start_token = self.location.token_offset;
            self.scan_token();
        }
        let eof_token = Token::new(TokenType::EOF, String::new(), "null".to_string(), self.location.line);
        self.token_stream.push(eof_token);
    }
    fn scan_token(&mut self) {
        let char = self.read_char();
        match char {
            b'(' => self.add_token(TokenType::LEFT_PAREN),
            b')' => self.add_token(TokenType::RIGHT_PAREN),
            k => println!("Unexpected Character {1} at {0}", self.location.line,k)
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
    fn print_token(&self) {
        for token in self.token_stream.iter() {
            println!("{0:?} {1} {2}",token.token_type, token.lexeme, token.literal);
        }
    }
}

fn main() {
    // let a = TokenType::AND;
    // println!("This is {:?}", a);
    // let a = Token::new(TokenType::STRING, "HELLO".to_string(), 2);
    // a.print_info();

    let mut a = Scanner::new("src/test.lox");
    a.print_streams();
    a.run_scan();
    a.print_token(); 
}

// pub struct ErrorHandler {
//     line_number: usize,
//     message: String,
//     line: String,
// }

// impl ErrorHandler {
//     fn report_error(ln: usize, msg: String, line: String) {
//         println!("At line {ln},| {line}, Error: {msg}")
//     }
// }
