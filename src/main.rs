mod error;
mod parser;
mod scanner;

use error::ParseError;
use parser::Parser;
use scanner::Scanner;
use std::env;
use std::io::{self, Write};
use std::process::ExitCode;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return ExitCode::FAILURE;
    }
    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let mut scanner = Scanner::new(&filename);
            scanner.run_scan();
            let code = scanner.print_error_stream();
            scanner.print_token();
            return ExitCode::from(code);
        }
        "parse" => {
            let mut scanner = Scanner::new(&filename);
            let token_stream = scanner.run_scan();

            let mut parser = Parser::new(token_stream);
            match parser.parse() {
                Ok(ast) => {
                    println!("{}", ast);
                    return ExitCode::from(0);
                }
                Err(e) => match e {
                    ParseError::InvalidToken(msg, code) => {
                        eprintln!("{msg}");
                        return ExitCode::from(code);
                    }
                },
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return ExitCode::FAILURE;
        }
    }
}
