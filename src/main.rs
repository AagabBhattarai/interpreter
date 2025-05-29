mod error;
mod evaluate;
mod native_function;
mod parser;
mod scanner;

use error::{EvalError, ParseError};
use evaluate::Evaluator;
use parser::{Declaration, Expr, Parser};
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
        "parse" => match scan_and_parse(filename) {
            Ok(ast) => {
                println!("{}", ast);
                return ExitCode::from(0);
            }
            Err((msg, code)) => {
                eprintln!("{msg}");
                return ExitCode::from(code);
            }
        },
        "evaluate" => match scan_and_parse(filename) {
            Ok(ast) => {
                let mut evaluator = Evaluator::new();
                let value = evaluator.evaluate_expr(&ast, 0);
                match value {
                    Ok(evaluate::Referenceable::Value(v)) => println!("{}", v),
                    Err(EvalError::OperandError(msg, code)) => {
                        eprintln!("{}", msg);
                        return ExitCode::from(code);
                    }
                    Err(EvalError::UndefinedVariable(msg, code)) => {
                        eprintln!("{}", msg);
                        return ExitCode::from(code);
                    }
                    Err(EvalError::NotCallable(msg, code)) => {
                        eprintln!("{}", msg);
                        return ExitCode::from(code);
                    }
                    Err(EvalError::ArityError(msg, code)) => {
                        eprintln!("{}", msg);
                        return ExitCode::from(code);
                    }
                    _ => panic!("can't print anything except value"),
                }
                return ExitCode::from(0);
            }
            Err((msg, code)) => {
                eprintln!("{msg}");
                return ExitCode::from(code);
            }
        },
        "run" => match scan_and_parse_statements(filename) {
            Ok(statements) => {
                // println!("{:?}", statements);
                let mut evaluator = Evaluator::new();
                evaluator.register_native_functions();
                match evaluator.evaluate(&statements) {
                    Ok(_) => return ExitCode::from(0),
                    Err(EvalError::OperandError(msg, code)) => {
                        eprintln!("{}", msg);
                        return ExitCode::from(code);
                    }
                    Err(EvalError::UndefinedVariable(msg, code)) => {
                        eprintln!("{}", msg);
                        return ExitCode::from(code);
                    }
                    Err(EvalError::NotCallable(msg, code)) => {
                        eprintln!("{}", msg);
                        return ExitCode::from(code);
                    }

                    Err(EvalError::ArityError(msg, code)) => {
                        eprintln!("{}", msg);
                        return ExitCode::from(code);
                    }
                }
            }
            Err((msg, code)) => {
                eprintln!("{msg}");
                return ExitCode::from(code);
            }
        },
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return ExitCode::FAILURE;
        }
    }
}

fn scan_and_parse_statements(filename: &String) -> Result<Vec<Declaration>, (String, u8)> {
    let mut scanner = Scanner::new(&filename);
    let token_stream = scanner.run_scan();
    let mut parser = Parser::new(token_stream);
    match parser.parse_program() {
        Ok(statements) => return Ok(statements),
        Err(e) => match e {
            ParseError::InvalidToken(msg, code) => {
                return Err((msg, code));
            }
            ParseError::ExpectedToken(msg, code) => {
                return Err((msg, code));
            }
        },
    }
}

fn scan_and_parse(filename: &String) -> Result<Expr, (String, u8)> {
    let mut scanner = Scanner::new(&filename);
    let token_stream = scanner.run_scan();
    let mut parser = Parser::new(token_stream);
    match parser.parse() {
        Ok(ast) => return Ok(ast),
        Err(e) => match e {
            ParseError::InvalidToken(msg, code) => {
                return Err((msg, code));
            }
            ParseError::ExpectedToken(msg, code) => {
                return Err((msg, code));
            }
        },
    }
}
