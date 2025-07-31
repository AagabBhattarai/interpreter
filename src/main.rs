#![allow(unreachable_patterns, dead_code)]

mod environment;
mod error;
mod evaluate;
mod expression;
mod native_function;
mod parser;
mod resolver;
mod scanner;

use error::{EvalError, ParseError, ResolutionError};
use evaluate::Evaluator;
use expression::Expr;
use parser::{Declaration, Parser};
use resolver::Resolver;
use scanner::Scanner;

use std::collections::HashMap;
use std::env;
use std::io::{self, Write};
use std::process::ExitCode;

use crate::expression::ExprId;

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
                // Wrap ast: Expr in Statement::Expr, then in Declaration::Statement, then in a Vec
                let statement = parser::Statement::Expression {
                    expr: ast.clone(),
                    line: 0,
                };
                let declaration = Declaration::Statement(statement);
                let statements = vec![declaration];

                // Run resolver on the statements to get depths
                match run_resolver(&statements) {
                    Ok(depths) => {
                        // Use the same AST (statements) for evaluation
                        let mut evaluator = Evaluator::new(depths);
                        match evaluator.evaluate_expr(&ast, 0) {
                            Ok(value) => match value {
                                evaluate::Referenceable::Value(val) => println!("{}", val),
                                _ => {
                                    eprintln!("Error: Expression did not evaluate to a value.");
                                    return ExitCode::from(1);
                                }
                            },
                            Err(EvalError::OperandError(msg, code))
                            | Err(EvalError::UndefinedVariable(msg, code))
                            | Err(EvalError::NotCallable(msg, code))
                            | Err(EvalError::ArityError(msg, code)) => {
                                eprintln!("{}", msg);
                                return ExitCode::from(code);
                            }
                            _ => panic!("can't print anything except value"),
                        }
                        return ExitCode::from(0);
                    }
                    Err((msg, code)) => {
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
        "resolve" => match scan_and_parse_statements(filename) {
            Ok(statements) => {
                println!("{:#?}", statements);
                let mut resolver = Resolver::new();
                match resolver.resolve(&statements) {
                    Ok(depths) => {
                        println!("Resolution successful. Variable depths: {:#?}", depths);
                        return ExitCode::from(0);
                    }
                    Err(ResolutionError::ReDeclaration(msg, code)) => {
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
        // Next task clean up all this main.rs repeatation
        "run" => match scan_and_parse_statements(filename) {
            Ok(statements) => {
                // println!("{:#?}", statements);
                match run_resolver(&statements) {
                    Ok(depth) => {
                        let mut evaluator = Evaluator::new(depth);
                        match evaluator.evaluate(&statements) {
                            Ok(_) => return ExitCode::from(0),
                            Err(e) => match e {
                                EvalError::OperandError(msg, code)
                                | EvalError::UndefinedVariable(msg, code)
                                | EvalError::NotCallable(msg, code)
                                | EvalError::ArityError(msg, code) => {
                                    eprintln!("{}", msg);
                                    return ExitCode::from(code);
                                }
                                _ => panic!("Only return value is not included here"),
                            },
                        }
                    }
                    Err((msg, code)) => {
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
fn run_resolver(declarations: &Vec<Declaration>) -> Result<HashMap<ExprId, usize>, (String, u8)> {
    let mut resolver = Resolver::new();
    match resolver.resolve(&declarations) {
        Ok(depths) => {
            // println!("Resolution successful. Variable depths: {:#?}", depths);
            Ok(depths)
        }
        Err(ResolutionError::ReDeclaration(msg, code)) => Err((msg, code)),
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
