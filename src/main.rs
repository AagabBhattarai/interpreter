use interpreter::parser::Parser;
use interpreter::scanner::Scanner;
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
            let ast = parser.parse();

            println!("{}", ast);

            return ExitCode::from(0);
            // todo!()
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return ExitCode::FAILURE;
        }
    }
}
