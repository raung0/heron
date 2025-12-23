#![allow(dead_code)]

use clap::{Arg, Command};
use heron::diagnostics::pretty_print_parser_error;
use heron::frontend;

fn main() {
    let matches = Command::new("heron")
        .about("Heron compiler")
        .arg(
            Arg::new("file")
                .help("Path to the Heron source file to parse")
                .required(true)
                .value_name("FILE"),
        )
        .get_matches();

    let file = matches
        .get_one::<String>("file")
        .expect("clap enforces required argument")
        .to_owned();

    let input = match std::fs::read_to_string(&file) {
        Ok(input) => input,
        Err(e) => {
            eprintln!("error: failed to read `{file}`: {e}");
            std::process::exit(1);
        }
    };

    let mut lexer = frontend::Lexer::new(input, file);
    let input = lexer.get_input();
    let mut parser = frontend::Parser::new(&mut lexer).unwrap();
    let ast = parser.parse();
    let errors = parser.take_errors();
    if errors.is_empty() {
        match ast {
            Ok(ast) => println!(
                "{}",
                frontend::AST::pretty_format(format!("{}", ast).as_str())
            ),
            Err(e) => pretty_print_parser_error(e, input.as_str()),
        }
    } else {
        let mut first = true;
        for err in errors {
            if !first {
                eprintln!();
            }
            pretty_print_parser_error(err, input.as_str());
            first = false;
        }
    }
}
