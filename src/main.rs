#![allow(dead_code)]

use clap::{Arg, ArgAction, Command};
use heron::diagnostics::{pretty_print_error, pretty_print_multiple_errors};
use heron::frontend::{self, FrontendError, run_passes};

fn main() {
    let matches = Command::new("heron")
        .about("Heron compiler")
        .arg(
            Arg::new("file")
                .help("Path to the Heron source file to parse")
                .required(true)
                .value_name("FILE"),
        )
        .arg(
            Arg::new("dump_ast")
                .short('A')
                .long("dump-ast")
                .help("Dump the parsed AST")
                .action(ArgAction::SetTrue),
        )
        .get_matches();

    let dump_ast = matches.get_flag("dump_ast");

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
            Ok(ast) => {
                if dump_ast {
                    println!(
                        "{}",
                        frontend::AST::pretty_format(format!("{}", ast).as_str())
                    );
                }
                let errors = run_passes(ast);
                pretty_print_multiple_errors(input, errors);
            }
            Err(e) => pretty_print_error(
                heron::frontend::FrontendError::ParseError(e),
                input.as_str(),
            ),
        }
    } else {
        let errors = errors
            .iter()
            .map(|x| FrontendError::ParseError(x.clone()))
            .collect();
        pretty_print_multiple_errors(input, errors);
    }
}
