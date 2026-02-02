#![allow(dead_code)]

use clap::{Arg, ArgAction, Command};
use heron::diagnostics::{Message, emit_message, pretty_print_error, pretty_print_multiple_errors};
use heron::frontend::{self, FrontendError, dot_module_graph, run_passes_with_modules};
use termcolor::{ColorChoice, StandardStream};

fn main() {
	let matches = Command::new("heron")
		.about("Heron compiler")
		.arg(Arg::new("file")
			.help("Path to the Heron source file to parse")
			.required(true)
			.value_name("FILE"))
		.arg(Arg::new("dump_ast")
			.short('A')
			.long("dump-ast")
			.help("Dump the parsed AST")
			.action(ArgAction::SetTrue))
		.arg(Arg::new("module_path")
			.short('M')
			.value_name("DIR")
			.help("Add a module search path")
			.action(ArgAction::Append))
		.arg(Arg::new("dump_module_graph")
			.long("dump-module-graph")
			.help("Emit Graphviz DOT module graph")
			.action(ArgAction::SetTrue))
		.get_matches();

	let dump_ast = matches.get_flag("dump_ast");
	let dump_module_graph = matches.get_flag("dump_module_graph");
	let module_paths: Vec<String> = matches
		.get_many::<String>("module_path")
		.map(|values| values.cloned().collect())
		.unwrap_or_default();

	let file = matches
		.get_one::<String>("file")
		.expect("clap enforces required argument")
		.to_owned();
	let file_for_lexer = file.clone();

	let input = match std::fs::read_to_string(&file) {
		Ok(input) => input,
		Err(e) => {
			eprintln!("error: failed to read `{file}`: {e}");
			std::process::exit(1);
		}
	};

	let mut lexer = frontend::Lexer::new(input, file_for_lexer);
	let input = lexer.get_input();
	let mut parser = match frontend::Parser::new(&mut lexer) {
		Ok(parser) => parser,
		Err(err) => {
			pretty_print_error(
				heron::frontend::FrontendError::ParseError(err),
				input.as_str(),
			);
			return;
		}
	};
	let ast = parser.parse();
	let errors = parser.take_errors();
	if errors.is_empty() {
		match ast {
			Ok(ast) => {
				if dump_ast && !dump_module_graph {
					println!(
						"{}",
						frontend::AST::pretty_format(
							format!("{}", ast).as_str()
						)
					);
				}
				let (program, errors, warnings) =
					run_passes_with_modules(ast, file.as_str(), &module_paths);
				if !warnings.is_empty() {
					let mut stderr = StandardStream::stderr(ColorChoice::Auto);
					for warning in warnings {
						let _ = emit_message(
							&mut stderr,
							Message::Warning(warning),
							input.as_str(),
						);
					}
				}
				if !errors.is_empty() {
					pretty_print_multiple_errors(input, errors);
					return;
				}
				if dump_module_graph {
					println!("{}", dot_module_graph(&program));
					return;
				}
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
