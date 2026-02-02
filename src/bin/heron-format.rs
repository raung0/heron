use clap::{Arg, ArgAction, Command};
use heron::frontend;
use heron::{
	diagnostics::pretty_print_error,
	formatter::{format_ast_with_options, FormatConfig, FormatOptions},
};

fn main() {
	let matches = Command::new("heron-format")
		.about("Heron formatter")
		.arg(Arg::new("file")
			.help("Path(s) to the Heron source file(s) to format")
			.required_unless_present("default_config")
			.value_name("FILE")
			.num_args(1..))
		.arg(Arg::new("option")
			.short('o')
			.help("Formatter option (key=value), overrides .heron-format")
			.value_name("KEY=VALUE")
			.action(ArgAction::Append))
		.arg(Arg::new("in_place")
			.short('i')
			.long("in-place")
			.help("Overwrite the input file instead of writing to stdout")
			.action(ArgAction::SetTrue))
		.arg(Arg::new("default_config")
			.long("default-config")
			.help("Print the default .heron-format configuration")
			.action(ArgAction::SetTrue))
		.get_matches();

	if matches.get_flag("default_config") {
		let config_str =
			FormatOptions::default()
				.to_config_string()
				.unwrap_or_else(|err| {
					eprintln!(
						"error: failed to serialize default config: {err}"
					);
					std::process::exit(1);
				});
		print!("{config_str}");
		return;
	}

	let files: Vec<String> = matches
		.get_many::<String>("file")
		.expect("clap enforces required argument")
		.map(|s| s.to_owned())
		.collect();
	let in_place = matches.get_flag("in_place");
	let mut options = FormatOptions::default();
	if let Some(config_str) = load_format_config() {
		let config: FormatConfig = serde_ini::from_str(&config_str).unwrap_or_else(|err| {
			eprintln!("error: failed to parse .heron-format: {err}");
			std::process::exit(1);
		});
		if let Err(err) = options.apply_config(&config) {
			eprintln!("error: {err}");
			std::process::exit(1);
		}
	}
	if let Some(opts) = matches.get_many::<String>("option") {
		for opt in opts {
			let config: FormatConfig = serde_ini::from_str(opt).unwrap_or_else(|err| {
				eprintln!("error: failed to parse option `{opt}`: {err}");
				std::process::exit(1);
			});
			if let Err(err) = options.apply_config(&config) {
				eprintln!("error: {err}");
				std::process::exit(1);
			}
		}
	}

	for file in files {
		let input = match std::fs::read_to_string(&file) {
			Ok(input) => input,
			Err(e) => {
				eprintln!("error: failed to read `{file}`: {e}");
				std::process::exit(1);
			}
		};

		let mut lexer = frontend::Lexer::new(input, file.clone());
		let input = lexer.get_input();
		let mut parser = match frontend::Parser::new(&mut lexer) {
			Ok(parser) => parser,
			Err(err) => {
				pretty_print_error(
					heron::frontend::FrontendError::ParseError(err),
					input.as_str(),
				);
				continue;
			}
		};
		let ast = parser.parse();
		let errors = parser.take_errors();
		if errors.is_empty() {
			match ast {
				Ok(ast) => {
					let formatted = format_ast_with_options(&ast, &options);
					if in_place {
						if let Err(e) = std::fs::write(&file, formatted) {
							eprintln!(
								"error: failed to write `{file}`: {e}"
							);
							std::process::exit(1);
						}
					} else {
						print!("{formatted}");
					}
				}
				Err(e) => pretty_print_error(
					heron::frontend::FrontendError::ParseError(e),
					input.as_str(),
				),
			}
		} else {
			let mut first = true;
			for err in errors {
				if !first {
					eprintln!();
				}
				pretty_print_error(
					heron::frontend::FrontendError::ParseError(err),
					input.as_str(),
				);
				first = false;
			}
		}
	}
}

fn load_format_config() -> Option<String> {
	let path = std::path::Path::new(".heron-format");
	if !path.exists() {
		return None;
	}
	match std::fs::read_to_string(path) {
		Ok(contents) => Some(contents),
		Err(err) => {
			eprintln!("error: failed to read `.heron-format`: {err}");
			std::process::exit(1);
		}
	}
}
