use std::io;

use crate::frontend::{
	FrontendError, FrontendWarning, Keyword, LexerError, Operator, ParseError, SourceLocation,
	TokenValue,
};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

type ParserError = ParseError;

pub fn pretty_print_error(err: FrontendError, input: &str) {
	let mut stderr = StandardStream::stderr(ColorChoice::Auto);
	let _ = emit_message(&mut stderr, Message::Error(err), input);
}

pub fn emit_message<W: WriteColor>(
	writer: &mut W,
	message: Message,
	input: &str,
) -> io::Result<()> {
	let mut labels: Vec<Option<String>> = Vec::new();
	let mut locations: Vec<SourceLocation> = Vec::new();

	let (message, kind, hint) = match message {
		Message::Error(err) => {
			let (message, hint) = emit_error_message(err, &mut labels, &mut locations);
			(message, MessageKind::Error, hint)
		}
		Message::Warning(warning) => (
			emit_warning_message(warning, &mut labels, &mut locations),
			MessageKind::Warning,
			None,
		),
	};

	let mut spec = ColorSpec::new();
	match kind {
		MessageKind::Error => {
			spec.set_fg(Some(Color::Red)).set_bold(true);
			writer.set_color(&spec)?;
			write!(writer, "error")?;
		}
		MessageKind::Warning => {
			spec.set_fg(Some(Color::Yellow)).set_bold(true);
			writer.set_color(&spec)?;
			write!(writer, "warning")?;
		}
	}
	writer.reset()?;
	if let Some(loc) = locations.first() {
		let mut cyan_spec = ColorSpec::new();
		cyan_spec.set_fg(Some(Color::Cyan));
		write!(writer, ": {message} at ")?;
		writer.set_color(&cyan_spec)?;
		writeln!(
			writer,
			"{}:{}:{}",
			if loc.file.is_empty() {
				"<input>"
			} else {
				loc.file.as_str()
			},
			loc.range.begin.1.max(1),
			loc.range.begin.0.max(1)
		)?;
		writer.reset()?;
	} else {
		writeln!(writer, ": {message}")?;
	}

	if !locations.is_empty() {
		let resolved_input = resolve_input(&locations, input);
		highlight_locations(writer, &locations, &labels, resolved_input.as_str())?;
	}

	if let Some(hint) = hint {
		let mut green_spec = ColorSpec::new();
		green_spec.set_fg(Some(Color::Green));
		writer.set_color(&green_spec)?;
		write!(writer, "hint")?;
		writer.reset()?;
		write!(writer, ": {hint}")?;
		writeln!(writer)?;
		writer.reset()?;
	}

	Ok(())
}

fn emit_error_message(
	err: FrontendError,
	labels: &mut Vec<Option<String>>,
	locations: &mut Vec<SourceLocation>,
) -> (String, Option<String>) {
	match err {
		FrontendError::ParseError(err) => {
			let mut hint = None;
			let message = match err {
				ParserError::LexerError(e) => {
					labels.push(None);
					locations.push(e.location().clone());
					format!("lexer error: {}", describe_lexer_error(&e))
				}
				ParserError::InvalidUnaryOperator(tok) => {
					labels.push(Some("invalid unary operator".to_string()));
					locations.push(tok.location);
					format!(
						"invalid unary operator {}",
						describe_token_value(&tok.v)
					)
				}
				ParserError::InvalidBinaryOperator(tok) => {
					labels.push(Some("invalid binary operator".to_string()));
					locations.push(tok.location);
					format!(
						"invalid binary operator {}",
						describe_token_value(&tok.v)
					)
				}
				ParserError::UnexpectedToken(tok, expected) => {
					labels.push(Some(format!(
						"expected {}",
						describe_token_value(&expected)
					)));
					locations.push(tok.location);
					format!(
						"unexpected {} (expected {})",
						describe_token_value(&tok.v),
						describe_token_value(&expected)
					)
				}
				ParserError::ExpectedToken(tok, expected) => {
					labels.push(Some(format!(
						"expected {}",
						describe_token_value(&expected)
					)));
					locations.push(tok.location);
					format!(
						"expected {} but found {}",
						describe_token_value(&expected),
						describe_token_value(&tok.v)
					)
				}
				ParserError::UnclosedOperatorName(tok, expected) => {
					labels.push(Some(format!(
						"operator name not closed (expected {})",
						describe_token_value(&expected)
					)));
					locations.push(tok.location);
					format!(
						"operator name not closed (expected {})",
						describe_token_value(&expected)
					)
				}
				ParserError::InvalidOperatorName(tok) => {
					labels.push(Some("invalid operator".to_string()));
					locations.push(tok.location);
					"invalid operator".to_string()
				}
				ParserError::InvalidFactorToken(tok) => {
					labels.push(None);
					locations.push(tok.location);
					format!(
						"invalid factor token {}",
						describe_token_value(&tok.v)
					)
				}
				ParserError::ExpectedConditionExpression(tok) => {
					labels.push(None);
					locations.push(tok.location);
					"expected condition expression".to_string()
				}
				ParserError::InvalidForBinding(loc) => {
					labels.push(None);
					locations.push(loc);
					"invalid for binding, must be identifier or reference to identifier"
						.to_string()
				}
				ParserError::ExpectedBody(loc) => {
					labels.push(None);
					locations.push(loc);
					"expected body".to_string()
				}
				ParserError::ExpectedExpression(tok) => {
					labels.push(None);
					locations.push(tok.location);
					format!(
						"expected expression, but found {}",
						describe_token_value(&tok.v)
					)
				}
				ParserError::InvalidDeclarationType(tok) => {
					labels.push(None);
					locations.push(tok.location);
					format!(
						"invalid declaration type, found {}",
						describe_token_value(&tok.v)
					)
				}
				ParserError::InvalidArraySize(loc) => {
					labels.push(Some(
						"array size must be identifier or dotted path"
							.to_string(),
					));
					locations.push(loc);
					"invalid array size expression".to_string()
				}
				ParserError::InvalidNumericType(tok) => {
					labels.push(Some("invalid numeric type".to_string()));
					locations.push(tok.location);
					hint = match &tok.v {
						TokenValue::Id(name)
							if name == "u0" || name == "u1" =>
						{
							Some("use bool instead".to_string())
						}
						_ => None,
					};
					format!(
						"invalid numeric type {}",
						describe_token_value(&tok.v)
					)
				}
				ParserError::PostReturnIdAlreadyDefined(tok) => {
					labels.push(Some("post return identifier already defined"
						.to_string()));
					locations.push(tok.location);
					"post return identifier already defined".to_string()
				}
				ParserError::MixedInitializerListStyles(tok) => {
					labels.push(Some(
						"initializer list items must be all named or all positional"
							.to_string(),
					));
					locations.push(tok.location);
					"mixed initializer list styles".to_string()
				}
				ParserError::MissingInitializerDot(tok) => {
					labels.push(Some(
						"named initializer items must start with '.'"
							.to_string(),
					));
					locations.push(tok.location);
					"missing '.' before initializer item name".to_string()
				}
				ParserError::UnsupportedIncrement(tok) => {
					labels.push(Some("`++` is not supported".to_string()));
					locations.push(tok.location);
					"`++` is not supported".to_string()
				}
				ParserError::UnsupportedDecrement(tok) => {
					labels.push(Some("`--` is not supported".to_string()));
					locations.push(tok.location);
					"`--` is not supported".to_string()
				}
			};
			(message, hint)
		}
		FrontendError::InvalidEnumeratedArrayEnum(source_location) => {
			labels.push(Some("invalid enumareted array enum".to_string()));
			locations.push(source_location);
			message("invalid enumareted array enum")
		}
		FrontendError::InitializerListHasDuplicateFields {
			first_found_definition,
			conflicting_definition,
		} => {
			labels.push(Some("first field".to_string()));
			locations.push(first_found_definition);
			labels.push(Some("conflicting field".to_string()));
			locations.push(conflicting_definition);
			message("duplicate fields in initializer list")
		}
		FrontendError::DuplicateValueDeclaration {
			name,
			first_found_definition,
			conflicting_definition,
		} => {
			labels.push(Some("first declaration".to_string()));
			locations.push(first_found_definition);
			labels.push(Some("duplicate declaration".to_string()));
			locations.push(conflicting_definition);
			message(format!("duplicate declaration of `{name}`"))
		}
		FrontendError::DuplicateFieldDeclaration {
			name,
			first_found_definition,
			conflicting_definition,
		} => {
			labels.push(Some("first field".to_string()));
			locations.push(first_found_definition);
			labels.push(Some("duplicate field".to_string()));
			locations.push(conflicting_definition);
			message(format!("duplicate field `{name}`"))
		}
		FrontendError::HideOutsideScope { location, name } => {
			labels.push(Some("hide only allowed in scope".to_string()));
			locations.push(location);
			message(format!("`hide` not allowed here for `{name}`"))
		}
		FrontendError::InvalidStructMember { location } => {
			labels.push(Some("invalid struct member".to_string()));
			locations.push(location);
			message("struct bodies may only contain declarations")
		}
		FrontendError::InlineStructTypeNotAllowed { location } => {
			labels.push(Some("use a named struct type".to_string()));
			locations.push(location);
			(
				"inline struct types are not allowed in type arguments".to_string(),
				Some("define a named type like `Unit :: struct{}`".to_string()),
			)
		}
		FrontendError::GenericOperatorConstraint {
			location,
			call_location,
			operator,
			lhs,
			rhs,
		} => {
			labels.push(Some("invalid operator".to_string()));
			locations.push(location);
			labels.push(Some("instantiated here".to_string()));
			locations.push(call_location);
			message(format!(
				"Failed to find suitable operator overload for {lhs}.operator{operator}({rhs})"
			))
		}
		FrontendError::GenericMemberConstraint {
			location,
			call_location,
			member,
			lhs,
		} => {
			labels.push(Some("invalid member access".to_string()));
			locations.push(location);
			labels.push(Some("instantiated here".to_string()));
			locations.push(call_location);
			message(format!("type {lhs} has no member {member}"))
		}
		FrontendError::StructOrUnionNotInComptimeDeclaration(source_location) => {
			labels.push(Some(
				"structs and unions must be in comptime declarations".to_string()
			));
			locations.push(source_location);
			message("structs and unions must be in comptime declarations")
		}
		FrontendError::InvalidDeclarationArity(source_location) => {
			labels.push(Some("invalid multi declaration arity".to_string()));
			locations.push(source_location);
			message("invalid multi declaration arity")
		}
		FrontendError::MissingPackage(source_location) => {
			labels.push(Some("missing package declaration".to_string()));
			locations.push(source_location);
			message("missing package declaration")
		}
		FrontendError::PackageMismatch {
			location,
			expected,
			found,
		} => {
			labels.push(Some(format!("expected package `{expected}`")));
			locations.push(location);
			message(format!(
				"package mismatch: expected `{expected}`, found `{found}`"
			))
		}
		FrontendError::ModuleNotFound { location, module } => {
			labels.push(Some("module not found".to_string()));
			locations.push(location);
			message(format!("module not found: `{module}`"))
		}
		FrontendError::DuplicateModuleAlias { location, alias } => {
			labels.push(Some("duplicate module alias".to_string()));
			locations.push(location);
			message(format!("duplicate module alias `{alias}`"))
		}
		FrontendError::DuplicateModuleImport { location, module } => {
			labels.push(Some("duplicate module import".to_string()));
			locations.push(location);
			message(format!("duplicate module import `{module}`"))
		}
		FrontendError::DuplicateExport { location, name } => {
			labels.push(Some("duplicate export".to_string()));
			locations.push(location);
			message(format!("duplicate export `{name}`"))
		}
		FrontendError::UnknownType {
			location,
			name,
			hint,
		} => {
			labels.push(Some("unknown type".to_string()));
			locations.push(location);
			(format!("unknown type `{name}`"), hint)
		}
		FrontendError::UnknownValue { location, name } => {
			labels.push(Some("unknown value".to_string()));
			locations.push(location);
			message(format!("unknown value `{name}`"))
		}
		FrontendError::TypeMismatch {
			location,
			expected,
			found,
		} => {
			labels.push(Some(format!("expected {expected}")));
			locations.push(location);
			message(format!(
				"type mismatch (expected {expected}, found {found})"
			))
		}
		FrontendError::InvalidOperator {
			location,
			operator,
			lhs,
			rhs,
		} => {
			labels.push(Some("invalid operator".to_string()));
			locations.push(location);
			let args = rhs.unwrap_or_default();
			let sig = if args.is_empty() {
				format!("{lhs}.operator{operator}()")
			} else {
				format!("{lhs}.operator{operator}({args})")
			};
			message(format!(
				"Failed to find suitable operator overload for {sig}"
			))
		}
		FrontendError::InvalidCall { location, callee } => {
			labels.push(Some("invalid call".to_string()));
			locations.push(location);
			message(format!("invalid call to {callee}"))
		}
		FrontendError::MissingOperatorSelf {
			location,
			operator,
			hint,
		} => {
			labels.push(Some("missing self parameter".to_string()));
			locations.push(location);
			(
				format!("operator {operator} must take self parameter"),
				Some(hint),
			)
		}
		FrontendError::MissingField {
			location,
			type_name,
			field,
		} => {
			labels.push(Some("missing field".to_string()));
			locations.push(location);
			message(format!("type {type_name} has no field {field}"))
		}
		FrontendError::CyclicTypeDefinition {
			location,
			type_name,
			cycle: _,
			cycle_locations,
		} => {
			labels.push(Some("cyclic type definition".to_string()));
			locations.push(location);
			for (location, label) in cycle_locations {
				labels.push(Some(label));
				locations.push(location);
			}
			(
				format!("type `{type_name}` has a cyclic definition"),
				Some(format!(
					"consider making one of the fields a reference (e.g. `&{type_name}`)"
				)),
			)
		}
		FrontendError::InvalidIndex {
			location,
			target,
			index,
		} => {
			labels.push(Some("invalid index".to_string()));
			locations.push(location);
			message(format!("cannot index {target} with {index}"))
		}
		FrontendError::NonBoolCondition { location, found } => {
			labels.push(Some("expected bool".to_string()));
			locations.push(location);
			message(format!("condition must be bool (found {found})"))
		}
		FrontendError::UntypedLiteralNeedsContext { location, kind } => {
			labels.push(Some("needs type context".to_string()));
			locations.push(location);
			message(format!("untyped {kind} literal needs a type context"))
		}
		FrontendError::DuplicateUnionVariantType { location, ty } => {
			labels.push(Some("duplicate union variant type".to_string()));
			locations.push(location);
			message(format!("union contains duplicate variant type {ty}"))
		}
		FrontendError::UnusedValue { location, hint } => {
			labels.push(Some("unused value".to_string()));
			locations.push(location);
			("unused value".to_string(), Some(hint))
		}
		FrontendError::InaccessibleMember {
			location,
			type_name,
			member,
		} => {
			labels.push(Some("inaccessible member".to_string()));
			locations.push(location);
			(
				format!("{type_name}.{member} is not accessible"),
				Some("mark it as `pub` to expose it".to_string()),
			)
		}
	}
}

fn message(text: impl Into<String>) -> (String, Option<String>) {
	(text.into(), None)
}

fn emit_warning_message(
	warning: FrontendWarning,
	labels: &mut Vec<Option<String>>,
	locations: &mut Vec<SourceLocation>,
) -> String {
	match warning {
		FrontendWarning::ModuleConflict {
			location,
			module,
			chosen_path,
			ignored_paths,
		} => {
			labels.push(Some("module conflict".to_string()));
			locations.push(location);
			let ignored = if ignored_paths.is_empty() {
				"".to_string()
			} else {
				format!("; ignored: {}", ignored_paths.join(", "))
			};
			format!("module `{module}` resolved to `{chosen_path}`{ignored}")
		}
	}
}

pub enum Message {
	Error(FrontendError),
	Warning(FrontendWarning),
}

enum MessageKind {
	Error,
	Warning,
}

fn resolve_input(locations: &[SourceLocation], default_input: &str) -> String {
	let Some(location) = locations.iter().find(|loc| !loc.file.is_empty()) else {
		return default_input.to_string();
	};

	match std::fs::read_to_string(&location.file) {
		Ok(contents) => contents,
		Err(_) => default_input.to_string(),
	}
}

fn describe_lexer_error(err: &LexerError) -> String {
	match err {
		LexerError::InvalidString { message, .. } => {
			format!("invalid string literal: {message}")
		}
		LexerError::InvalidLifetimeName { ch, .. } => {
			format!("invalid lifetime name '{}'", ch)
		}
		LexerError::UnexpectedCharacter { ch, .. } => {
			format!("unexpected character '{}'", ch.escape_default())
		}
	}
}

fn describe_token_value(value: &TokenValue) -> String {
	match value {
		TokenValue::Id(name) => format!("identifier `{}`", name),
		TokenValue::String(data) => format!("string literal \"{}\"", data.escape_default()),
		TokenValue::Char(ch) => {
			format!("character literal '{}'", ch.escape_default())
		}
		TokenValue::Keyword(keyword) => format!("keyword `{}`", keyword_lexeme(keyword)),
		TokenValue::Integer(v) => format!("integer literal {}", v),
		TokenValue::Float(v) => format!("float literal {}", v),
		TokenValue::Op { op, has_equals } => {
			format!("operator `{}`", operator_symbol(op, *has_equals))
		}
		TokenValue::Lifetime(ch) => format!("lifetime `'{}'", ch),
		TokenValue::Colon => "symbol `:`".to_string(),
		TokenValue::Semicolon => "symbol `;`".to_string(),
		TokenValue::Comma => "symbol `,`".to_string(),
		TokenValue::Arrow => "symbol `->`".to_string(),
		TokenValue::Ellipsis => "symbol `..`".to_string(),
		TokenValue::ListInit => "symbol `.{`".to_string(),
		TokenValue::LParen => "symbol `(`".to_string(),
		TokenValue::RParen => "symbol `)`".to_string(),
		TokenValue::LSquirly => "symbol `{`".to_string(),
		TokenValue::RSquirly => "symbol `}`".to_string(),
		TokenValue::LBracket => "symbol `[`".to_string(),
		TokenValue::RBracket => "symbol `]`".to_string(),
		TokenValue::EOF => "end of file".to_string(),
	}
}

fn keyword_lexeme(keyword: &Keyword) -> &'static str {
	match keyword {
		Keyword::Fn => "fn",
		Keyword::Package => "package",
		Keyword::Use => "use",
		Keyword::As => "as",
		Keyword::In => "in",
		Keyword::For => "for",
		Keyword::While => "while",
		Keyword::Pub => "pub",
		Keyword::Mut => "mut",
		Keyword::Cast => "cast",
		Keyword::Transmute => "transmute",
		Keyword::Defer => "defer",
		Keyword::Where => "where",
		Keyword::Pre => "pre",
		Keyword::Post => "post",
		Keyword::Ensures => "ensures",
		Keyword::Do => "do",
		Keyword::Extends => "extends",
		Keyword::Operator => "operator",
		Keyword::Return => "return",
		Keyword::Match => "match",
		Keyword::Case => "case",
		Keyword::If => "if",
		Keyword::Else => "else",
		Keyword::Newtype => "newtype",
		Keyword::Alias => "alias",
		Keyword::Struct => "struct",
		Keyword::Enum => "enum",
		Keyword::Union => "union",
		Keyword::RawUnion => "raw_union",
		Keyword::Void => "void",
		Keyword::Hide => "hide",
	}
}

fn operator_symbol(op: &Operator, has_equals: bool) -> String {
	let mut symbol = match op {
		Operator::Add => "+",
		Operator::Sub => "-",
		Operator::Mul => "*",
		Operator::Divide => "/",
		Operator::LessThan => "<",
		Operator::GreaterThan => ">",
		Operator::Not => "!",
		Operator::Or => "||",
		Operator::And => "&&",
		Operator::BinAnd => "&",
		Operator::BinOr => "|",
		Operator::BinXOR => "^",
		Operator::Dot => ".",
		Operator::Set => "=",
	}
	.to_string();

	if has_equals {
		symbol.push('=');
	}

	symbol
}

fn highlight_locations<W: WriteColor>(
	writer: &mut W,
	locations: &[SourceLocation],
	labels: &[Option<String>],
	input: &str,
) -> io::Result<()> {
	for (idx, loc) in locations.iter().enumerate() {
		let label = labels.get(idx).and_then(|l| l.as_deref());
		highlight_single_location(writer, loc, label, input)?;
		if idx + 1 != locations.len() {
			writeln!(writer)?;
		}
	}
	Ok(())
}

fn highlight_single_location<W: WriteColor>(
	writer: &mut W,
	loc: &SourceLocation,
	label: Option<&str>,
	input: &str,
) -> io::Result<()> {
	let line_number = loc.range.begin.1.max(1) as usize;
	let column_number = loc.range.begin.0.max(1) as usize;

	let raw_line = fetch_line(input, line_number);
	let (visual_line, columns) = visualize_line(raw_line);

	let end_column = loc.range.end.0.max(loc.range.begin.0 + 1) as usize;

	let start_offset = column_to_visual_offset(column_number, &columns);
	let mut end_offset = column_to_visual_offset(end_column, &columns);
	if end_offset <= start_offset {
		end_offset = start_offset + 1;
	}

	let caret_width = end_offset - start_offset;
	let line_str = line_number.to_string();
	let gutter_width = line_str.len().max(2);

	let mut dim_spec = ColorSpec::new();
	dim_spec.set_fg(Some(Color::Ansi256(8)));
	let mut caret_spec = ColorSpec::new();
	caret_spec.set_fg(Some(Color::Red)).set_bold(true);
	let mut context_spec = ColorSpec::new();
	context_spec
		.set_fg(Some(Color::Ansi256(8)))
		.set_dimmed(true);

	let prev_line_number = line_number.saturating_sub(1);
	if prev_line_number > 0 {
		let prev_raw = fetch_line(input, prev_line_number);
		let (prev_visual, _) = visualize_line(prev_raw);
		write_source_line(
			writer,
			prev_line_number,
			&prev_visual,
			gutter_width,
			&dim_spec,
			true,
			&context_spec,
		)?;
	} else {
		write!(writer, "  ")?;
		writer.set_color(&dim_spec)?;
		write!(writer, "{:>width$}", "", width = gutter_width)?;
		write!(writer, " ")?;
		write!(writer, "|")?;
		writer.reset()?;
		writeln!(writer)?;
	}

	write_source_line(
		writer,
		line_number,
		&visual_line,
		gutter_width,
		&dim_spec,
		false,
		&context_spec,
	)?;

	write!(writer, "  ")?;
	write!(writer, "{:>width$}", "", width = gutter_width)?;
	write!(writer, " ")?;
	writer.set_color(&dim_spec)?;
	write!(writer, "|")?;
	writer.reset()?;
	write!(writer, " ")?;
	let caret_padding = " ".repeat(start_offset);
	write!(writer, "{caret_padding}")?;
	writer.set_color(&caret_spec)?;
	write!(writer, "{}", "^".repeat(caret_width))?;
	writer.reset()?;
	if let Some(text) = label {
		write!(writer, " {text}")?;
	}
	writeln!(writer)?;

	Ok(())
}

fn fetch_line(input: &str, line_number: usize) -> &str {
	if line_number == 0 {
		return "";
	}
	input.split('\n')
		.nth(line_number - 1)
		.map(|line| line.trim_end_matches('\r'))
		.unwrap_or("")
}

fn visualize_line(line: &str) -> (String, Vec<usize>) {
	let mut visual = String::new();
	let mut offsets = Vec::new();
	offsets.push(0);

	let mut width = 0usize;
	for ch in line.chars() {
		if ch == '\t' {
			visual.push(' ');
			visual.push(' ');
			width += 2;
		} else {
			visual.push(ch);
			width += 1;
		}
		offsets.push(width);
	}

	(visual, offsets)
}

fn column_to_visual_offset(column: usize, offsets: &[usize]) -> usize {
	if column == 0 || offsets.is_empty() {
		return 0;
	}
	let idx = column.saturating_sub(1);
	if idx >= offsets.len() {
		*offsets.last().unwrap()
	} else {
		offsets[idx]
	}
}

fn write_source_line<W: WriteColor>(
	writer: &mut W,
	line_number: usize,
	text: &str,
	gutter_width: usize,
	gutter_spec: &ColorSpec,
	dim_line: bool,
	dim_text_spec: &ColorSpec,
) -> io::Result<()> {
	write!(writer, "  ")?;
	if dim_line {
		writer.set_color(dim_text_spec)?;
	}
	write!(writer, "{:>width$}", line_number, width = gutter_width)?;
	if dim_line {
		writer.reset()?;
	}
	write!(writer, " ")?;
	writer.set_color(gutter_spec)?;
	write!(writer, "|")?;
	writer.reset()?;
	if dim_line {
		writer.set_color(dim_text_spec)?;
	}
	writeln!(writer, " {text}")?;
	if dim_line {
		writer.reset()?;
	}
	Ok(())
}

pub fn pretty_print_multiple_errors(input: String, errors: Vec<FrontendError>) {
	let mut first = true;
	for err in errors {
		if !first {
			eprintln!();
		}
		pretty_print_error(err, input.as_str());
		first = false;
	}
}
