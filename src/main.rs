#![allow(dead_code)]

pub mod frontend;

use std::io;

use clap::{Arg, Command};
use frontend::{Keyword, LexerError, Operator, ParseError, SourceLocation, TokenValue};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

type ParserError = ParseError;

struct OnExit;

impl Drop for OnExit {
    fn drop(&mut self) {
        println!("Done!");
    }
}

fn pretty_print_parser_error(err: ParserError, input: String) {
    let mut stderr = StandardStream::stderr(ColorChoice::Auto);
    let _ = emit_parser_error(&mut stderr, err, input.as_str());
}

fn emit_parser_error<W: WriteColor>(
    writer: &mut W,
    err: ParserError,
    input: &str,
) -> io::Result<()> {
    let mut label: Option<String> = None;
    let mut location: Option<SourceLocation> = None;

    let message = match err {
        ParserError::LexerError(e) => {
            format!("lexer error: {}", describe_lexer_error(&e))
        }
        ParserError::InvalidUnaryOperator(tok) => {
            label = Some("invalid unary operator".to_string());
            location = Some(tok.location);
            format!("invalid unary operator {}", describe_token_value(&tok.v))
        }
        ParserError::InvalidBinaryOperator(tok) => {
            label = Some("invalid binary operator".to_string());
            location = Some(tok.location);
            format!("invalid binary operator {}", describe_token_value(&tok.v))
        }
        ParserError::UnexpectedToken(tok, expected) => {
            label = Some(format!("expected {}", describe_token_value(&expected)));
            location = Some(tok.location);
            format!(
                "unexpected {} (expected {})",
                describe_token_value(&tok.v),
                describe_token_value(&expected)
            )
        }
        ParserError::ExpectedToken(tok, expected) => {
            label = Some(format!("expected {}", describe_token_value(&expected)));
            location = Some(tok.location);
            format!(
                "expected {} but found {}",
                describe_token_value(&expected),
                describe_token_value(&tok.v)
            )
        }
        ParserError::InvalidFactorToken(tok) => {
            location = Some(tok.location);
            format!("invalid factor token {}", describe_token_value(&tok.v))
        }
        ParserError::ExpectedConditionExpression(tok) => {
            location = Some(tok.location);
            format!("expected condition expression")
        }
        ParserError::InvalidForBinding(loc) => {
            location = Some(loc);
            format!("invalid for binding, must be identifier or reference to identifier")
        }
        ParserError::ExpectedBody(loc) => {
            location = Some(loc);
            format!("expected body")
        }
        ParserError::ExpectedExpression(tok) => {
            location = Some(tok.location);
            format!(
                "expected expression, but found {}",
                describe_token_value(&tok.v)
            )
        }
        ParserError::InvalidDeclarationType(tok) => {
            location = Some(tok.location);
            format!(
                "invalid declaration type, found {}",
                describe_token_value(&tok.v)
            )
        }
        ParserError::PostReturnIdAlreadyDefined(tok) => {
            label = Some("post return identifier already defined".to_string());
            location = Some(tok.location);
            format!("post return identifier already defined")
        }
        ParserError::MixedInitializerListStyles(tok) => {
            label = Some("initializer list items must be all named or all positional".to_string());
            location = Some(tok.location);
            format!("mixed initializer list styles")
        }
    };

    let mut error_spec = ColorSpec::new();
    error_spec.set_fg(Some(Color::Red)).set_bold(true);
    writer.set_color(&error_spec)?;
    write!(writer, "error")?;
    writer.reset()?;
    if let Some(loc) = &location {
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

    if let Some(loc) = location {
        highlight_location(writer, &loc, label.as_deref(), input)?;
    }

    Ok(())
}

fn describe_lexer_error(err: &LexerError) -> String {
    match err {
        LexerError::InvalidString(msg) => {
            format!("invalid string literal: {msg}")
        }
        LexerError::InvalidLifetimeName(ch) => {
            format!("invalid lifetime name '{}'", ch)
        }
        LexerError::UnexpectedCharacter(ch) => {
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
        Keyword::Defer => "defer",
        Keyword::Where => "where",
        Keyword::Pre => "pre",
        Keyword::Post => "post",
        Keyword::Ensures => "ensures",
        Keyword::Do => "do",
        Keyword::Extends => "extends",
        Keyword::Using => "using",
        Keyword::Throws => "throws",
        Keyword::Throw => "throw",
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

fn highlight_location<W: WriteColor>(
    writer: &mut W,
    loc: &SourceLocation,
    label: Option<&str>,
    input: &str,
) -> io::Result<()> {
    let line_number = loc.range.begin.1.max(1) as usize;
    let column_number = loc.range.begin.0.max(1) as usize;

    let raw_line = fetch_line(input, line_number);
    let (visual_line, columns) = visualize_line(raw_line);

    let multi_line = loc.range.end.1 != loc.range.begin.1;
    let mut end_column = loc.range.end.0.max(loc.range.begin.0 + 1) as usize;
    if multi_line {
        end_column = columns.len();
    }

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
        if multi_line {
            write!(writer, " (spans multiple lines)")?;
        }
    } else if multi_line {
        write!(writer, " (spans multiple lines)")?;
    }
    writeln!(writer)?;

    Ok(())
}

fn fetch_line<'a>(input: &'a str, line_number: usize) -> &'a str {
    if line_number == 0 {
        return "";
    }
    input
        .split('\n')
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

fn main() {
    let _guard = OnExit;

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
    match ast {
        Ok(ast) => println!(
            "{}",
            frontend::AST::pretty_format(format!("{}", ast).as_str())
        ),
        Err(e) => pretty_print_parser_error(e, input),
    }
}
