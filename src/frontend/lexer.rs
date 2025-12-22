use std::fmt;
use std::fs::read_to_string;

#[derive(Clone, Debug)]
pub struct LocationRange {
    pub begin: (i32, i32),
    pub end: (i32, i32),
}

#[derive(Clone, Debug)]
pub struct SourceLocation {
    pub file: String,
    pub range: LocationRange,
}

impl SourceLocation {
    pub fn new_from_file(file: String) -> Self {
        Self {
            file,
            range: LocationRange {
                begin: (1, 1),
                end: (1, 1),
            },
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Divide,
    LessThan,
    GreaterThan,
    Not,
    Or,
    And,
    BinAnd,
    BinOr,
    BinXOR,
    Dot,
    Set,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Keyword {
    Fn,
    Package,
    Use,
    As,
    In,
    For,
    While,
    Pub,
    Mut,
    Defer,
    Where,
    Pre,
    Post,
    Ensures,
    Do,
    Extends,
    Using,
    Throws,
    Throw,
    Return,
    Match,
    Case,
    If,
    Else,
    Newtype,
    Alias,
    Struct,
    Enum,
    Union,
    RawUnion,
}

#[derive(PartialEq, Clone, Debug)]
pub enum TokenValue {
    Id(String),
    String(String),
    Char(char),
    Keyword(Keyword),
    Integer(u64),
    Float(f64),
    Op { op: Operator, has_equals: bool },
    Lifetime(char),
    Colon,
    Semicolon,
    Comma,
    Arrow,
    Ellipsis, // .. not ...
    ListInit,
    LParen,
    RParen,
    LSquirly,
    RSquirly,
    LBracket,
    RBracket,
    EOF,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub location: SourceLocation,
    pub v: TokenValue,
}

impl Token {
    pub fn new() -> Self {
        Self {
            location: SourceLocation::new_from_file("".to_string()),
            v: TokenValue::EOF,
        }
    }
}

#[derive(Debug, Clone)]
pub enum LexerError {
    InvalidString(String),
    InvalidLifetimeName(char),
    UnexpectedCharacter(char),
}

#[derive(Clone)]
pub struct Lexer {
    input: String,
    location: SourceLocation,
    input_idx: usize,
    ch: char,
    next_ch: char,
    next_is_semicolon: bool,
    last_token: Token,
}

fn parse_escape(c: &str) -> Option<char> {
    if c.starts_with('x') {
        u32::from_str_radix(&c[1..], 16)
            .ok()
            .and_then(std::char::from_u32)
    } else if c.chars().all(|ch| ch.is_ascii_digit()) {
        u32::from_str_radix(c, 8).ok().and_then(std::char::from_u32)
    } else {
        match c {
            "n" => Some('\n'),
            "r" => Some('\r'),
            "t" => Some('\t'),
            "b" => Some('\x08'),
            "e" => Some('\x1b'),
            "\"" => Some('"'),
            "'" => Some('\''),
            "\\" => Some('\\'),
            _ => None,
        }
    }
}

impl Lexer {
    pub fn new_from_file(file: String) -> Self {
        Self::new(read_to_string(&file).unwrap(), file)
    }

    pub fn new(input: String, file: String) -> Self {
        let mut lexer = Self {
            input,
            location: SourceLocation::new_from_file(file.clone()),
            input_idx: 0,
            ch: '\0',
            next_ch: '\0',
            next_is_semicolon: false,
            last_token: Token {
                location: SourceLocation::new_from_file(file),
                v: TokenValue::EOF,
            },
        };
        lexer.ch = lexer.peek(0);
        lexer.next_ch = lexer.peek(1);
        lexer
    }

    pub fn get_input(&self) -> String {
        self.input.clone()
    }

    pub fn next(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();

        if self.ch == '/' && self.peek(1) == '/' {
            while self.ch != '\n' && self.ch != '\0' {
                self.advance();
            }
            return self.next();
        }

        self.skip_whitespace();

        if self.next_is_semicolon {
            self.next_is_semicolon = false;
            let mut tok = Token {
                location: self.snapshot_location(),
                v: TokenValue::Semicolon,
            };
            self.finalize_location(&mut tok.location);
            self.last_token = tok.clone();
            return Ok(tok);
        }

        if self.ch == '\0' {
            let mut tok = Token {
                location: self.snapshot_location(),
                v: TokenValue::EOF,
            };
            self.finalize_location(&mut tok.location);
            self.last_token = tok.clone();
            return Ok(tok);
        }

        let start_loc = self.snapshot_location();

        if is_ident_start(self.ch) {
            let (ident, end_loc) = self.read_identifier();
            let v = match keyword_from_str(&ident) {
                Some(kw) => TokenValue::Keyword(kw),
                None => TokenValue::Id(ident),
            };
            let mut tok = Token {
                location: end_loc,
                v,
            };
            self.finalize_location(&mut tok.location);
            self.last_token = tok.clone();
            return Ok(tok);
        }

        if self.ch.is_ascii_digit() {
            let mut tok = self.read_number()?;
            self.finalize_location(&mut tok.location);
            self.last_token = tok.clone();
            return Ok(tok);
        }

        if self.ch == '"' {
            let mut tok = self.read_string()?;
            self.finalize_location(&mut tok.location);
            self.last_token = tok.clone();
            return Ok(tok);
        }

        let mut tok = Token {
            location: start_loc,
            v: TokenValue::EOF,
        };

        match self.ch {
            '[' => {
                tok.v = TokenValue::LBracket;
                self.advance();
            }
            ']' => {
                tok.v = TokenValue::RBracket;
                self.advance();
            }
            '{' => {
                tok.v = TokenValue::LSquirly;
                self.advance();
            }
            '}' => {
                tok.v = TokenValue::RSquirly;
                self.advance();
            }
            '(' => {
                tok.v = TokenValue::LParen;
                self.advance();
            }
            ')' => {
                tok.v = TokenValue::RParen;
                self.advance();
            }
            ',' => {
                tok.v = TokenValue::Comma;
                self.advance();
            }
            '\'' => {
                let c1 = self.peek(1);
                let c2 = self.peek(2);

                if c1 != '\\' && c2 == '\'' {
                    let ch = c1;
                    self.advance(); // '
                    self.advance(); // ch
                    self.advance(); // '
                    tok.v = TokenValue::Char(ch);
                } else if c1 == '\\' {
                    self.advance(); // '
                    self.advance(); // '\'
                    let mut esc = String::new();
                    if self.ch == 'x' {
                        esc.push('x');
                        self.advance();
                        while is_hex_digit(self.ch) {
                            esc.push(self.ch);
                            self.advance();
                        }
                    } else if self.ch.is_ascii_digit() && self.ch < '8' {
                        let mut n = 0;
                        while n < 3 && self.ch.is_ascii_digit() && self.ch < '8' {
                            esc.push(self.ch);
                            self.advance();
                            n += 1;
                        }
                    } else {
                        esc.push(self.ch);
                        self.advance();
                    }
                    if let Some(ch) = parse_escape(&esc) {
                        if self.ch != '\'' {
                            return Err(LexerError::InvalidString(
                                "Unterminated character literal".to_string(),
                            ));
                        }
                        self.advance(); // closing '
                        tok.v = TokenValue::Char(ch);
                    } else {
                        return Err(LexerError::InvalidString(format!(
                            "Invalid escape in char: \\{}",
                            esc
                        )));
                    }
                } else if c1.is_ascii_lowercase() && !c1.is_whitespace() && c2 != '\'' {
                    self.advance(); // '
                    tok.v = TokenValue::Lifetime(c1);
                    self.advance(); // letter
                } else {
                    return Err(LexerError::InvalidLifetimeName(c1));
                }
            }
            ':' => {
                tok.v = TokenValue::Colon;
                self.advance();
            }
            ';' => {
                tok.v = TokenValue::Semicolon;
                self.advance();
            }
            '.' => {
                if self.peek(1) == '{' {
                    tok.v = TokenValue::ListInit;
                    self.advance();
                    self.advance();
                } else if self.peek(1) == '.' {
                    tok.v = TokenValue::Ellipsis;
                    self.advance();
                    self.advance();
                } else {
                    tok.v = TokenValue::Op {
                        op: Operator::Dot,
                        has_equals: false,
                    };
                    self.advance();
                }
            }

            '-' => {
                if self.peek(1) == '>' {
                    tok.v = TokenValue::Arrow;
                    self.advance();
                    self.advance();
                } else if self.peek(1) == '=' {
                    tok.v = TokenValue::Op {
                        op: Operator::Sub,
                        has_equals: true,
                    };
                    self.advance();
                    self.advance();
                } else {
                    tok.v = TokenValue::Op {
                        op: Operator::Sub,
                        has_equals: false,
                    };
                    self.advance();
                }
            }

            '+' => {
                if self.peek(1) == '=' {
                    tok.v = TokenValue::Op {
                        op: Operator::Add,
                        has_equals: true,
                    };
                    self.advance();
                    self.advance();
                } else {
                    tok.v = TokenValue::Op {
                        op: Operator::Add,
                        has_equals: false,
                    };
                    self.advance();
                }
            }

            '*' => {
                if self.peek(1) == '=' {
                    tok.v = TokenValue::Op {
                        op: Operator::Mul,
                        has_equals: true,
                    };
                    self.advance();
                    self.advance();
                } else {
                    tok.v = TokenValue::Op {
                        op: Operator::Mul,
                        has_equals: false,
                    };
                    self.advance();
                }
            }

            '/' => {
                if self.peek(1) == '=' {
                    tok.v = TokenValue::Op {
                        op: Operator::Divide,
                        has_equals: true,
                    };
                    self.advance();
                    self.advance();
                } else {
                    tok.v = TokenValue::Op {
                        op: Operator::Divide,
                        has_equals: false,
                    };
                    self.advance();
                }
            }

            '=' => {
                if self.peek(1) == '=' {
                    tok.v = TokenValue::Op {
                        op: Operator::Set,
                        has_equals: true, // "=="
                    };
                    self.advance();
                    self.advance();
                } else {
                    tok.v = TokenValue::Op {
                        op: Operator::Set,
                        has_equals: false, // "="
                    };
                    self.advance();
                }
            }

            '<' => {
                if self.peek(1) == '=' {
                    tok.v = TokenValue::Op {
                        op: Operator::LessThan,
                        has_equals: true, // "<="
                    };
                    self.advance();
                    self.advance();
                } else {
                    tok.v = TokenValue::Op {
                        op: Operator::LessThan,
                        has_equals: false, // "<"
                    };
                    self.advance();
                }
            }

            '>' => {
                if self.peek(1) == '=' {
                    tok.v = TokenValue::Op {
                        op: Operator::GreaterThan,
                        has_equals: true, // ">="
                    };
                    self.advance();
                    self.advance();
                } else {
                    tok.v = TokenValue::Op {
                        op: Operator::GreaterThan,
                        has_equals: false, // ">"
                    };
                    self.advance();
                }
            }

            '!' => {
                if self.peek(1) == '=' {
                    tok.v = TokenValue::Op {
                        op: Operator::Not,
                        has_equals: true, // "!="
                    };
                    self.advance();
                    self.advance();
                } else {
                    tok.v = TokenValue::Op {
                        op: Operator::Not,
                        has_equals: false, // "!"
                    };
                    self.advance();
                }
            }

            '|' => {
                if self.peek(1) == '|' {
                    tok.v = TokenValue::Op {
                        op: Operator::Or,
                        has_equals: false,
                    };
                    self.advance();
                    self.advance();
                } else if self.peek(1) == '=' {
                    tok.v = TokenValue::Op {
                        op: Operator::BinOr,
                        has_equals: true, // "|="
                    };
                    self.advance();
                    self.advance();
                } else {
                    tok.v = TokenValue::Op {
                        op: Operator::BinOr,
                        has_equals: false, // "|"
                    };
                    self.advance();
                }
            }

            '&' => {
                if self.peek(1) == '&' {
                    tok.v = TokenValue::Op {
                        op: Operator::And,
                        has_equals: false,
                    };
                    self.advance();
                    self.advance();
                } else if self.peek(1) == '=' {
                    tok.v = TokenValue::Op {
                        op: Operator::BinAnd,
                        has_equals: true, // "&="
                    };
                    self.advance();
                    self.advance();
                } else {
                    tok.v = TokenValue::Op {
                        op: Operator::BinAnd,
                        has_equals: false, // "&"
                    };
                    self.advance();
                }
            }

            '^' => {
                if self.peek(1) == '=' {
                    tok.v = TokenValue::Op {
                        op: Operator::BinXOR,
                        has_equals: true, // "^="
                    };
                    self.advance();
                    self.advance();
                } else {
                    tok.v = TokenValue::Op {
                        op: Operator::BinXOR,
                        has_equals: false, // "^"
                    };
                    self.advance();
                }
            }

            _ => {
                return Err(LexerError::UnexpectedCharacter(self.ch));
            }
        }

        self.finalize_location(&mut tok.location);
        self.last_token = tok.clone();
        Ok(tok)
    }

    fn snapshot_location(&self) -> SourceLocation {
        let mut loc = self.location.clone();
        loc.range.end = loc.range.begin;
        loc
    }

    fn finalize_location(&self, loc: &mut SourceLocation) {
        self.mark_progress(loc);
    }

    fn mark_progress(&self, loc: &mut SourceLocation) {
        loc.range.end = self.location.range.begin;
    }

    fn advance_with_progress(&mut self, loc: &mut SourceLocation) {
        self.mark_progress(loc);
        self.advance();
    }

    fn peek(&self, offset: usize) -> char {
        self.input[self.input_idx..]
            .chars()
            .nth(offset)
            .unwrap_or('\0')
    }

    fn advance(&mut self) {
        if self.input_idx >= self.input.len() {
            self.ch = '\0';
            self.next_ch = '\0';
            return;
        }

        if self.ch == '\n' {
            self.location.range.begin.1 += 1;
            self.location.range.begin.0 = 1;
        } else {
            self.location.range.begin.0 += 1;
        }

        self.input_idx += self.ch.len_utf8();
        self.ch = self.peek(0);
        self.next_ch = self.peek(1);
    }

    fn should_emit_semicolon(&self) -> bool {
        !(self.last_token.v == TokenValue::Comma
            || self.last_token.v == TokenValue::LBracket
            || self.last_token.v == TokenValue::LParen
            || self.last_token.v == TokenValue::LSquirly)
    }

    fn skip_whitespace(&mut self) {
        while (self.ch == '\n' || self.ch == ' ' || self.ch == '\t') && self.ch != '\0' {
            if self.ch == '\n' && self.should_emit_semicolon() {
                self.next_is_semicolon = true;
                self.advance();
                return;
            }
            self.advance();
        }
    }

    fn read_string(&mut self) -> Result<Token, LexerError> {
        let mut src = self.snapshot_location();

        if self.ch != '"' {
            return Err(LexerError::InvalidString(
                "Strings must begin with a quote".to_string(),
            ));
        }
        self.advance();

        let mut data = String::new();
        while self.ch != '"' && self.ch != '\0' {
            if self.ch == '\\' {
                self.advance(); // consume '\'
                let mut esc = String::new();
                if self.ch == 'x' {
                    esc.push('x');
                    self.advance();
                    while is_hex_digit(self.ch) {
                        esc.push(self.ch);
                        self.advance();
                    }
                } else if self.ch.is_ascii_digit() && self.ch < '8' {
                    let mut n = 0;
                    while n < 3 && self.ch.is_ascii_digit() && self.ch < '8' {
                        esc.push(self.ch);
                        self.advance();
                        n += 1;
                    }
                } else {
                    esc.push(self.ch);
                    self.advance();
                }
                if let Some(ch) = parse_escape(&esc) {
                    data.push(ch);
                } else {
                    return Err(LexerError::InvalidString(format!(
                        "Invalid escape: \\{}",
                        esc
                    )));
                }
                self.mark_progress(&mut src);
                continue;
            }

            data.push(self.ch);
            self.advance_with_progress(&mut src);
        }

        if self.ch != '"' {
            return Err(LexerError::InvalidString(
                "Strings must end with a quote".to_string(),
            ));
        }
        self.advance();

        self.mark_progress(&mut src);

        Ok(Token {
            location: src,
            v: TokenValue::String(data),
        })
    }

    fn read_identifier(&mut self) -> (String, SourceLocation) {
        let mut out = String::new();
        let mut loc = self.snapshot_location();

        while is_ident_continue(self.ch) {
            out.push(self.ch);
            self.advance();
        }

        self.finalize_location(&mut loc);

        (out, loc)
    }

    fn read_number(&mut self) -> Result<Token, LexerError> {
        let mut src = self.snapshot_location();

        let mut raw = String::new();

        let base = 10u32;
        let mut is_float = false;

        if self.ch == '0' {
            let n1 = self.peek(1);
            if n1 == 'x' || n1 == 'X' {
                raw.push_str("0x");
                self.advance(); // 0
                self.advance(); // x
                while is_hex_digit(self.ch) {
                    raw.push(self.ch);
                    self.advance_with_progress(&mut src);
                }
                let val = u64::from_str_radix(raw.trim_start_matches("0x"), 16).unwrap_or(0);
                self.finalize_location(&mut src);
                return Ok(Token {
                    location: src,
                    v: TokenValue::Integer(val),
                });
            } else if n1 == 'b' || n1 == 'B' {
                raw.push_str("0b");
                self.advance(); // 0
                self.advance(); // b
                while self.ch == '0' || self.ch == '1' {
                    raw.push(self.ch);
                    self.advance_with_progress(&mut src);
                }
                let val = u64::from_str_radix(raw.trim_start_matches("0b"), 2).unwrap_or(0);
                self.finalize_location(&mut src);
                return Ok(Token {
                    location: src,
                    v: TokenValue::Integer(val),
                });
            } else if n1 == 'o' || n1 == 'O' {
                raw.push_str("0o");
                self.advance(); // 0
                self.advance(); // o
                while self.ch >= '0' && self.ch <= '7' {
                    raw.push(self.ch);
                    self.advance_with_progress(&mut src);
                }
                let val = u64::from_str_radix(raw.trim_start_matches("0o"), 8).unwrap_or(0);
                self.finalize_location(&mut src);
                return Ok(Token {
                    location: src,
                    v: TokenValue::Integer(val),
                });
            }
        }

        while self.ch.is_ascii_digit() {
            raw.push(self.ch);
            self.advance_with_progress(&mut src);
        }

        if self.ch == '.' && self.peek(1).is_ascii_digit() {
            is_float = true;
            raw.push('.');
            self.advance(); // '.'
            while self.ch.is_ascii_digit() {
                raw.push(self.ch);
                self.advance_with_progress(&mut src);
            }
        }

        if self.ch == 'e' || self.ch == 'E' {
            is_float = true;
            raw.push(self.ch);
            self.advance();
            if self.ch == '+' || self.ch == '-' {
                raw.push(self.ch);
                self.advance();
            }
            while self.ch.is_ascii_digit() {
                raw.push(self.ch);
                self.advance_with_progress(&mut src);
            }
        }

        if is_float {
            let val = raw.parse::<f64>().unwrap_or(0.0);
            self.finalize_location(&mut src);
            Ok(Token {
                location: src,
                v: TokenValue::Float(val),
            })
        } else {
            let val = match base {
                10 => raw.parse::<u64>().unwrap_or(0),
                _ => unreachable!(),
            };
            self.finalize_location(&mut src);
            Ok(Token {
                location: src,
                v: TokenValue::Integer(val),
            })
        }
    }
}

fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic()
}

fn is_ident_continue(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric()
}

fn is_hex_digit(c: char) -> bool {
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
}

fn keyword_from_str(s: &str) -> Option<Keyword> {
    Some(match s {
        "fn" => Keyword::Fn,
        "package" => Keyword::Package,
        "use" => Keyword::Use,
        "as" => Keyword::As,
        "in" => Keyword::In,
        "for" => Keyword::For,
        "while" => Keyword::While,
        "pub" => Keyword::Pub,
        "mut" => Keyword::Mut,
        "defer" => Keyword::Defer,
        "where" => Keyword::Where,
        "pre" => Keyword::Pre,
        "post" => Keyword::Post,
        "ensures" => Keyword::Ensures,
        "do" => Keyword::Do,
        "extends" => Keyword::Extends,
        "using" => Keyword::Using,
        "throws" => Keyword::Throws,
        "throw" => Keyword::Throw,
        "return" => Keyword::Return,
        "match" => Keyword::Match,
        "case" => Keyword::Case,
        "if" => Keyword::If,
        "else" => Keyword::Else,
        "newtype" => Keyword::Newtype,
        "alias" => Keyword::Alias,
        "struct" => Keyword::Struct,
        "enum" => Keyword::Enum,
        "union" => Keyword::Union,
        "raw_union" => Keyword::RawUnion,
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::read_to_string;

    fn expect_lexer_error(source: &str) -> LexerError {
        let mut lx = Lexer::new(source.to_string(), "<test>".to_string());
        match lx.next() {
            Ok(tok) => panic!("expected lexer to fail, but produced token {:?}", tok.v),
            Err(err) => err,
        }
    }

    fn lex_values(source: &str) -> Vec<TokenValue> {
        let mut lx = Lexer::new(source.to_string(), "<test>".to_string());
        let mut values = Vec::new();
        loop {
            match lx.next() {
                Ok(tok) => {
                    let value = tok.v.clone();
                    let is_eof = matches!(value, TokenValue::EOF);
                    values.push(value);
                    if is_eof {
                        break;
                    }
                }
                Err(err) => panic!("unexpected lexer error: {:?}", err),
            }
        }
        values
    }

    #[test]
    fn invalid_lifetime_name_produces_error() {
        match expect_lexer_error("'1") {
            LexerError::InvalidLifetimeName(ch) => assert_eq!(ch, '1'),
            other => panic!("expected InvalidLifetimeName, got {:?}", other),
        }
    }

    #[test]
    fn unterminated_string_literal_reports_error() {
        match expect_lexer_error("\"unterminated") {
            LexerError::InvalidString(msg) => {
                assert!(
                    msg.contains("end with a quote"),
                    "expected unterminated string message, got {}",
                    msg
                );
            }
            other => panic!(
                "expected InvalidString for unterminated literal, got {:?}",
                other
            ),
        }
    }

    #[test]
    fn tokenizes_identifiers_literals_and_ops() {
        let toks = lex_values("foo += 42.5");
        assert_eq!(toks.len(), 4, "expected id, op, literal, EOF");
        match &toks[0] {
            TokenValue::Id(s) => assert_eq!(s, "foo"),
            other => panic!("expected first token to be Id, got {:?}", other),
        }
        match &toks[1] {
            TokenValue::Op { op, has_equals } => {
                assert_eq!(op, &Operator::Add);
                assert_eq!(has_equals, &true);
            }
            other => panic!("expected '+=' token, got {:?}", other),
        }
        match &toks[2] {
            TokenValue::Float(v) => assert!((*v - 42.5).abs() < 1e-9),
            other => panic!("expected float literal, got {:?}", other),
        }
        assert!(
            matches!(toks[3], TokenValue::EOF),
            "expected EOF terminator token"
        );
    }

    #[test]
    fn inserts_semicolon_after_newline_when_needed() {
        let toks = lex_values("foo\nbar");
        assert_eq!(toks.len(), 4, "expected foo ; bar EOF");
        assert!(matches!(&toks[0], TokenValue::Id(s) if s == "foo"));
        assert!(matches!(&toks[1], TokenValue::Semicolon));
        assert!(matches!(&toks[2], TokenValue::Id(s) if s == "bar"));
        assert!(matches!(toks[3], TokenValue::EOF));
    }

    #[test]
    fn newline_after_lparen_does_not_emit_semicolon() {
        let toks = lex_values("foo(\nbar)");
        assert_eq!(
            toks.len(),
            5,
            "expected foo ( bar ) EOF without inserted semicolon"
        );
        assert!(matches!(&toks[0], TokenValue::Id(s) if s == "foo"));
        assert!(matches!(toks[1], TokenValue::LParen));
        assert!(matches!(&toks[2], TokenValue::Id(s) if s == "bar"));
        assert!(matches!(toks[3], TokenValue::RParen));
        assert!(matches!(toks[4], TokenValue::EOF));
    }

    #[test]
    fn tokens_report_correct_range_end() {
        let mut lx = Lexer::new("foo + 42".to_string(), "<test>".to_string());

        let id = lx.next().expect("id token");
        assert!(matches!(&id.v, TokenValue::Id(s) if s == "foo"));
        assert_eq!(id.location.range.begin, (1, 1));
        assert_eq!(id.location.range.end, (4, 1));

        let plus = lx.next().expect("plus token");
        assert!(matches!(
            plus.v,
            TokenValue::Op {
                op: Operator::Add,
                has_equals: false
            }
        ));
        assert_eq!(plus.location.range.begin, (5, 1));
        assert_eq!(plus.location.range.end, (6, 1));

        let int = lx.next().expect("integer token");
        assert!(matches!(int.v, TokenValue::Integer(42)));
        assert_eq!(int.location.range.begin, (7, 1));
        assert_eq!(int.location.range.end, (9, 1));
    }

    #[test]
    fn testing_he_has_semicolon_between_point_and_child() {
        let input = read_to_string("testing.he").expect("read testing.he");
        let values = lex_values(&input);

        let mut saw_point = false;
        let mut saw_end_point_struct = false;
        let mut saw_separator_after_point = false;
        for v in values {
            if !saw_point {
                if v == TokenValue::Id("Point".to_string()) {
                    saw_point = true;
                }
                continue;
            }

            if !saw_end_point_struct {
                if v == TokenValue::Semicolon {
                    // There are many semicolons inside, but the explicit `};` ends the declaration.
                    saw_end_point_struct = true;
                }
                continue;
            }

            if !saw_separator_after_point {
                if v == TokenValue::Semicolon {
                    saw_separator_after_point = true;
                }
                continue;
            }

            if v == TokenValue::Id("Child".to_string()) {
                return;
            }
        }

        panic!("did not observe semicolon-separated `Point` then `Child` in token stream");
    }
}
