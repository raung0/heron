use std::fmt;

use crate::frontend::{Operator, SourceLocation};

pub enum Type {
    Integer { bit_size: u8, signed: bool },
    Float { bit_size: u8 },
    Bool,
    Rune,
    Pointer,
    CArray { underlying: Box<Type> }
    Array { size: u32, underlying: Box<Type> },
    Reference { mutable: bool, lifetime: Option<char>, underlying: Box<Type> }
}

pub enum ASTValue {
    Id(String),
    String(String),
    Char(char),
    Integer(u64),
    Float(f64),
    BinExpr {
        op: Operator,
        lhs: Box<AST>,
        rhs: Box<AST>,
        has_eq: bool,
    },
    Ref {
        mutable: bool,
        lifetime: Option<char>,
        v: Box<AST>,
    },
    Deref(Box<AST>),
    ExprList(Vec<Box<AST>>),
    ExprListNoScope(Vec<Box<AST>>),
    If {
        cond: Box<AST>,
        decl: Option<Box<AST>>,
        body: Box<AST>,
        else_: Option<Box<AST>>,
    },
    While {
        cond: Box<AST>,
        decl: Option<Box<AST>>,
        body: Box<AST>,
    },
    For {
        bindings: Vec<Box<AST>>,
        iter: Box<AST>,
        body: Box<AST>,
    },
    Pub(Box<AST>),
    Set(String, Box<AST>),
    Declaration(String, Box<AST>),
    DeclarationConstexpr(String, Box<AST>),
    Type(Box<Type>),
}

pub struct AST {
    pub location: SourceLocation,
    pub v: ASTValue,
}

impl AST {
    pub fn from(location: SourceLocation, v: ASTValue) -> Box<Self> {
        Box::new(Self { location, v })
    }

    pub fn pretty_format(s: &str) -> String {
        #[derive(Debug)]
        enum Tok {
            LParen,
            RParen,
            Bracket(String),
            Atom(String),
        }

        fn tokenize(input: &str) -> Vec<Tok> {
            let mut t = Vec::new();
            let mut it = input.chars().peekable();

            while let Some(&c) = it.peek() {
                match c {
                    '(' => {
                        it.next();
                        t.push(Tok::LParen);
                    }
                    ')' => {
                        it.next();
                        t.push(Tok::RParen);
                    }
                    '[' => {
                        let mut buf = String::new();
                        let mut depth = 0usize;
                        while let Some(ch) = it.next() {
                            buf.push(ch);
                            match ch {
                                '[' => depth += 1,
                                ']' => {
                                    depth = depth.saturating_sub(1);
                                    if depth == 0 {
                                        break;
                                    }
                                }
                                _ => {}
                            }
                        }
                        t.push(Tok::Bracket(buf));
                    }
                    c if c.is_whitespace() => {
                        while let Some(&wc) = it.peek() {
                            if wc.is_whitespace() {
                                it.next();
                            } else {
                                break;
                            }
                        }
                    }
                    _ => {
                        let mut buf = String::new();
                        while let Some(&ac) = it.peek() {
                            if ac == '(' || ac == ')' || ac.is_whitespace() || ac == '[' {
                                break;
                            }
                            buf.push(ac);
                            it.next();
                        }
                        if !buf.is_empty() {
                            t.push(Tok::Atom(buf));
                        }
                    }
                }
            }
            t
        }

        struct Frame {
            align_col: usize,
            first_done: bool,
        }

        fn current_col(s: &str) -> usize {
            s.rsplit('\n')
                .next()
                .map(|ln| ln.chars().count())
                .unwrap_or(0)
        }

        let toks = tokenize(s);
        let mut out = String::new();
        let mut stack: Vec<Frame> = Vec::new();
        let mut i = 0usize;

        while i < toks.len() {
            match &toks[i] {
                Tok::LParen => {
                    if let Some(fr) = stack.last_mut() {
                        if fr.first_done {
                            out.push('\n');
                            for _ in 0..fr.align_col {
                                out.push(' ');
                            }
                        } else {
                            fr.first_done = true;
                        }
                    }

                    out.push('(');
                    stack.push(Frame {
                        align_col: 0,
                        first_done: false,
                    });

                    if let Some(next) = toks.get(i + 1) {
                        match next {
                            Tok::Atom(h) => {
                                out.push_str(h);
                                let col = current_col(&out) + 1;
                                if let Some(fr) = stack.last_mut() {
                                    fr.align_col = col;
                                    fr.first_done = true;
                                }
                                out.push(' ');
                                i += 1;

                                if let Some(second) = toks.get(i + 1) {
                                    match second {
                                        Tok::Atom(s) | Tok::Bracket(s) => {
                                            if !matches!(second, Tok::RParen) {
                                                out.push_str(s);
                                                i += 1;
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            Tok::Bracket(b) => {
                                out.push_str(b);
                                let col = current_col(&out) + 1;
                                if let Some(fr) = stack.last_mut() {
                                    fr.align_col = col;
                                    fr.first_done = true;
                                }
                                out.push(' ');
                                i += 1;
                            }
                            _ => {}
                        }
                    }
                }

                Tok::RParen => {
                    stack.pop();
                    out.push(')');
                }

                Tok::Atom(_) | Tok::Bracket(_) => {
                    if let Some(fr) = stack.last_mut() {
                        out.push('\n');
                        for _ in 0..fr.align_col {
                            out.push(' ');
                        }
                        out.push_str(match &toks[i] {
                            Tok::Atom(s) | Tok::Bracket(s) => s,
                            _ => unreachable!(),
                        });
                    } else {
                        if !out.is_empty() {
                            out.push('\n');
                        }
                        out.push_str(match &toks[i] {
                            Tok::Atom(s) | Tok::Bracket(s) => s,
                            _ => unreachable!(),
                        });
                    }
                }
            }
            i += 1;
        }

        out
    }
}

impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ASTValue::*;
        match &self.v {
            Id(v) => write!(f, "{}", v),
            String(v) => write!(f, "\"{}\"", v.escape_default()),
            Char(v) => {
                if v.is_control() || v.is_whitespace() {
                    write!(f, "'\\x{:02X}'", *v as u8)
                } else {
                    write!(f, "'{}'", v.escape_default())
                }
            }
            Integer(v) => write!(f, "{:?}", v),
            Float(v) => write!(f, "{:?}", v),
            BinExpr {
                op,
                lhs,
                rhs,
                has_eq,
            } => {
                write!(
                    f,
                    "(BinExp [op: {}, has_eq: {}] {} {})",
                    op, has_eq, lhs, rhs
                )
            }
            Ref {
                mutable,
                lifetime,
                v,
            } => {
                write!(
                    f,
                    "(Ref [mut: {}, lifetime: {:?}] {})",
                    mutable, lifetime, v
                )
            }
            Deref(v) => {
                write!(f, "(Deref {})", v)
            }
            ExprList(v) => {
                let folded = v
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(f, "(ExprList {})", folded)
            }
            ExprListNoScope(v) => {
                let folded = v
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(f, "(ExprListNoScope {})", folded)
            }
            If {
                cond,
                decl,
                body,
                else_,
            } => {
                let decl_s = match decl {
                    Some(d) => d.to_string(),
                    None => "None".to_string(),
                };
                let else_s = match else_ {
                    Some(e) => e.to_string(),
                    None => "None".to_string(),
                };
                write!(f, "(If {} {} {} {})", decl_s, cond, body, else_s)
            }
            While { cond, decl, body } => {
                let decl_s = match decl {
                    Some(d) => d.to_string(),
                    None => "None".to_string(),
                };
                write!(f, "(If {} {} {})", decl_s, cond, body)
            }
            For {
                bindings,
                iter,
                body,
            } => {
                let bindings_s = bindings
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(f, "(For {} {} {})", bindings_s, iter, body)
            }
            Pub(node) => {
                write!(f, "(Pub {})", node)
            }
            Set(s, v) => {
                write!(f, "(Set {:?} {})", s, v)
            }
            Declaration(name, value) => {
                write!(f, "(Declaration {:?} {})", name, value)
            }
            DeclarationConstexpr(name, value) => {
                write!(f, "(DeclarationConstexpr {:?} {})", name, value)
            }
        }
    }
}
