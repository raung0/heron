use std::fmt;

use crate::frontend::{Operator, SourceLocation};

#[derive(Clone, Debug, PartialEq)]
pub enum GenericArg {
    Type(Box<Type>),
    Expr(String),
    Name(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum GenericParam {
    Lifetime(char),
    Type {
        names: Vec<String>,
        constraint: Option<Box<Type>>,
    },
    Value {
        names: Vec<String>,
        ty: Box<Type>,
    },
}

pub struct FnParam {
    pub using: bool,
    pub names: Vec<String>,
    pub ty: Option<Box<Type>>,
    pub default: Option<Box<AST>>,
}

pub struct EnsuresClause {
    pub binders: Vec<String>,
    pub condition: Box<AST>,
}

pub enum FnBody {
    Block(Box<AST>),
    Expr(Box<AST>),
}

#[derive(Clone, Debug)]
pub enum Type {
    Id(String),
    Generic {
        base: String,
        args: Vec<GenericArg>,
    },
    Fn {
        params: Vec<Box<Type>>,
        return_type: Option<Box<Type>>,
    },
    Integer {
        bit_size: u8,
        signed: bool,
    },
    Float {
        bit_size: u8,
    },
    Bool,
    Rune,
    // ^#type#
    Pointer {
        underlying: Box<Type>,
    },
    // []#type#
    Slice {
        underlying: Box<Type>,
    },
    // [int]#type#
    Array {
        size: String,
        underlying: Box<Type>,
    },
    // [^]#type#
    CArray {
        underlying: Box<Type>,
    },
    // &#type#
    Reference {
        mutable: bool,
        lifetime: Option<char>,
        underlying: Box<Type>,
    },
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Id(a), Type::Id(b)) => a == b,
            (Type::Generic { base: ab, args: aa }, Type::Generic { base: bb, args: ba }) => {
                ab == bb && aa == ba
            }
            (
                Type::Fn { params: ap, return_type: ar },
                Type::Fn { params: bp, return_type: br },
            ) => ap == bp && ar == br,
            (
                Type::Integer {
                    bit_size: a_bits,
                    signed: a_signed,
                },
                Type::Integer {
                    bit_size: b_bits,
                    signed: b_signed,
                },
            ) => a_bits == b_bits && a_signed == b_signed,
            (Type::Float { bit_size: a }, Type::Float { bit_size: b }) => a == b,
            (Type::Bool, Type::Bool) => true,
            (Type::Rune, Type::Rune) => true,
            (Type::Pointer { underlying: a }, Type::Pointer { underlying: b }) => a == b,
            (Type::Slice { underlying: a }, Type::Slice { underlying: b }) => a == b,
            (
                Type::Array {
                    size: a_size,
                    underlying: a_under,
                },
                Type::Array {
                    size: b_size,
                    underlying: b_under,
                },
            ) => a_size == b_size && a_under == b_under,
            (Type::CArray { underlying: a }, Type::CArray { underlying: b }) => a == b,
            (
                Type::Reference {
                    mutable: am,
                    lifetime: al,
                    underlying: au,
                },
                Type::Reference {
                    mutable: bm,
                    lifetime: bl,
                    underlying: bu,
                },
            ) => am == bm && al == bl && au == bu,
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Id(name) => write!(f, "{}", name),
            Type::Generic { base, args } => {
                write!(f, "{}<", base)?;
                for (i, a) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    match a {
                        GenericArg::Type(t) => write!(f, "{}", t)?,
                        GenericArg::Expr(e) => write!(f, "{}", e)?,
                        GenericArg::Name(n) => write!(f, "{}", n)?,
                    }
                }
                write!(f, ">")
            }
            Type::Fn { params, return_type } => {
                write!(f, "fn(")?;
                for (i, p) in params.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, ")")?;
                if let Some(rt) = return_type {
                    write!(f, " -> {}", rt)?;
                }
                Ok(())
            }
            Type::Integer { bit_size, signed } => {
                write!(f, "{}{}", if *signed { "i" } else { "u" }, bit_size)
            }
            Type::Float { bit_size } => write!(f, "f{}", bit_size),
            Type::Bool => write!(f, "bool"),
            Type::Rune => write!(f, "rune"),
            Type::Pointer { underlying } => write!(f, "^{}", underlying),
            Type::Slice { underlying } => write!(f, "[]{}", underlying),
            Type::Array { size, underlying } => write!(f, "[{}]{}", size, underlying),
            Type::CArray { underlying } => write!(f, "[^]{}", underlying),
            Type::Reference {
                mutable,
                lifetime,
                underlying,
            } => {
                write!(f, "&")?;
                if let Some(l) = lifetime {
                    write!(f, "'{} ", l)?;
                }
                if *mutable {
                    write!(f, "mut ")?;
                }
                write!(f, "{}", underlying)
            }
        }
    }
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
    SetMulti {
        names: Vec<String>,
        values: Vec<Box<AST>>,
    },
    DeclarationMulti {
        names: Vec<String>,
        types: Vec<Box<Type>>,
        values: Option<Vec<Box<AST>>>,
        constexpr: bool,
    },
    Type(Box<Type>),
    Fn {
        generics: Vec<GenericParam>,
        params: Vec<FnParam>,
        return_type: Option<Box<Type>>,
        throws: Vec<Box<Type>>,
        where_clause: Option<Box<AST>>,
        ensures: Vec<EnsuresClause>,
        body: FnBody,
    },
    Struct {
        generics: Vec<GenericParam>,
        extends: Option<Box<Type>>,
        body: Box<AST>,
    },
    Union {
        generics: Vec<GenericParam>,
        variants: Vec<Box<Type>>,
    },
    RawUnion {
        generics: Vec<GenericParam>,
        body: Box<AST>,
    },
    Newtype {
        underlying: Box<Type>,
        constraint: Option<String>,
    },
    Alias {
        underlying: Box<Type>,
    },
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
                    '"' => {
                        let mut buf = String::new();
                        let mut escaped = false;
                        while let Some(ch) = it.next() {
                            buf.push(ch);
                            if escaped {
                                escaped = false;
                                continue;
                            }
                            if ch == '\\' {
                                escaped = true;
                                continue;
                            }
                            if ch == '"' && buf.len() > 1 {
                                break;
                            }
                        }
                        t.push(Tok::Atom(buf));
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pretty_format_does_not_split_quoted_atoms() {
        let formatted = AST::pretty_format(r#"(Type "123 456")"#);
        assert!(
            formatted.contains(r#""123 456""#),
            "expected quoted atom to remain intact, got:\n{formatted}"
        );
    }
}

impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ASTValue::*;
        let quote_type = |t: &crate::frontend::Type| format!("\"{}\"", t);
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
            SetMulti { names, values } => {
                write!(f, "(SetMulti {:?} [", names)?;
                for (i, v) in values.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "])")
            }
            DeclarationMulti {
                names,
                types,
                values,
                constexpr,
            } => {
                let types_s = types
                    .iter()
                    .map(|t| quote_type(t.as_ref()))
                    .collect::<Vec<_>>()
                    .join(", ");
                let values_s = match values {
                    Some(values) => values
                        .iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<_>>()
                        .join(" "),
                    None => "None".to_string(),
                };
                write!(
                    f,
                    "(DeclarationMulti constexpr={} {:?} [{}] {})",
                    constexpr, names, types_s, values_s
                )
            }
            Type(t) => {
                write!(f, "(Type {})", quote_type(t.as_ref()))
            }
            Fn {
                generics,
                params,
                return_type,
                throws,
                where_clause,
                ensures,
                body,
            } => {
                write!(f, "(Fn")?;
                if !generics.is_empty() {
                    write!(f, " <{}>", generics.len())?;
                }
                write!(f, " [params: {}]", params.len())?;
                if let Some(rt) = return_type {
                    write!(f, " [ret: {}]", quote_type(rt.as_ref()))?;
                }
                if !throws.is_empty() {
                    let throws_s = throws
                        .iter()
                        .map(|t| quote_type(t.as_ref()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, " [throws: {}]", throws_s)?;
                }
                if where_clause.is_some() {
                    write!(f, " [where]")?;
                }
                if !ensures.is_empty() {
                    write!(f, " [ensures: {}]", ensures.len())?;
                }
                match body {
                    FnBody::Block(b) => write!(f, " (BodyBlock {})", b)?,
                    FnBody::Expr(e) => write!(f, " (BodyExpr {})", e)?,
                }
                write!(f, ")")
            }
            Struct {
                generics,
                extends,
                body,
            } => {
                write!(f, "(Struct")?;
                if !generics.is_empty() {
                    write!(f, " <{}>", generics.len())?;
                }
                if let Some(ex) = extends {
                    write!(f, " (extends {})", quote_type(ex.as_ref()))?;
                }
                write!(f, " {})", body)
            }
            Union {
                generics,
                variants,
            } => {
                write!(f, "(Union")?;
                if !generics.is_empty() {
                    write!(f, " <{}>", generics.len())?;
                }
                write!(f, " [")?;
                for (i, v) in variants.iter().enumerate() {
                    if i != 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", quote_type(v.as_ref()))?;
                }
                write!(f, "])")
            }
            RawUnion { generics, body } => {
                write!(f, "(RawUnion")?;
                if !generics.is_empty() {
                    write!(f, " <{}>", generics.len())?;
                }
                write!(f, " {})", body)
            }
            Newtype {
                underlying,
                constraint,
            } => {
                write!(
                    f,
                    "(Newtype {} {:?})",
                    quote_type(underlying.as_ref()),
                    constraint
                )
            }
            Alias { underlying } => {
                write!(f, "(Alias {})", quote_type(underlying.as_ref()))
            }
        }
    }
}
