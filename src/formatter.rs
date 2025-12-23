use crate::frontend::{
    AST, ASTValue, EnsuresClause, FnBody, FnParam, GenericArg, GenericParam, InitializerItem,
    MatchBinder, MatchCase, MatchCasePattern, Operator, PostClause, SourceLocation, Trivia,
    TriviaKind, Type,
};
use serde::de::{self, Visitor};
use serde::{Deserialize, Deserializer};
use std::cell::Cell;

#[derive(Clone, Debug)]
pub struct FormatOptions {
    pub use_tabs: bool,
    pub tab_size: usize,
    pub separate_imports: bool,
    pub collapse_empty_lines: bool,
    pub line_width: usize,
    pub trailing_newline: bool,
}

#[derive(Debug, Default, Deserialize)]
pub struct FormatConfig {
    #[serde(default, deserialize_with = "deserialize_bool_opt")]
    pub use_tabs: Option<bool>,
    #[serde(default, deserialize_with = "deserialize_usize_opt")]
    pub tab_size: Option<usize>,
    #[serde(default, deserialize_with = "deserialize_bool_opt")]
    pub separate_imports: Option<bool>,
    #[serde(default, deserialize_with = "deserialize_bool_opt")]
    pub collapse_empty_lines: Option<bool>,
    #[serde(default, deserialize_with = "deserialize_usize_opt")]
    pub line_width: Option<usize>,
    #[serde(default, deserialize_with = "deserialize_bool_opt")]
    pub trailing_newline: Option<bool>,
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self {
            use_tabs: true,
            tab_size: 4,
            separate_imports: false,
            collapse_empty_lines: true,
            line_width: 80,
            trailing_newline: false,
        }
    }
}

impl FormatOptions {
    pub fn apply_config(&mut self, config: &FormatConfig) -> Result<(), String> {
        if let Some(use_tabs) = config.use_tabs {
            self.use_tabs = use_tabs;
        }
        if let Some(tab_size) = config.tab_size {
            if tab_size == 0 {
                return Err("tab_size must be greater than 0".to_string());
            }
            self.tab_size = tab_size;
        }
        if let Some(separate_imports) = config.separate_imports {
            self.separate_imports = separate_imports;
        }
        if let Some(collapse_empty_lines) = config.collapse_empty_lines {
            self.collapse_empty_lines = collapse_empty_lines;
        }
        if let Some(line_width) = config.line_width {
            if line_width == 0 {
                return Err("line_width must be greater than 0".to_string());
            }
            self.line_width = line_width;
        }
        if let Some(trailing_newline) = config.trailing_newline {
            self.trailing_newline = trailing_newline;
        }
        Ok(())
    }
}

fn deserialize_bool_opt<'de, D>(deserializer: D) -> Result<Option<bool>, D::Error>
where
    D: Deserializer<'de>,
{
    struct BoolVisitor;

    impl<'de> Visitor<'de> for BoolVisitor {
        type Value = Option<bool>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a boolean or string boolean")
        }

        fn visit_bool<E>(self, value: bool) -> Result<Self::Value, E> {
            Ok(Some(value))
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            parse_bool(value)
                .map(Some)
                .ok_or_else(|| E::custom(format!("invalid boolean value: {value}")))
        }

        fn visit_string<E>(self, value: String) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            self.visit_str(&value)
        }
    }

    deserializer.deserialize_any(BoolVisitor)
}

fn deserialize_usize_opt<'de, D>(deserializer: D) -> Result<Option<usize>, D::Error>
where
    D: Deserializer<'de>,
{
    struct UsizeVisitor;

    impl<'de> Visitor<'de> for UsizeVisitor {
        type Value = Option<usize>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a positive integer")
        }

        fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E> {
            Ok(Some(value as usize))
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            value
                .trim()
                .parse::<usize>()
                .map(Some)
                .map_err(|_| E::custom(format!("invalid integer value: {value}")))
        }

        fn visit_string<E>(self, value: String) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            self.visit_str(&value)
        }
    }

    deserializer.deserialize_any(UsizeVisitor)
}

fn parse_bool(value: &str) -> Option<bool> {
    match value.trim().to_ascii_lowercase().as_str() {
        "true" | "1" | "yes" | "on" => Some(true),
        "false" | "0" | "no" | "off" => Some(false),
        _ => None,
    }
}

pub fn format_ast(ast: &AST) -> String {
    format_ast_with_options(ast, &FormatOptions::default())
}

pub fn format_ast_with_options(ast: &AST, options: &FormatOptions) -> String {
    let mut formatter = Formatter::new(&ast.trivia, options);
    formatter.format_root(ast);
    formatter.finish()
}

struct Formatter {
    out: String,
    indent: usize,
    comments: Vec<Trivia>,
    comment_idx: Cell<usize>,
    options: FormatOptions,
}

impl Formatter {
    fn new(trivia: &[Trivia], options: &FormatOptions) -> Self {
        let mut comments: Vec<Trivia> = trivia
            .iter()
            .filter(|t| matches!(t.kind, TriviaKind::LineComment))
            .cloned()
            .collect();

        comments.sort_by_key(|t| (t.location.range.begin.1, t.location.range.begin.0));

        Self {
            out: String::new(),
            indent: 0,
            comments,
            comment_idx: Cell::new(0),
            options: options.clone(),
        }
    }

    fn finish(mut self) -> String {
        if self.options.trailing_newline && !self.out.ends_with('\n') {
            self.out.push('\n');
        }
        self.out
    }

    fn format_root(&mut self, ast: &AST) {
        match &ast.v {
            ASTValue::ExprList(items) => {
                let mut last_line = self.format_stmt_list(items);
                self.emit_comments_until(None, &mut last_line);
            }
            _ => {
                self.write_indent();
                self.format_stmt(ast);
                self.out.push('\n');
                let mut last_line = self.statement_end_line(ast);
                self.emit_comments_until(None, &mut last_line);
            }
        }
    }

    fn format_stmt_list(&mut self, items: &[Box<AST>]) -> i32 {
        let mut last_line = items
            .first()
            .map(|item| item.location.range.begin.1 - 1)
            .unwrap_or(1);

        let mut prev_was_package_use = false;

        for item in items {
            let start_line = item.location.range.begin.1;
            let is_package_use = matches!(item.v, ASTValue::Package { .. } | ASTValue::Use { .. });

            if self.options.separate_imports
                && self.indent == 0
                && prev_was_package_use
                && !is_package_use
            {
                self.emit_blank_lines(2);
                last_line += 2;
            }

            self.emit_comments_until(Some(start_line), &mut last_line);

            let mut gap = start_line - last_line - 1;
            if self.options.collapse_empty_lines && gap > 1 {
                gap = 1;
            }
            if gap > 0 {
                self.emit_blank_lines(gap as usize);
            }

            self.write_indent();
            self.format_stmt(item);
            self.out.push('\n');

            last_line = self.statement_end_line(item);
            prev_was_package_use = is_package_use;
        }

        last_line
    }

    fn format_stmt(&mut self, ast: &AST) {
        match &ast.v {
            ASTValue::Package { path } => {
                self.out.push_str("package ");
                self.out.push_str(&path.join("."));
            }
            ASTValue::Use { path, alias } => {
                self.out.push_str("use ");
                self.out.push_str(&path.join("."));
                if let Some(alias) = alias {
                    self.out.push_str(" as ");
                    self.out.push_str(alias);
                }
            }
            ASTValue::Return(value) => {
                self.out.push_str("return");
                if let Some(value) = value {
                    self.out.push(' ');
                    self.out.push_str(&self.format_expr(value, 0));
                }
            }
            ASTValue::Throw(value) => {
                self.out.push_str("throw ");
                self.out.push_str(&self.format_expr(value, 0));
            }
            ASTValue::Defer(value) => {
                self.out.push_str("defer ");
                self.out.push_str(&self.format_expr(value, 0));
            }
            ASTValue::ExprList(_) => self.format_block(ast),

            ASTValue::If {
                cond,
                decl,
                body,
                else_,
            } => self.format_if(decl.as_deref(), cond, body, else_.as_deref()),
            ASTValue::While { cond, decl, body } => self.format_while(decl.as_deref(), cond, body),
            ASTValue::ForLoop {
                init,
                cond,
                step,
                body,
            } => self.format_for_loop(init.as_deref(), cond.as_deref(), step.as_deref(), body),
            ASTValue::For {
                bindings,
                iter,
                body,
            } => self.format_for(bindings, iter, body),
            ASTValue::Match {
                binder,
                scrutinee,
                cases,
            } => self.format_match(binder.as_ref(), scrutinee, cases),

            ASTValue::Pub(node) => {
                self.out.push_str("pub ");
                self.format_stmt(node);
            }

            ASTValue::Set(name, value) => {
                self.out.push_str(name);
                self.out.push_str(" = ");
                self.out.push_str(&self.format_expr(value, 0));
            }
            ASTValue::SetMulti { names, values } => {
                self.out.push_str(&names.join(", "));
                self.out.push_str(" = ");
                self.out
                    .push_str(&self.format_expr_list_inline(values, ", "));
            }

            ASTValue::DeclarationMulti {
                names,
                types,
                values,
                constexpr,
            } => {
                if *constexpr
                    && types.is_empty()
                    && names.len() == 1
                    && values.as_ref().map_or(false, |v| v.len() == 1)
                    && values
                        .as_ref()
                        .and_then(|v| v.first())
                        .map(|value| {
                            matches!(
                                value.v,
                                ASTValue::DeclarationMulti {
                                    constexpr: true,
                                    ..
                                }
                            )
                        })
                        .unwrap_or(false)
                {
                    let value = &values.as_ref().unwrap()[0];
                    self.out
                        .push_str(&self.format_namespace_chain(&names[0], value));
                } else if values
                    .as_ref()
                    .map_or(false, |v| v.len() == 1 && self.is_stmt_value(&v[0]))
                {
                    self.out.push_str(&names.join(", "));
                    if types.is_empty() {
                        self.out.push_str(if *constexpr { " :: " } else { " := " });
                    } else {
                        self.out.push_str(": ");
                        self.out.push_str(
                            &types
                                .iter()
                                .map(|t| self.format_type(t))
                                .collect::<Vec<_>>()
                                .join(", "),
                        );
                        self.out.push_str(if *constexpr { " : " } else { " = " });
                    }
                    let value = &values.as_ref().unwrap()[0];
                    self.format_stmt(value);
                } else {
                    self.format_declaration_multi(names, types, values.as_deref(), *constexpr);
                }
            }

            ASTValue::Declaration(name, value) => {
                self.out.push_str(name);
                self.out.push_str(" := ");
                if self.is_stmt_value(value) {
                    self.format_stmt(value);
                } else {
                    self.out.push_str(&self.format_expr(value, 0));
                }
            }
            ASTValue::DeclarationConstexpr(name, value) => {
                self.out.push_str(name);
                self.out.push_str(" :: ");
                if self.is_stmt_value(value) {
                    self.format_stmt(value);
                } else {
                    self.out.push_str(&self.format_expr(value, 0));
                }
            }

            ASTValue::Fn {
                generics,
                params,
                return_type,
                throws,
                pre,
                post,
                where_clause,
                ensures,
                body,
            } => self.format_fn(
                generics,
                params,
                return_type.as_deref(),
                throws,
                pre,
                post.as_ref(),
                where_clause.as_deref(),
                ensures,
                body,
            ),

            ASTValue::Struct {
                generics,
                extends,
                body,
            } => self.format_struct(generics, extends.as_deref(), body),
            ASTValue::Enum { variants } => self.format_enum(variants),
            ASTValue::Union { generics, variants } => self.format_union(generics, variants),
            ASTValue::RawUnion { generics, body } => self.format_raw_union(generics, body),
            ASTValue::Newtype {
                underlying,
                constraint,
            } => self.format_newtype(underlying, constraint.as_deref()),
            ASTValue::Alias { underlying } => {
                self.out.push_str("alias ");
                self.out.push_str(&self.format_type(underlying));
            }

            _ => {
                self.out.push_str(&self.format_expr(ast, 0));
            }
        }
    }

    fn format_if(&mut self, decl: Option<&AST>, cond: &AST, body: &AST, else_: Option<&AST>) {
        self.out.push_str("if ");
        self.format_optional_decl(decl);
        self.out.push_str(&self.format_expr(cond, 0));
        self.format_block_or_do(body);

        if let Some(else_) = else_ {
            self.out.push_str(" else ");
            match &else_.v {
                ASTValue::If {
                    cond,
                    decl,
                    body,
                    else_,
                } => self.format_if(decl.as_deref(), cond, body, else_.as_deref()),
                _ => self.format_block_or_do(else_),
            }
        }
    }

    fn format_while(&mut self, decl: Option<&AST>, cond: &AST, body: &AST) {
        self.out.push_str("while ");
        self.format_optional_decl(decl);
        self.out.push_str(&self.format_expr(cond, 0));
        self.format_block_or_do(body);
    }

    fn format_for_loop(
        &mut self,
        init: Option<&AST>,
        cond: Option<&AST>,
        step: Option<&AST>,
        body: &AST,
    ) {
        self.out.push_str("for");

        if init.is_none() && step.is_none() && cond.is_some() {
            self.out.push(' ');
            self.out
                .push_str(&cond.map(|v| self.format_expr(v, 0)).unwrap_or_default());
        } else if init.is_some() || cond.is_some() || step.is_some() {
            self.out.push(' ');
            self.out
                .push_str(&init.map(|v| self.format_expr(v, 0)).unwrap_or_default());
            self.out.push_str("; ");
            self.out
                .push_str(&cond.map(|v| self.format_expr(v, 0)).unwrap_or_default());
            self.out.push_str("; ");
            self.out
                .push_str(&step.map(|v| self.format_expr(v, 0)).unwrap_or_default());
        }

        self.format_block_or_do(body);
    }

    fn format_for(&mut self, bindings: &[Box<AST>], iter: &AST, body: &AST) {
        self.out.push_str("for ");
        self.out
            .push_str(&self.format_expr_list_inline(bindings, ", "));
        self.out.push_str(" in ");
        self.out.push_str(&self.format_expr(iter, 0));
        self.format_block_or_do(body);
    }

    fn format_match(&mut self, binder: Option<&MatchBinder>, scrutinee: &AST, cases: &[MatchCase]) {
        self.out.push_str("match ");

        if let Some(binder) = binder {
            if binder.by_ref {
                self.out.push('&');
            }
            if let Some(lifetime) = binder.lifetime {
                self.out.push(' ');
                self.out.push('\'');
                self.out.push(lifetime);
            }
            if binder.mutable {
                self.out.push_str(" mut");
            }
            self.out.push(' ');
            self.out.push_str(&binder.name);
            self.out.push_str(" in ");
        }

        self.out.push_str(&self.format_expr(scrutinee, 0));
        self.out.push(' ');
        self.out.push('{');
        self.out.push('\n');

        self.indent += 1;
        for case in cases {
            self.write_indent();
            self.out.push_str("case");

            let pat = self.format_match_pattern(&case.pattern);
            if !pat.is_empty() {
                self.out.push(' ');
                self.out.push_str(&pat);
            }

            if let Some(guard) = &case.guard {
                self.out.push_str(" if ");
                self.out.push_str(&self.format_expr(guard, 0));
            }

            self.out.push_str(" -> ");

            match &case.body.v {
                ASTValue::ExprList(_) => self.format_block(&case.body),
                _ => self.out.push_str(&self.format_expr(&case.body, 0)),
            }

            self.out.push('\n');
        }
        self.indent -= 1;

        self.write_indent();
        self.out.push('}');
    }

    fn format_match_pattern(&self, pattern: &MatchCasePattern) -> String {
        match pattern {
            MatchCasePattern::Default => String::new(),
            MatchCasePattern::Exprs(exprs) => self.format_expr_list_inline(exprs, ", "),
            MatchCasePattern::Type(ty) => self.format_type(ty),
        }
    }

    fn format_declaration_multi(
        &mut self,
        names: &[String],
        types: &[Box<Type>],
        values: Option<&[Box<AST>]>,
        constexpr: bool,
    ) {
        self.out.push_str(&names.join(", "));
        if types.is_empty() && values.is_none() {
            return;
        }

        let types_s = if types.is_empty() {
            String::new()
        } else {
            types
                .iter()
                .map(|t| self.format_type(t))
                .collect::<Vec<_>>()
                .join(", ")
        };

        let values_s = values.map(|v| self.format_expr_list_inline(v, ", "));

        match (constexpr, types.is_empty(), values_s) {
            (false, true, Some(values_s)) => {
                self.out.push_str(" := ");
                self.out.push_str(&values_s);
            }
            (true, true, Some(values_s)) => {
                self.out.push_str(" :: ");
                self.out.push_str(&values_s);
            }
            (false, false, Some(values_s)) => {
                self.out.push_str(": ");
                self.out.push_str(&types_s);
                self.out.push_str(" = ");
                self.out.push_str(&values_s);
            }
            (true, false, Some(values_s)) => {
                self.out.push_str(": ");
                self.out.push_str(&types_s);
                self.out.push_str(" : ");
                self.out.push_str(&values_s);
            }
            (_, false, None) => {
                self.out.push_str(": ");
                self.out.push_str(&types_s);
            }
            _ => {}
        }
    }

    fn format_fn(
        &mut self,
        generics: &[GenericParam],
        params: &[FnParam],
        return_type: Option<&Type>,
        throws: &[Box<Type>],
        pre: &[Box<AST>],
        post: Option<&PostClause>,
        where_clause: Option<&AST>,
        ensures: &[EnsuresClause],
        body: &FnBody,
    ) {
        let params_entries = self.format_param_entries(params);

        let mut signature_inline = String::from("fn");
        if !generics.is_empty() {
            let generics_s = generics
                .iter()
                .map(|g| g.to_string())
                .collect::<Vec<_>>()
                .join("; ");
            signature_inline.push('<');
            signature_inline.push_str(&generics_s);
            signature_inline.push('>');
        }
        if !params_entries.is_empty() {
            signature_inline.push_str(" (");
            signature_inline.push_str(&params_entries.join("; "));
            signature_inline.push(')');
        }
        if let Some(return_type) = return_type {
            signature_inline.push_str(" -> ");
            signature_inline.push_str(&self.format_type(return_type));
        }

        let wraps_signature = self.line_width_exceeded(self.indent, &signature_inline);

        self.out.push_str("fn");
        if !generics.is_empty() {
            let generics_s = generics
                .iter()
                .map(|g| g.to_string())
                .collect::<Vec<_>>()
                .join("; ");
            self.out.push('<');
            self.out.push_str(&generics_s);
            self.out.push('>');
        }

        if !params_entries.is_empty() {
            self.out.push_str(" (");
            if wraps_signature {
                self.out.push('\n');
                let param_indent = self.indent + 1;
                for entry in &params_entries {
                    self.write_indent_level(param_indent);
                    self.out.push_str(entry);
                    self.out.push('\n');
                }
                self.write_indent();
                self.out.push(')');
            } else {
                self.out.push_str(&params_entries.join("; "));
                self.out.push(')');
            }
        }

        if let Some(return_type) = return_type {
            self.out.push_str(" -> ");
            self.out.push_str(&self.format_type(return_type));
        }

        let multiline_clauses = wraps_signature
            || !pre.is_empty()
            || post.is_some()
            || where_clause.is_some()
            || !ensures.is_empty()
            || (throws.len() > 1);

        let clause_indent = self.indent + 1;

        if !throws.is_empty() {
            let throws_s = throws
                .iter()
                .map(|t| self.format_type(t))
                .collect::<Vec<_>>()
                .join(", ");
            self.emit_clause(multiline_clauses, clause_indent, "throws ", |f| {
                f.out.push_str(&throws_s);
            });
        }

        if !pre.is_empty() {
            let pre_s = self.format_expr_list_inline(pre, ", ");
            self.emit_clause(multiline_clauses, clause_indent, "pre ", |f| {
                f.out.push_str(&pre_s);
            });
        }

        if let Some(post) = post {
            self.emit_clause(multiline_clauses, clause_indent, "post ", |f| {
                if let Some(ret) = &post.return_id {
                    f.out.push_str(ret);
                    f.out.push_str(": ");
                }
                f.out
                    .push_str(&f.format_expr_list_inline(&post.conditions, ", "));
            });
        }

        if let Some(where_clause) = where_clause {
            let where_s = self.format_expr(where_clause, 0);
            self.emit_clause(multiline_clauses, clause_indent, "where ", |f| {
                f.out.push_str(&where_s);
            });
        }

        for ensures_clause in ensures {
            self.emit_clause(multiline_clauses, clause_indent, "ensures", |f| {
                if !ensures_clause.binders.is_empty() {
                    f.out.push('<');
                    f.out.push_str(&ensures_clause.binders.join(", "));
                    f.out.push('>');
                }
                f.out.push(' ');
                f.out.push_str(&f.format_expr(&ensures_clause.condition, 0));
            });
        }

        match body {
            FnBody::Block(block) => {
                let is_block = matches!(block.v, ASTValue::ExprList(_));
                match (multiline_clauses, is_block) {
                    (true, true) => {
                        self.out.push('\n');
                        self.write_indent();
                        self.format_block(block);
                    }
                    (false, true) => {
                        self.out.push(' ');
                        self.format_block(block);
                    }
                    (true, false) => {
                        self.out.push('\n');
                        self.write_indent();
                        self.out.push_str("do ");
                        self.out.push_str(&self.format_expr(block, 0));
                    }
                    (false, false) => {
                        self.out.push_str(" do ");
                        self.out.push_str(&self.format_expr(block, 0));
                    }
                }
            }
            FnBody::Expr(expr) => {
                if multiline_clauses {
                    self.out.push('\n');
                    self.write_indent();
                    self.out.push_str("do ");
                    self.out.push_str(&self.format_expr(expr, 0));
                } else {
                    self.out.push_str(" do ");
                    self.out.push_str(&self.format_expr(expr, 0));
                }
            }
        }
    }

    fn format_struct(&mut self, generics: &[GenericParam], extends: Option<&Type>, body: &AST) {
        self.out.push_str("struct");

        if !generics.is_empty() {
            let generics_s = generics
                .iter()
                .map(|g| g.to_string())
                .collect::<Vec<_>>()
                .join("; ");
            self.out.push('<');
            self.out.push_str(&generics_s);
            self.out.push('>');
        }

        if let Some(extends) = extends {
            self.out.push_str(" extends ");
            self.out.push_str(&self.format_type(extends));
        }

        if matches!(body.v, ASTValue::ExprList(_)) {
            self.out.push(' ');
            self.format_block(body);
        } else {
            self.out.push_str(" { }");
        }
    }

    fn format_enum(&mut self, variants: &[crate::frontend::EnumVariant]) {
        self.out.push_str("enum {");
        self.out.push('\n');

        self.indent += 1;
        for variant in variants {
            self.write_indent();
            self.out.push_str(&variant.name);
            if let Some(value) = &variant.value {
                self.out.push_str(" = ");
                self.out.push_str(&self.format_expr(value, 0));
            }
            self.out.push('\n');
        }
        self.indent -= 1;

        self.write_indent();
        self.out.push('}');
    }

    fn format_union(&mut self, generics: &[GenericParam], variants: &[Box<Type>]) {
        self.out.push_str("union");

        if !generics.is_empty() {
            let generics_s = generics
                .iter()
                .map(|g| g.to_string())
                .collect::<Vec<_>>()
                .join("; ");
            self.out.push('<');
            self.out.push_str(&generics_s);
            self.out.push('>');
        }

        self.out.push_str(" {");
        self.out.push('\n');

        self.indent += 1;
        for variant in variants {
            self.write_indent();
            self.out.push_str(&self.format_type(variant));
            self.out.push('\n');
        }
        self.indent -= 1;

        self.write_indent();
        self.out.push('}');
    }

    fn format_raw_union(&mut self, generics: &[GenericParam], body: &AST) {
        self.out.push_str("raw_union");

        if !generics.is_empty() {
            let generics_s = generics
                .iter()
                .map(|g| g.to_string())
                .collect::<Vec<_>>()
                .join("; ");
            self.out.push('<');
            self.out.push_str(&generics_s);
            self.out.push('>');
        }

        if matches!(body.v, ASTValue::ExprList(_)) {
            self.out.push(' ');
            self.format_block(body);
        } else {
            self.out.push_str(" { }");
        }
    }

    fn format_newtype(&mut self, underlying: &Type, constraint: Option<&str>) {
        self.out.push_str("newtype ");
        self.out.push_str(&self.format_type(underlying));
        if let Some(constraint) = constraint {
            self.out.push('(');
            self.out.push_str(constraint);
            self.out.push(')');
        }
    }

    fn format_block_or_do(&mut self, body: &AST) {
        match &body.v {
            ASTValue::ExprList(_) => {
                self.out.push(' ');
                self.format_block(body);
            }
            _ => {
                self.out.push_str(" do ");
                self.out.push_str(&self.format_expr(body, 0));
            }
        }
    }

    fn format_block(&mut self, block: &AST) {
        let ASTValue::ExprList(items) = &block.v else {
            return;
        };

        self.out.push('{');
        self.out.push('\n');

        self.indent += 1;
        let mut last_line = self.format_stmt_list(items);

        let end_line = self.statement_end_line(block);
        self.emit_comments_until(Some(end_line + 1), &mut last_line);

        self.indent -= 1;

        self.write_indent();
        self.out.push('}');
    }

    fn format_param_entries(&self, params: &[FnParam]) -> Vec<String> {
        let mut out = Vec::new();

        for param in params {
            let mut buf = String::new();
            buf.push_str(&param.names.join(", "));

            if let Some(ty) = &param.ty {
                buf.push_str(": ");
                buf.push_str(&self.format_type(ty));
            }
            if let Some(default) = &param.default {
                buf.push_str(" = ");
                buf.push_str(&self.format_expr(default, 0));
            }

            out.push(buf);
        }

        out
    }

    fn format_expr_list_inline(&self, exprs: &[Box<AST>], sep: &str) -> String {
        exprs
            .iter()
            .map(|e| self.format_expr(e, 0))
            .collect::<Vec<_>>()
            .join(sep)
    }

    fn format_expr(&self, ast: &AST, parent_prec: u8) -> String {
        match &ast.v {
            ASTValue::Id(name) => name.clone(),
            ASTValue::String(v) => format!("\"{}\"", v.escape_default()),
            ASTValue::Char(v) => {
                if v.is_control() || v.is_whitespace() {
                    format!("'\\x{:02X}'", *v as u8)
                } else {
                    format!("'{}'", v.escape_default())
                }
            }
            ASTValue::Integer(v) => v.to_string(),
            ASTValue::Float(v) => v.to_string(),
            ASTValue::DotId(name) => format!(".{}", name),
            ASTValue::Type(ty) => self.format_type(ty),
            ASTValue::NamedArg { name, value } => {
                format!("{}: {}", name, self.format_expr(value, 0))
            }

            ASTValue::Not(inner) => self.wrap_if_needed(
                format!("!{}", self.format_expr(inner, self.unary_prec())),
                self.unary_prec(),
                parent_prec,
            ),
            ASTValue::Mut(inner) => self.wrap_if_needed(
                format!("mut {}", self.format_expr(inner, self.unary_prec())),
                self.unary_prec(),
                parent_prec,
            ),
            ASTValue::Cast { ty, value } => {
                let mut out = String::from("cast<");
                if let Some(ty) = ty {
                    out.push_str(&self.format_type(ty.as_ref()));
                }
                out.push('>');
                out.push(' ');
                out.push_str(&self.format_expr(value, self.unary_prec()));
                self.wrap_if_needed(out, self.unary_prec(), parent_prec)
            }
            ASTValue::Transmute { ty, value } => {
                let mut out = String::from("transmute<");
                if let Some(ty) = ty {
                    out.push_str(&self.format_type(ty.as_ref()));
                }
                out.push('>');
                out.push(' ');
                out.push_str(&self.format_expr(value, self.unary_prec()));
                self.wrap_if_needed(out, self.unary_prec(), parent_prec)
            }

            ASTValue::Ref {
                mutable,
                lifetime,
                v,
            } => {
                let mut out = String::from("&");
                if let Some(l) = lifetime {
                    out.push('\'');
                    out.push(*l);
                    out.push(' ');
                }
                if *mutable {
                    out.push_str("mut ");
                }
                out.push_str(&self.format_expr(v, self.unary_prec()));
                self.wrap_if_needed(out, self.unary_prec(), parent_prec)
            }

            ASTValue::PtrOf(inner) => self.wrap_if_needed(
                format!("^{}", self.format_expr(inner, self.unary_prec())),
                self.unary_prec(),
                parent_prec,
            ),
            ASTValue::Deref(inner) => self.wrap_if_needed(
                format!("{}^", self.format_expr(inner, self.unary_prec())),
                self.unary_prec(),
                parent_prec,
            ),

            ASTValue::Call { callee, args } => {
                let mut out = String::new();
                out.push_str(&self.format_expr(callee, self.postfix_prec()));
                out.push('(');
                out.push_str(&self.format_expr_list_inline(args, ", "));
                out.push(')');
                self.wrap_if_needed(out, self.postfix_prec(), parent_prec)
            }

            ASTValue::GenericApply { target, args } => {
                let mut out = String::new();
                out.push_str(&self.format_expr(target, self.postfix_prec()));
                out.push('<');
                out.push_str(&self.format_generic_args(args));
                out.push('>');
                self.wrap_if_needed(out, self.postfix_prec(), parent_prec)
            }

            ASTValue::Index { target, indices } => {
                let mut out = String::new();
                out.push_str(&self.format_expr(target, self.postfix_prec()));
                out.push('[');
                out.push_str(&self.format_expr_list_inline(indices, ", "));
                out.push(']');
                self.wrap_if_needed(out, self.postfix_prec(), parent_prec)
            }

            ASTValue::BinExpr {
                op,
                lhs,
                rhs,
                has_eq,
            } => {
                let prec = self.bin_prec(op, *has_eq);
                let lhs_s = self.format_expr(lhs, prec);
                let rhs_s = self.format_expr(rhs, prec + 1);

                let out = if *op == Operator::Dot {
                    format!("{}.{}", lhs_s, rhs_s)
                } else {
                    format!("{} {} {}", lhs_s, self.bin_op(op, *has_eq), rhs_s)
                };

                self.wrap_if_needed(out, prec, parent_prec)
            }

            ASTValue::InitializerList(items) => {
                self.format_initializer_list(items, None, &ast.location)
            }
            ASTValue::TypedInitializerList { ty, items } => {
                self.format_initializer_list(items, Some(ty.as_ref()), &ast.location)
            }

            ASTValue::ExprList(items) => {
                if items.is_empty() {
                    "{ }".to_string()
                } else {
                    format!("{{ {} }}", self.format_expr_list_inline(items, "; "))
                }
            }
            ASTValue::ExprListNoScope(items) => self.format_expr_list_inline(items, ", "),

            ASTValue::DeclarationMulti {
                names,
                types,
                values,
                constexpr,
            } => {
                if *constexpr
                    && types.is_empty()
                    && names.len() == 1
                    && values.as_ref().map_or(false, |v| v.len() == 1)
                {
                    let value = &values.as_ref().unwrap()[0];
                    self.format_namespace_chain(&names[0], value)
                } else {
                    Formatter::format_stmt_inline(ast)
                }
            }

            ASTValue::If { .. }
            | ASTValue::While { .. }
            | ASTValue::For { .. }
            | ASTValue::ForLoop { .. }
            | ASTValue::Match { .. }
            | ASTValue::Fn { .. }
            | ASTValue::Struct { .. }
            | ASTValue::Enum { .. }
            | ASTValue::Union { .. }
            | ASTValue::RawUnion { .. }
            | ASTValue::Newtype { .. }
            | ASTValue::Alias { .. }
            | ASTValue::Package { .. }
            | ASTValue::Use { .. }
            | ASTValue::Declaration(..)
            | ASTValue::DeclarationConstexpr(..)
            | ASTValue::Set(..)
            | ASTValue::SetMulti { .. }
            | ASTValue::Return(..)
            | ASTValue::Throw(..)
            | ASTValue::Defer(..)
            | ASTValue::Pub(..) => self.format_stmt_inline_at_indent(ast, self.indent),
        }
    }

    fn wrap_if_needed(&self, out: String, my_prec: u8, parent_prec: u8) -> String {
        if my_prec < parent_prec {
            format!("({})", out)
        } else {
            out
        }
    }

    fn format_initializer_list(
        &self,
        items: &[InitializerItem],
        ty: Option<&Type>,
        loc: &SourceLocation,
    ) -> String {
        let mut prefix = String::new();

        if let Some(ty) = ty {
            prefix.push_str(&self.format_type(ty));
            prefix.push(' ');
        }

        if items.is_empty() {
            let mut out = String::new();
            out.push_str(&prefix);
            out.push_str(".{ }");
            return out;
        }

        let named_items = items
            .iter()
            .all(|item| matches!(item, InitializerItem::Named { .. }));
        let inline_items = self.render_initializer_items(items, named_items, false);
        let inline = format!("{}.{{ {} }}", prefix, inline_items.join(", "));
        let list_start_line = loc.range.begin.1;
        let list_end_line = loc.range.end.1;
        let item_ranges = items
            .iter()
            .map(Self::initializer_item_range)
            .collect::<Vec<_>>();
        let comments_in_range = self.take_comments_in_range(list_start_line, list_end_line);

        if comments_in_range.is_empty() && !self.line_width_exceeded(self.indent, &inline) {
            return inline;
        }

        let base_indent = if self.options.use_tabs {
            "\t".repeat(self.indent)
        } else {
            " ".repeat(self.options.tab_size.max(1) * self.indent)
        };
        let item_indent = if self.options.use_tabs {
            "\t".repeat(self.indent + 1)
        } else {
            " ".repeat(self.options.tab_size.max(1) * (self.indent + 1))
        };

        let multiline_items = self.render_initializer_items(items, named_items, true);
        let item_lines = item_ranges
            .iter()
            .map(|(line, _)| *line)
            .collect::<Vec<_>>();
        let mut out = String::new();
        out.push_str(&prefix);
        out.push_str(".{\n");
        let mut comment_idx = 0;
        let mut last_line = list_start_line;
        for (idx, item) in multiline_items.iter().enumerate() {
            let item_line = item_lines.get(idx).copied().unwrap_or(list_start_line);
            while comment_idx < comments_in_range.len()
                && comments_in_range[comment_idx].location.range.begin.1 < item_line
            {
                let comment = &comments_in_range[comment_idx];
                let comment_line = comment.location.range.begin.1;
                let gap = comment_line - last_line - 1;
                if gap > 0 {
                    let mut emit = gap;
                    if self.options.collapse_empty_lines && emit > 1 {
                        emit = 1;
                    }
                    for _ in 0..emit {
                        out.push('\n');
                    }
                }
                for line_text in self.wrap_comment_lines(&comment.text, self.indent + 1) {
                    out.push_str(&item_indent);
                    out.push_str(&line_text);
                    out.push('\n');
                }
                last_line = comment_line;
                comment_idx += 1;
            }

            out.push_str(&item_indent);
            out.push_str(item);
            if idx + 1 < multiline_items.len() {
                out.push(',');
            }
            out.push('\n');
            last_line = item_line;
        }
        while comment_idx < comments_in_range.len() {
            let comment = &comments_in_range[comment_idx];
            let comment_line = comment.location.range.begin.1;
            let gap = comment_line - last_line - 1;
            if gap > 0 {
                let mut emit = gap;
                if self.options.collapse_empty_lines && emit > 1 {
                    emit = 1;
                }
                for _ in 0..emit {
                    out.push('\n');
                }
            }
            for line_text in self.wrap_comment_lines(&comment.text, self.indent + 1) {
                out.push_str(&item_indent);
                out.push_str(&line_text);
                out.push('\n');
            }
            last_line = comment_line;
            comment_idx += 1;
        }
        out.push_str(&base_indent);
        out.push('}');
        out
    }

    fn render_initializer_items(
        &self,
        items: &[InitializerItem],
        named_items: bool,
        align_names: bool,
    ) -> Vec<String> {
        let mut rendered = Vec::new();
        let max_name = if align_names && named_items {
            items
                .iter()
                .filter_map(|item| match item {
                    InitializerItem::Named { name, .. } => Some(name.len()),
                    _ => None,
                })
                .max()
                .unwrap_or(0)
        } else {
            0
        };

        if named_items {
            for item in items {
                if let InitializerItem::Named { name, value } = item {
                    if let ASTValue::Id(id) = &value.v {
                        if id == name {
                            rendered.push(format!(".{}", name));
                            continue;
                        }
                    }
                    if align_names {
                        let pad = max_name.saturating_sub(name.len());
                        let padding = " ".repeat(pad + 1);
                        rendered.push(format!(
                            ".{}{}= {}",
                            name,
                            padding,
                            self.format_expr(value, 0)
                        ));
                    } else {
                        rendered.push(format!(".{} = {}", name, self.format_expr(value, 0)));
                    }
                }
            }
        } else {
            for item in items {
                match item {
                    InitializerItem::Positional(value) => rendered.push(self.format_expr(value, 0)),
                    InitializerItem::Named { name, value } => {
                        rendered.push(format!(".{} = {}", name, self.format_expr(value, 0)));
                    }
                }
            }
        }

        rendered
    }

    fn initializer_item_range(item: &InitializerItem) -> (i32, i32) {
        match item {
            InitializerItem::Named { value, .. } | InitializerItem::Positional(value) => {
                (value.location.range.begin.1, value.location.range.end.1)
            }
        }
    }

    fn take_comments_in_range(&self, start_line: i32, end_line: i32) -> Vec<Trivia> {
        let mut taken = Vec::new();
        let mut idx = self.comment_idx.get();
        while idx < self.comments.len() {
            let comment = &self.comments[idx];
            let line = comment.location.range.begin.1;
            if line < start_line {
                break;
            }
            if line > end_line {
                break;
            }
            taken.push(comment.clone());
            idx += 1;
        }
        if !taken.is_empty() {
            self.comment_idx.set(idx);
        }
        taken
    }

    fn format_generic_args(&self, args: &[GenericArg]) -> String {
        args.iter()
            .map(|arg| match arg {
                GenericArg::Type(t) => self.format_type(t),
                GenericArg::Expr(e) => e.clone(),
                GenericArg::Name(n) => n.clone(),
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn format_type(&self, ty: &Type) -> String {
        ty.to_string()
    }

    fn bin_op(&self, op: &Operator, has_eq: bool) -> String {
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

        if has_eq {
            symbol.push('=');
        }

        symbol
    }

    fn bin_prec(&self, op: &Operator, has_eq: bool) -> u8 {
        match op {
            Operator::Dot => 12,
            Operator::Mul | Operator::Divide => 9,
            Operator::Add | Operator::Sub => 8,
            Operator::LessThan | Operator::GreaterThan => 7,
            Operator::Set if has_eq => 6,
            Operator::Not if has_eq => 6,
            Operator::BinAnd => 5,
            Operator::BinXOR => 4,
            Operator::BinOr => 3,
            Operator::And => 2,
            Operator::Or => 1,
            Operator::Set => 0,
            Operator::Not => 0,
        }
    }

    fn unary_prec(&self) -> u8 {
        11
    }

    fn postfix_prec(&self) -> u8 {
        12
    }

    fn write_indent(&mut self) {
        self.write_indent_level(self.indent);
    }

    fn write_indent_level(&mut self, level: usize) {
        if self.options.use_tabs {
            for _ in 0..level {
                self.out.push('\t');
            }
        } else {
            let width = self.options.tab_size.max(1);
            for _ in 0..level {
                for _ in 0..width {
                    self.out.push(' ');
                }
            }
        }
    }

    fn statement_end_line(&self, ast: &AST) -> i32 {
        match &ast.v {
            ASTValue::Pub(inner) => self.statement_end_line(inner),
            ASTValue::Declaration(_, value) => self.statement_end_line(value),
            ASTValue::DeclarationConstexpr(_, value) => self.statement_end_line(value),

            ASTValue::DeclarationMulti { values, .. } => values
                .as_ref()
                .and_then(|vals| vals.last())
                .map(|v| self.statement_end_line(v))
                .unwrap_or(ast.location.range.end.1),

            ASTValue::Set(_, value) => self.statement_end_line(value),
            ASTValue::SetMulti { values, .. } => values
                .last()
                .map(|v| self.statement_end_line(v))
                .unwrap_or(ast.location.range.end.1),

            ASTValue::ExprList(_) => ast.location.range.end.1,

            ASTValue::Fn { body, .. } => match body {
                FnBody::Block(block) => self.statement_end_line(block),
                FnBody::Expr(expr) => expr.location.range.end.1,
            },

            ASTValue::Struct { body, .. } | ASTValue::RawUnion { body, .. } => {
                self.statement_end_line(body)
            }
            ASTValue::Enum { .. } | ASTValue::Union { .. } => ast.location.range.end.1,

            ASTValue::Match { cases, .. } => {
                let inner_end = cases
                    .last()
                    .map(|case| self.statement_end_line(&case.body))
                    .unwrap_or(ast.location.range.end.1);
                inner_end + 1
            }

            ASTValue::If { body, else_, .. } => {
                if let Some(else_) = else_ {
                    self.statement_end_line(else_)
                } else {
                    self.statement_end_line(body)
                }
            }

            ASTValue::While { body, .. }
            | ASTValue::For { body, .. }
            | ASTValue::ForLoop { body, .. } => self.statement_end_line(body),

            _ => ast.location.range.end.1,
        }
    }

    fn emit_comments_until(&mut self, stop_line: Option<i32>, last_line: &mut i32) -> bool {
        let mut emitted = false;

        while self.comment_idx.get() < self.comments.len() {
            let idx = self.comment_idx.get();
            let comment_line = self.comments[idx].location.range.begin.1;
            if stop_line.is_some_and(|l| comment_line >= l) {
                break;
            }
            self.emit_one_comment(comment_line, last_line);
            emitted = true;
        }

        emitted
    }

    fn emit_one_comment(&mut self, comment_line: i32, last_line: &mut i32) {
        let gap = comment_line - *last_line - 1;
        if gap > 0 {
            let mut emit = gap;
            if self.options.collapse_empty_lines && emit > 1 {
                emit = 1;
            }
            self.emit_blank_lines(emit as usize);
            *last_line += gap;
        }

        let idx = self.comment_idx.get();
        let text = self.comments[idx].text.clone();
        let lines = self.wrap_comment_lines(&text, self.indent);

        for line_text in &lines {
            self.write_indent();
            self.out.push_str(line_text);
            self.out.push('\n');
        }

        *last_line = comment_line + (lines.len().saturating_sub(1) as i32);
        self.comment_idx.set(idx + 1);
    }

    fn emit_blank_lines(&mut self, count: usize) {
        for _ in 0..count {
            self.out.push('\n');
        }
    }

    fn format_stmt_inline(ast: &AST) -> String {
        Formatter::format_stmt_inline_with_options(ast, 0, &FormatOptions::default())
    }

    fn format_stmt_inline_at_indent(&self, ast: &AST, indent: usize) -> String {
        Formatter::format_stmt_inline_with_options(ast, indent, &self.options)
    }

    fn format_stmt_inline_with_options(
        ast: &AST,
        indent: usize,
        options: &FormatOptions,
    ) -> String {
        let mut formatter = Formatter::new(&[], options);
        formatter.indent = indent;
        formatter.format_stmt(ast);
        formatter.out
    }

    fn is_stmt_value(&self, ast: &AST) -> bool {
        matches!(
            ast.v,
            ASTValue::Fn { .. }
                | ASTValue::Struct { .. }
                | ASTValue::Enum { .. }
                | ASTValue::Union { .. }
                | ASTValue::RawUnion { .. }
                | ASTValue::Match { .. }
                | ASTValue::If { .. }
                | ASTValue::While { .. }
                | ASTValue::For { .. }
                | ASTValue::ForLoop { .. }
        )
    }

    fn format_namespace_chain(&self, name: &str, value: &AST) -> String {
        let mut out = String::new();
        out.push_str(name);
        out.push_str("::");

        if let ASTValue::DeclarationMulti {
            names,
            types,
            values,
            constexpr,
        } = &value.v
        {
            if *constexpr
                && types.is_empty()
                && names.len() == 1
                && values.as_ref().map_or(false, |v| v.len() == 1)
            {
                let inner = &values.as_ref().unwrap()[0];
                out.push_str(&self.format_namespace_chain(&names[0], inner));
                return out;
            }
        }

        out.push_str(&self.format_expr(value, self.postfix_prec()));
        out
    }

    fn indent_width(&self, level: usize) -> usize {
        let size = self.options.tab_size.max(1);
        level.saturating_mul(size)
    }

    fn text_width(&self, text: &str) -> usize {
        let size = self.options.tab_size.max(1);
        let mut width = 0;
        for ch in text.chars() {
            if ch == '\t' {
                width += size;
            } else {
                width += 1;
            }
        }
        width
    }

    fn line_width_exceeded(&self, indent_level: usize, text: &str) -> bool {
        let max_width = self.options.line_width;
        if max_width == 0 {
            return false;
        }
        let width = self.indent_width(indent_level) + self.text_width(text);
        width > max_width
    }

    fn wrap_comment_lines(&self, comment: &str, indent_level: usize) -> Vec<String> {
        let trimmed = comment.trim_end();
        if !trimmed.starts_with("//") {
            return vec![trimmed.to_string()];
        }

        let mut content = &trimmed[2..];
        if content.starts_with(' ') {
            content = &content[1..];
        }
        if content.is_empty() {
            return vec!["//".to_string()];
        }
        if content.starts_with(' ') || content.starts_with('\t') {
            return vec![trimmed.to_string()];
        }

        let max_width = self.options.line_width;
        let indent_width = self.indent_width(indent_level);
        let prefix = "// ";

        if max_width == 0 || max_width <= indent_width + prefix.len() {
            return vec![trimmed.to_string()];
        }

        let content_width = max_width - indent_width - prefix.len();
        let mut lines: Vec<String> = Vec::new();

        let mut current = String::new();
        let mut current_width = 0;

        for word in content.split_whitespace() {
            let word_width = self.text_width(word);
            if current.is_empty() {
                current.push_str(word);
                current_width = word_width;
                continue;
            }

            if current_width + 1 + word_width <= content_width {
                current.push(' ');
                current.push_str(word);
                current_width += 1 + word_width;
            } else {
                lines.push(current);
                current = word.to_string();
                current_width = word_width;
            }
        }

        if !current.is_empty() {
            lines.push(current);
        }

        lines
            .into_iter()
            .map(|line| format!("{}{}", prefix, line))
            .collect()
    }

    fn format_optional_decl(&mut self, decl: Option<&AST>) {
        if let Some(decl) = decl {
            self.out.push_str(&self.format_expr(decl, 0));
            self.out.push_str("; ");
        }
    }

    fn emit_clause(
        &mut self,
        multiline: bool,
        indent: usize,
        prefix: &str,
        body: impl FnOnce(&mut Self),
    ) {
        if multiline {
            self.out.push('\n');
            self.write_indent_level(indent);
        } else {
            self.out.push(' ');
        }
        self.out.push_str(prefix);
        body(self);
    }
}
