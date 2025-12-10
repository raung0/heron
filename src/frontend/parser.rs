use crate::frontend::{
    ASTValue, Keyword, Lexer, LexerError, Operator, SourceLocation, Token, TokenValue, AST,
};

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    cur: Token,
    next: Token,

    enable_multi_id_parse: bool,
}

#[derive(Debug)]
pub enum ParseError {
    LexerError(LexerError),
    InvalidUnaryOperator(Token),
    InvalidBinaryOperator(Token),
    UnexpectedToken(Token, TokenValue),
    ExpectedToken(Token, TokenValue),
    InvalidFactorToken(Token),
    ExpectedConditionExpression(Token),
    InvalidForBinding(SourceLocation),
    ExpectedBody(SourceLocation),
    ExpectedExpression(Token),
    InvalidDeclarationType(Token),
}

type ParseResult = Result<Box<AST>, ParseError>;

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Result<Self, ParseError> {
        let mut ret = Self {
            lexer,
            cur: Token::new(),
            next: Token::new(),
            enable_multi_id_parse: true,
        };
        ret.next()?;
        ret.next()?;
        Ok(ret)
    }

    pub fn next(&mut self) -> Result<(), ParseError> {
        self.cur = self.next.clone();
        let new = self.lexer.next();
        if let Err(e) = new {
            return Err(ParseError::LexerError(e));
        }
        self.next = new.unwrap();
        Ok(())
    }

    pub fn parse(&mut self) -> ParseResult {
        self.parse_expression_list(&[TokenValue::EOF], false)
    }

    fn parse_expression_list(&mut self, list_end: &[TokenValue], only_comma: bool) -> ParseResult {
        let mut lst = Vec::<Box<AST>>::new();
        let mut loc = self.cur.location.clone();

        while !list_end.contains(&self.cur.v) {
            if !lst.is_empty() {
                match self.cur.v {
                    TokenValue::Semicolon if !only_comma => self.next()?,
                    TokenValue::Comma => self.next()?,
                    _ => {
                        return Err(ParseError::ExpectedToken(
                            self.cur.clone(),
                            if only_comma {
                                TokenValue::Comma
                            } else {
                                TokenValue::Semicolon
                            },
                        ));
                    }
                }
                if list_end.contains(&self.cur.v) {
                    break;
                }
            }

            if self.cur.v == TokenValue::Semicolon {
                if only_comma {
                    return Err(ParseError::UnexpectedToken(
                        self.cur.clone(),
                        TokenValue::Comma,
                    ));
                }
                self.next()?;
                continue;
            }
            if self.cur.v == TokenValue::Comma {
                self.next()?;
                continue;
            }

            let v = self.parse_expression()?;
            loc.range.end = v.location.range.end;
            lst.push(v);
        }

        Ok(AST::from(loc, ASTValue::ExprList(lst)))
    }

    fn parse_expression(&mut self) -> ParseResult {
        self.parse_binary_expressions()
    }

    fn parse_binary_expression<N>(
        &mut self,
        operators: &'static [Operator],
        has_eq: bool,
        mut next: N,
    ) -> ParseResult
    where
        N: FnMut(&mut Self) -> ParseResult,
    {
        let mut lhs = next(self)?;

        while match &self.cur.v {
            TokenValue::Op { op, has_equals } => *has_equals == has_eq && operators.contains(op),
            _ => false,
        } {
            let (op, _) = match &self.cur.v {
                TokenValue::Op { op, has_equals } => (op.clone(), *has_equals),
                _ => unreachable!(),
            };

            self.next()?;
            let rhs = next(self)?;

            let mut loc = (*lhs).location.clone();
            loc.range.end = (*rhs).location.range.end;

            lhs = AST::from(
                loc,
                ASTValue::BinExpr {
                    op,
                    lhs,
                    rhs,
                    has_eq,
                },
            );
        }

        Ok(lhs)
    }

    fn parse_binary_expressions(&mut self) -> ParseResult {
        let parse_member =
            |s: &mut Self| s.parse_binary_expression(&[Operator::Dot], false, Self::parse_factor);

        let parse_mul = |s: &mut Self| {
            s.parse_binary_expression(&[Operator::Mul, Operator::Divide], false, parse_member)
        };

        let parse_add = |s: &mut Self| {
            s.parse_binary_expression(&[Operator::Add, Operator::Sub], false, parse_mul)
        };

        let parse_rel = |s: &mut Self| {
            // <, >, <=, >=
            s.parse_binary_expression(&[Operator::LessThan, Operator::GreaterThan], true, |s| {
                s.parse_binary_expression(
                    &[Operator::LessThan, Operator::GreaterThan],
                    false,
                    parse_add,
                )
            })
        };

        let parse_eq = |s: &mut Self| {
            // ==, !=
            s.parse_binary_expression(&[Operator::Set, Operator::Not], true, parse_rel)
        };

        let parse_bitand =
            |s: &mut Self| s.parse_binary_expression(&[Operator::BinAnd], false, parse_eq);

        let parse_bitxor =
            |s: &mut Self| s.parse_binary_expression(&[Operator::BinXOR], false, parse_bitand);

        let parse_bitor =
            |s: &mut Self| s.parse_binary_expression(&[Operator::BinOr], false, parse_bitxor);

        let parse_and =
            |s: &mut Self| s.parse_binary_expression(&[Operator::And], false, parse_bitor);

        let parse_or = |s: &mut Self| s.parse_binary_expression(&[Operator::Or], false, parse_and);

        let parse_assign =
            |s: &mut Self| s.parse_binary_expression(&[Operator::Set], false, parse_or);

        let parse_assign_eq = |s: &mut Self| {
            s.parse_binary_expression(
                &[
                    Operator::Add,
                    Operator::Sub,
                    Operator::Mul,
                    Operator::Divide,
                    Operator::BinAnd,
                    Operator::BinOr,
                    Operator::BinXOR,
                ],
                true,
                parse_assign,
            )
        };

        parse_assign_eq(self)
    }

    fn parse_factor(&mut self) -> ParseResult {
        let mut l = self.cur.location.clone();

        match &self.cur.v {
            TokenValue::Id(v) => match self.next.v {
                TokenValue::Comma
                | TokenValue::Op {
                    op: Operator::Set,
                    has_equals: _,
                }
                | TokenValue::Colon
                    if self.enable_multi_id_parse =>
                {
                    self.parse_multi_id()
                }
                _ => {
                    let node = AST::from(l, ASTValue::Id(v.clone()));
                    self.next()?;
                    Ok(node)
                }
            },
            TokenValue::String(v) => {
                let node = AST::from(l, ASTValue::String(v.clone()));
                self.next()?;
                Ok(node)
            }
            TokenValue::Char(v) => {
                let node = AST::from(l, ASTValue::Char(v.clone()));
                self.next()?;
                Ok(node)
            }
            TokenValue::Integer(v) => {
                let node = AST::from(l, ASTValue::Integer(*v));
                self.next()?;
                Ok(node)
            }
            TokenValue::Float(v) => {
                let node = AST::from(l, ASTValue::Float(*v));
                self.next()?;
                Ok(node)
            }

            TokenValue::Keyword(k) => match k {
                Keyword::If => return self.parse_if(),
                Keyword::While => return self.parse_while(),
                Keyword::For => return self.parse_for(),
                _ => todo!("Unimplemented keyword: {:?}", k),
            },

            TokenValue::Op {
                op,
                has_equals: false,
            } => match op {
                Operator::BinAnd => {
                    self.next()?;

                    let mut lifetime: Option<char> = None;
                    if let TokenValue::Lifetime(c) = self.cur.v {
                        lifetime = Some(c);
                        self.next()?;
                    }

                    let is_mut = self.cur.v == TokenValue::Keyword(crate::frontend::Keyword::Mut);
                    if is_mut {
                        self.next()?;
                    }

                    // & [ 'a ] [ mut ] <expr>
                    let inner = self.parse_expression()?;
                    l.range.end = (*inner).location.range.end;

                    Ok(AST::from(
                        l,
                        ASTValue::Ref {
                            mutable: is_mut,
                            lifetime,
                            v: inner,
                        },
                    ))
                }
                Operator::Add => {
                    self.next()?;
                    self.parse_factor()
                }
                Operator::Sub => {
                    self.next()?;
                    let ast = self.parse_factor()?;
                    l.range.end = (*ast).location.range.end;
                    Ok(AST::from(
                        l.clone(),
                        ASTValue::BinExpr {
                            op: Operator::Mul,
                            lhs: ast,
                            rhs: AST::from(l, ASTValue::Float(-1.0)),
                            has_eq: false,
                        },
                    ))
                }
                _ => Err(ParseError::InvalidUnaryOperator(self.cur.clone())),
            },

            TokenValue::Op {
                has_equals: true, ..
            } => Err(ParseError::InvalidUnaryOperator(self.cur.clone())),

            TokenValue::LParen => {
                self.next()?;
                let expr = self.parse_expression()?;
                if self.cur.v != TokenValue::RParen {
                    return Err(ParseError::UnexpectedToken(
                        self.cur.clone(),
                        TokenValue::RParen,
                    ));
                }
                let end = self.cur.location.range.end;
                self.next()?;
                let mut expr = expr;
                (*expr).location.range.end = end;
                Ok(expr)
            }

            _ => Err(ParseError::InvalidFactorToken(self.cur.clone())),
        }
    }

    fn parse_if(&mut self) -> ParseResult {
        let mut loc = self.cur.location.clone();
        self.next()?;

        if self.cur.v == TokenValue::LSquirly {
            return Err(ParseError::ExpectedConditionExpression(self.cur.clone()));
        }

        let enable_multi_id_parse_prev = self.enable_multi_id_parse;
        self.enable_multi_id_parse = false;
        let expr = self.parse_expression()?;
        let cond: Box<AST>;
        let decl: Option<Box<AST>>;
        if self.cur.v == TokenValue::Semicolon {
            self.next()?;
            cond = self.parse_expression()?;
            decl = Some(expr);
        } else {
            cond = expr;
            decl = None;
        }

        if self.cur.v == TokenValue::Semicolon {
            self.enable_multi_id_parse = enable_multi_id_parse_prev;
            return Err(ParseError::ExpectedBody(self.cur.location.clone()));
        }

        let body = self.parse_block()?;
        loc.range.end = body.location.range.end;
        self.enable_multi_id_parse = enable_multi_id_parse_prev;

        let else_ = if self.cur.v == TokenValue::Keyword(Keyword::Else) {
            self.next()?;

            if self.cur.v == TokenValue::Keyword(Keyword::If) {
                let i = self.parse_if()?;
                loc.range.end = i.location.range.end;
                Some(i)
            } else {
                if self.cur.v == TokenValue::Semicolon {
                    return Err(ParseError::ExpectedBody(self.cur.location.clone()));
                }

                let else_body = self.parse_block()?;
                loc.range.end = else_body.location.range.end;
                Some(else_body)
            }
        } else {
            None
        };

        Ok(AST::from(
            loc,
            ASTValue::If {
                cond,
                decl,
                body,
                else_,
            },
        ))
    }

    fn parse_while(&mut self) -> ParseResult {
        let mut loc = self.cur.location.clone();
        self.next()?;

        let expr = self.parse_expression()?;
        let cond: Box<AST>;
        let decl: Option<Box<AST>>;
        if self.cur.v == TokenValue::Semicolon {
            self.next()?;
            cond = self.parse_expression()?;
            decl = Some(expr);
        } else {
            cond = expr;
            decl = None;
        }
        if self.cur.v == TokenValue::Semicolon {
            return Err(ParseError::ExpectedBody(self.cur.location.clone()));
        }

        let body = self.parse_block()?;
        loc.range.end = body.location.range.end;

        Ok(AST::from(loc, ASTValue::While { cond, decl, body }))
    }

    fn parse_for(&mut self) -> ParseResult {
        let mut loc = self.cur.location.clone();
        self.next()?;

        let enable_multi_id_parse_prev = self.enable_multi_id_parse;
        self.enable_multi_id_parse = false;
        let bindings_ast = self.parse_expression_list(
            &[TokenValue::Keyword(Keyword::In), TokenValue::LSquirly],
            true,
        )?;
        self.enable_multi_id_parse = enable_multi_id_parse_prev;

        let exprs = match *bindings_ast {
            AST {
                v: ASTValue::ExprList(exprs),
                ..
            } => exprs,
            _ => unreachable!("bindings_ast is always ExprList"),
        };

        let mut bindings = Vec::with_capacity(exprs.len());
        for binding in exprs {
            match &binding.v {
                ASTValue::Id(_) => bindings.push(binding),
                ASTValue::Ref { v, .. } if matches!(v.v, ASTValue::Id(_)) => bindings.push(binding),
                _ => return Err(ParseError::InvalidForBinding(binding.location.clone())),
            }
        }

        if self.cur.v != TokenValue::Keyword(Keyword::In) {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::Keyword(Keyword::In),
            ));
        }
        self.next()?;

        if self.cur.v == TokenValue::LSquirly {
            return Err(ParseError::ExpectedExpression(self.cur.clone()));
        }

        let iter = self.parse_expression()?;
        if self.cur.v == TokenValue::Semicolon {
            return Err(ParseError::ExpectedBody(self.cur.location.clone()));
        }

        let body = self.parse_block()?;
        loc.range.end = body.location.range.end;

        Ok(AST::from(
            loc,
            ASTValue::For {
                bindings,
                iter,
                body,
            },
        ))
    }

    fn parse_block(&mut self) -> ParseResult {
        if self.cur.v != TokenValue::LSquirly {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::LSquirly,
            ));
        }
        self.next()?;

        let ast = self.parse_expression_list(&[TokenValue::RSquirly], false)?;

        if self.cur.v != TokenValue::RSquirly {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::RSquirly,
            ));
        }
        self.next()?;

        Ok(ast)
    }

    fn parse_type(&mut self) -> ParseResult {}

    fn parse_multi_id(&mut self) -> ParseResult {
        let mut list_identifiers_lhs = Vec::<Box<AST>>::new();
        while let TokenValue::Id(id) = &self.cur.v {
            list_identifiers_lhs.push(AST::from(
                self.cur.location.clone(),
                ASTValue::Id(id.to_string()),
            ));
            self.next()?;
            match self.cur.v {
                TokenValue::Colon
                | TokenValue::Op {
                    op: Operator::Set,
                    has_equals: _,
                } => continue,
                _ => {}
            };
            if self.cur.v != TokenValue::Comma {
                return Err(ParseError::UnexpectedToken(
                    self.cur.clone(),
                    TokenValue::Comma,
                ));
            }
            self.next()?;
        }

        enum Kind {
            Assign,
            Define,
            DefineConstexpr,
        }
        let kind = match self.cur.v {
            TokenValue::Colon => match self.next.v {
                TokenValue::Colon => Kind::DefineConstexpr,
                TokenValue::Op {
                    op: Operator::Set,
                    has_equals: false,
                } => Kind::Define,
                _ => return Err(ParseError::InvalidDeclarationType(self.next.clone())),
            },
            TokenValue::Op {
                op: Operator::Set,
                has_equals: false,
            } => Kind::Assign,
            _ => {
                return Err(ParseError::UnexpectedToken(
                    self.cur.clone(),
                    TokenValue::Op {
                        op: Operator::Set,
                        has_equals: false,
                    },
                ));
            }
        };

        let enable_multi_id_parse_prev = self.enable_multi_id_parse;
        self.enable_multi_id_parse = false;
        loop {
            todo!()
        }
        self.enable_multi_id_parse = enable_multi_id_parse_prev;

        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_one(mut lx: Lexer) -> ParseResult {
        let mut p = Parser::new(&mut lx).expect("parser init");
        p.parse_factor()
    }

    fn parse_expr(mut lx: Lexer) -> ParseResult {
        let mut p = Parser::new(&mut lx).expect("parser init");
        let ast = p.parse()?;
        match *ast {
            AST {
                v: ASTValue::ExprList(exprs),
                ..
            } => {
                let mut iter = exprs.into_iter();
                let first = iter
                    .next()
                    .expect("expected parser to produce at least one expression");
                assert!(
                    iter.next().is_none(),
                    "expected a single expression in the input"
                );
                Ok(first)
            }
            _ => panic!("parser should wrap expressions in ExprList"),
        }
    }

    fn expect_binary<'a>(
        ast: &'a AST,
        expected_op: Operator,
        expected_has_eq: bool,
    ) -> (&'a AST, &'a AST) {
        match &ast.v {
            ASTValue::BinExpr {
                op,
                lhs,
                rhs,
                has_eq,
            } => {
                assert_eq!(
                    op, &expected_op,
                    "expected {:?} operator but found {:?}",
                    expected_op, op
                );
                assert_eq!(
                    has_eq, &expected_has_eq,
                    "expected has_eq={} but found {}",
                    expected_has_eq, has_eq
                );
                (&*lhs, &*rhs)
            }
            _ => panic!("expected BinaryExpression"),
        }
    }

    fn expect_id(ast: &AST, expected: &str) {
        match &ast.v {
            ASTValue::Id(name) => assert_eq!(name, expected, "expected id {}", expected),
            _ => panic!("expected Id({})", expected),
        }
    }

    fn expect_integer(ast: &AST, expected: u64) {
        match &ast.v {
            ASTValue::Integer(value) => assert_eq!(
                *value, expected,
                "expected integer {}, got {}",
                expected, value
            ),
            _ => panic!("expected Integer({})", expected),
        }
    }

    fn expect_expr_list_ids(ast: &AST, expected: &[&str]) {
        match &ast.v {
            ASTValue::ExprList(exprs) => {
                assert_eq!(
                    exprs.len(),
                    expected.len(),
                    "expected {} statements, got {}",
                    expected.len(),
                    exprs.len()
                );
                for (expr, expected_name) in exprs.iter().zip(expected.iter()) {
                    expect_id(expr.as_ref(), expected_name);
                }
            }
            _ => panic!("expected ExprList"),
        }
    }

    fn expect_if<'a>(ast: &'a AST) -> (Option<&'a AST>, &'a AST, &'a AST, Option<&'a AST>) {
        match &ast.v {
            ASTValue::If {
                decl,
                cond,
                body,
                else_,
            } => (
                decl.as_deref(),
                cond.as_ref(),
                body.as_ref(),
                else_.as_deref(),
            ),
            _ => panic!("expected If expression"),
        }
    }

    fn expect_while<'a>(ast: &'a AST) -> (Option<&'a AST>, &'a AST, &'a AST) {
        match &ast.v {
            ASTValue::While { decl, cond, body } => (decl.as_deref(), cond.as_ref(), body.as_ref()),
            _ => panic!("expected While expression"),
        }
    }

    fn expect_for<'a>(ast: &'a AST) -> (&'a [Box<AST>], &'a AST, &'a AST) {
        match &ast.v {
            ASTValue::For {
                bindings,
                iter,
                body,
                ..
            } => (bindings.as_slice(), iter.as_ref(), body.as_ref()),
            _ => panic!("expected For expression"),
        }
    }

    #[test]
    fn factor_id() {
        let lx = Lexer::new("foo".to_string(), "<test>".to_string());
        let ast = parse_one(lx).expect("ok");
        match *ast {
            AST { ref v, .. } => match v {
                ASTValue::Id(s) => assert_eq!(s, "foo"),
                _ => panic!("expected Id"),
            },
        }
    }

    #[test]
    fn factor_integer() {
        let lx = Lexer::new("123".to_string(), "<test>".to_string());
        let ast = parse_one(lx).expect("ok");
        match *ast {
            AST { ref v, .. } => match v {
                ASTValue::Integer(n) => assert_eq!(*n, 123),
                _ => panic!("expected Integer"),
            },
        }
    }

    #[test]
    fn factor_float() {
        let lx = Lexer::new("3.5".to_string(), "<test>".to_string());
        let ast = parse_one(lx).expect("ok");
        match *ast {
            AST { ref v, .. } => match v {
                ASTValue::Float(f) => assert!((*f - 3.5).abs() < 1e-9),
                _ => panic!("expected Float"),
            },
        }
    }

    #[test]
    fn factor_string() {
        let lx = Lexer::new(r#""hi""#.to_string(), "<test>".to_string());
        let ast = parse_one(lx).expect("ok");
        match *ast {
            AST { ref v, .. } => match v {
                ASTValue::String(s) => assert_eq!(s, "hi"),
                _ => panic!("expected String"),
            },
        }
    }

    #[test]
    fn unary_plus_noop() {
        let lx = Lexer::new("+42".to_string(), "<test>".to_string());
        let ast = parse_one(lx).expect("ok");
        match *ast {
            AST { ref v, .. } => match v {
                ASTValue::Integer(n) => assert_eq!(*n, 42),
                _ => panic!("expected Integer after unary +"),
            },
        }
    }

    #[test]
    fn unary_minus_as_mul_by_neg_one() {
        let lx = Lexer::new("-7".to_string(), "<test>".to_string());
        let ast = parse_one(lx).expect("ok");
        match *ast {
            AST { ref v, .. } => match v {
                ASTValue::BinExpr {
                    op,
                    lhs,
                    rhs,
                    has_eq: false,
                } => {
                    assert!(matches!(op, Operator::Mul));
                    // left should be the original factor (7)
                    match **lhs {
                        AST { ref v, .. } => match v {
                            ASTValue::Integer(n) => assert_eq!(*n, 7),
                            _ => panic!("left should be Integer(7)"),
                        },
                    }
                    // right should be Float(-1.0)
                    match **rhs {
                        AST { ref v, .. } => match v {
                            ASTValue::Float(f) => assert!((*f + 1.0).abs() < 1e-9),
                            _ => panic!("right should be Float(-1.0)"),
                        },
                    }
                }
                _ => panic!("expected BinaryExpression(*, x, -1.0)"),
            },
        }
    }

    #[test]
    fn reference_plain() {
        let lx = Lexer::new("&x".to_string(), "<test>".to_string());
        let ast = parse_one(lx).expect("ok");
        match *ast {
            AST { ref v, .. } => match v {
                ASTValue::Ref {
                    mutable,
                    lifetime,
                    v,
                } => {
                    assert_eq!(*mutable, false);
                    assert_eq!(*lifetime, None);
                    match **v {
                        AST { ref v, .. } => match v {
                            ASTValue::Id(s) => assert_eq!(s, "x"),
                            _ => panic!("& should wrap Id"),
                        },
                    }
                }
                _ => panic!("expected Reference"),
            },
        }
    }

    #[test]
    fn reference_lifetime_mut() {
        let lx = Lexer::new("&'a mut foo".to_string(), "<test>".to_string());
        let ast = parse_one(lx).expect("ok");
        match *ast {
            AST { ref v, .. } => match v {
                ASTValue::Ref {
                    mutable,
                    lifetime,
                    v,
                } => {
                    assert_eq!(*mutable, true);
                    assert_eq!(*lifetime, Some('a'));
                    match **v {
                        AST { ref v, .. } => match v {
                            ASTValue::Id(s) => assert_eq!(s, "foo"),
                            _ => panic!("&'a mut should wrap Id"),
                        },
                    }
                }
                _ => panic!("expected Reference with lifetime+mut"),
            },
        }
    }

    #[test]
    fn invalid_unary_operator() {
        // '/' as a leading token should be invalid as a unary operator here
        let lx = Lexer::new("/x".to_string(), "<test>".to_string());
        let err = parse_one(lx).err().expect("should be Err");
        match err {
            ParseError::InvalidUnaryOperator(tok) => match tok.v {
                TokenValue::Op {
                    op,
                    has_equals: false,
                } => assert!(matches!(op, Operator::Divide)),
                _ => panic!("expected Divide op token"),
            },
            _ => panic!("expected InvalidUnaryOperator"),
        }
    }

    #[test]
    fn missing_closing_paren_reports_unexpected_token() {
        let lx = Lexer::new("(foo".to_string(), "<test>".to_string());
        let err = match parse_one(lx) {
            Ok(_) => panic!("parser should error when ')' is missing"),
            Err(err) => err,
        };
        match err {
            ParseError::UnexpectedToken(tok, TokenValue::RParen) => match tok.v {
                TokenValue::EOF => {}
                other => panic!("expected EOF token, got {:?}", other),
            },
            _ => panic!("expected UnexpectedToken for missing ')'"),
        }
    }

    #[test]
    fn expression_list_requires_separators() {
        let mut lx = Lexer::new("foo bar".to_string(), "<test>".to_string());
        let mut parser = Parser::new(&mut lx).expect("parser init");
        let err = match parser.parse() {
            Ok(_) => panic!("parser should reject missing separator between expressions"),
            Err(err) => err,
        };
        match err {
            ParseError::ExpectedToken(tok, TokenValue::Semicolon) => match tok.v {
                TokenValue::Id(name) => assert_eq!(name, "bar"),
                other => panic!("expected identifier token, got {:?}", other),
            },
            _ => panic!("expected ExpectedToken error for missing separator"),
        }
    }

    #[test]
    fn if_requires_braced_block() {
        let mut lx = Lexer::new("if foo bar".to_string(), "<test>".to_string());
        let mut parser = Parser::new(&mut lx).expect("parser init");
        let err = match parser.parse() {
            Ok(_) => panic!("if statement without braces should fail"),
            Err(err) => err,
        };
        match err {
            ParseError::ExpectedToken(tok, TokenValue::LSquirly) => match tok.v {
                TokenValue::Id(name) => assert_eq!(name, "bar"),
                other => panic!("expected identifier token, got {:?}", other),
            },
            _ => panic!("expected ExpectedToken for missing '{{'"),
        }
    }

    #[test]
    fn block_statements_require_separators() {
        let mut lx = Lexer::new("if foo { bar baz }".to_string(), "<test>".to_string());
        let mut parser = Parser::new(&mut lx).expect("parser init");
        let err = match parser.parse() {
            Ok(_) => panic!("block statements without separators should fail"),
            Err(err) => err,
        };
        match err {
            ParseError::ExpectedToken(tok, TokenValue::Semicolon) => match tok.v {
                TokenValue::Id(name) => assert_eq!(name, "baz"),
                other => panic!("expected identifier token, got {:?}", other),
            },
            _ => panic!("expected ExpectedToken for missing separator inside block"),
        }
    }

    #[test]
    fn binary_mul_precedence_over_add() {
        let lx = Lexer::new("1 + 2 * 3".to_string(), "<test>".to_string());
        let ast = parse_expr(lx).expect("ok");
        let (lhs, rhs) = expect_binary(ast.as_ref(), Operator::Add, false);
        expect_integer(lhs, 1);
        let (mul_lhs, mul_rhs) = expect_binary(rhs, Operator::Mul, false);
        expect_integer(mul_lhs, 2);
        expect_integer(mul_rhs, 3);
    }

    #[test]
    fn binary_sub_left_associative() {
        let lx = Lexer::new("10 - 3 - 1".to_string(), "<test>".to_string());
        let ast = parse_expr(lx).expect("ok");
        let (lhs, rhs) = expect_binary(ast.as_ref(), Operator::Sub, false);
        let (inner_lhs, inner_rhs) = expect_binary(lhs, Operator::Sub, false);
        expect_integer(inner_lhs, 10);
        expect_integer(inner_rhs, 3);
        expect_integer(rhs, 1);
    }

    #[test]
    fn relational_and_equality_precedence() {
        let lx = Lexer::new("a <= b == c".to_string(), "<test>".to_string());
        let ast = parse_expr(lx).expect("ok");
        let (lhs, rhs) = expect_binary(ast.as_ref(), Operator::Set, true);
        let (rel_lhs, rel_rhs) = expect_binary(lhs, Operator::LessThan, true);
        expect_id(rel_lhs, "a");
        expect_id(rel_rhs, "b");
        expect_id(rhs, "c");
    }

    #[test]
    fn assignment_preserves_rhs_structure() {
        let lx = Lexer::new("foo = bar + baz".to_string(), "<test>".to_string());
        let ast = parse_expr(lx).expect("ok");
        let (lhs, rhs) = expect_binary(ast.as_ref(), Operator::Set, false);
        expect_id(lhs, "foo");
        let (add_lhs, add_rhs) = expect_binary(rhs, Operator::Add, false);
        expect_id(add_lhs, "bar");
        expect_id(add_rhs, "baz");
    }

    #[test]
    fn compound_assignment_uses_rhs_precedence() {
        let lx = Lexer::new("foo += bar * baz".to_string(), "<test>".to_string());
        let ast = parse_expr(lx).expect("ok");
        let (lhs, rhs) = expect_binary(ast.as_ref(), Operator::Add, true);
        expect_id(lhs, "foo");
        let (mul_lhs, mul_rhs) = expect_binary(rhs, Operator::Mul, false);
        expect_id(mul_lhs, "bar");
        expect_id(mul_rhs, "baz");
    }

    #[test]
    fn dot_binds_tighter_than_add() {
        let lx = Lexer::new("foo.bar + baz".to_string(), "<test>".to_string());
        let ast = parse_expr(lx).expect("ok");
        let (lhs, rhs) = expect_binary(ast.as_ref(), Operator::Add, false);
        let (dot_lhs, dot_rhs) = expect_binary(lhs, Operator::Dot, false);
        expect_id(dot_lhs, "foo");
        expect_id(dot_rhs, "bar");
        expect_id(rhs, "baz");
    }

    #[test]
    fn logical_and_has_higher_precedence_than_or() {
        let lx = Lexer::new("a && b || c".to_string(), "<test>".to_string());
        let ast = parse_expr(lx).expect("ok");
        let (lhs, rhs) = expect_binary(ast.as_ref(), Operator::Or, false);
        let (and_lhs, and_rhs) = expect_binary(lhs, Operator::And, false);
        expect_id(and_lhs, "a");
        expect_id(and_rhs, "b");
        expect_id(rhs, "c");
    }

    #[test]
    fn if_without_initializer_or_else() {
        let lx = Lexer::new("if foo { bar }".to_string(), "<test>".to_string());
        let ast = parse_expr(lx).expect("ok");
        let (decl, cond, body, else_) = expect_if(ast.as_ref());

        assert!(decl.is_none(), "if without initializer should not set decl");
        expect_id(cond, "foo");
        expect_expr_list_ids(body, &["bar"]);
        assert!(else_.is_none(), "expected no else branch");
    }

    #[test]
    fn if_with_initializer_and_else_block() {
        let source = "if foo = bar; baz { foo } else { qux }";
        let lx = Lexer::new(source.to_string(), "<test>".to_string());
        let ast = parse_expr(lx).expect("ok");
        let (decl, cond, body, else_) = expect_if(ast.as_ref());

        let decl_ast = decl.expect("initializer expression should exist");
        let (lhs, rhs) = expect_binary(decl_ast, Operator::Set, false);
        expect_id(lhs, "foo");
        expect_id(rhs, "bar");

        expect_id(cond, "baz");
        expect_expr_list_ids(body, &["foo"]);

        let else_block = else_.expect("else block should exist");
        expect_expr_list_ids(else_block, &["qux"]);
    }

    #[test]
    fn if_else_if_chain_nests_else_branch() {
        let lx = Lexer::new(
            "if foo { bar } else if baz { qux }".to_string(),
            "<test>".to_string(),
        );
        let ast = parse_expr(lx).expect("ok");
        let (_, cond, body, else_) = expect_if(ast.as_ref());

        expect_id(cond, "foo");
        expect_expr_list_ids(body, &["bar"]);

        let nested_if = else_.expect("expected else-if to produce nested if");
        let (nested_decl, nested_cond, nested_body, nested_else) = expect_if(nested_if);
        assert!(
            nested_decl.is_none(),
            "else-if initializer should be absent in this test"
        );
        expect_id(nested_cond, "baz");
        expect_expr_list_ids(nested_body, &["qux"]);
        assert!(
            nested_else.is_none(),
            "final nested if should not have its own else"
        );
    }

    #[test]
    fn while_without_initializer() {
        let lx = Lexer::new("while foo { bar }".to_string(), "<test>".to_string());
        let ast = parse_expr(lx).expect("ok");
        let (decl, cond, body) = expect_while(ast.as_ref());

        assert!(
            decl.is_none(),
            "while without initializer should have no decl"
        );
        expect_id(cond, "foo");
        expect_expr_list_ids(body, &["bar"]);
    }

    #[test]
    fn while_with_initializer() {
        let source = "while foo = bar; baz { foo }";
        let lx = Lexer::new(source.to_string(), "<test>".to_string());
        let ast = parse_expr(lx).expect("ok");
        let (decl, cond, body) = expect_while(ast.as_ref());

        let decl_ast = decl.expect("initializer expression should exist");
        let (lhs, rhs) = expect_binary(decl_ast, Operator::Set, false);
        expect_id(lhs, "foo");
        expect_id(rhs, "bar");

        expect_id(cond, "baz");
        expect_expr_list_ids(body, &["foo"]);
    }

    #[test]
    fn for_single_binding_parses() {
        let source = "for foo in items { foo }";
        let lx = Lexer::new(source.to_string(), "<test>".to_string());
        let ast = parse_expr(lx).expect("ok");
        let (bindings, iter, body) = expect_for(ast.as_ref());

        assert_eq!(bindings.len(), 1, "expected single binding");
        expect_id(bindings[0].as_ref(), "foo");
        expect_id(iter, "items");
        expect_expr_list_ids(body, &["foo"]);
    }

    #[test]
    fn for_allows_reference_bindings() {
        let source = "for &a, b in sequence { }";
        let lx = Lexer::new(source.to_string(), "<test>".to_string());
        let ast = parse_expr(lx).expect("ok");
        let (bindings, iter, body) = expect_for(ast.as_ref());

        assert_eq!(bindings.len(), 2, "expected two bindings");
        match &bindings[0].as_ref().v {
            ASTValue::Ref {
                mutable,
                lifetime,
                v,
            } => {
                assert!(!*mutable, "binding should not be mutable");
                assert_eq!(*lifetime, None, "unexpected lifetime on binding");
                expect_id(v.as_ref(), "a");
            }
            _ => panic!("first binding should be a reference"),
        }
        expect_id(bindings[1].as_ref(), "b");
        expect_id(iter, "sequence");
        expect_expr_list_ids(body, &[]);
    }

    #[test]
    fn for_binding_must_be_identifier_or_reference() {
        let mut lx = Lexer::new("for 123 in iter { foo }".to_string(), "<test>".to_string());
        let mut parser = Parser::new(&mut lx).expect("parser init");
        let err = match parser.parse() {
            Ok(_) => panic!("numeric binding should not parse"),
            Err(err) => err,
        };
        match err {
            ParseError::InvalidForBinding(_) => {}
            other => panic!("expected InvalidForBinding, got {:?}", other),
        }
    }

    #[test]
    fn for_requires_in_keyword() {
        let mut lx = Lexer::new("for foo { foo }".to_string(), "<test>".to_string());
        let mut parser = Parser::new(&mut lx).expect("parser init");
        let err = match parser.parse() {
            Ok(_) => panic!("missing 'in' should not parse"),
            Err(err) => err,
        };
        match err {
            ParseError::ExpectedToken(_, TokenValue::Keyword(Keyword::In)) => {}
            other => panic!("expected ExpectedToken(..., In), got {:?}", other),
        }
    }

    #[test]
    fn for_requires_iterator_expression() {
        let mut lx = Lexer::new("for foo in { } { foo }".to_string(), "<test>".to_string());
        let mut parser = Parser::new(&mut lx).expect("parser init");
        let err = match parser.parse() {
            Ok(_) => panic!("missing iterator expression should not parse"),
            Err(err) => err,
        };
        match err {
            ParseError::ExpectedExpression(tok) => match tok.v {
                TokenValue::LSquirly => {}
                other => panic!("expected '{{' token, got {:?}", other),
            },
            other => panic!("expected ExpectedExpression, got {:?}", other),
        }
    }

    #[test]
    fn for_requires_braced_body() {
        let mut lx = Lexer::new("for foo in iter;".to_string(), "<test>".to_string());
        let mut parser = Parser::new(&mut lx).expect("parser init");
        let err = match parser.parse() {
            Ok(_) => panic!("missing for-body braces should not parse"),
            Err(err) => err,
        };
        match err {
            ParseError::ExpectedBody(_) => {}
            other => panic!("expected ExpectedBody, got {:?}", other),
        }
    }
}
