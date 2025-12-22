use crate::frontend::{
    AST, ASTValue, EnsuresClause, FnBody, FnParam, GenericArg, GenericParam, Keyword, Lexer,
    LexerError, Operator, SourceLocation, Token, TokenValue, Type,
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
        let mut need_separator = false;

        while !list_end.contains(&self.cur.v) {
            if need_separator {
                match self.cur.v {
                    TokenValue::Semicolon if !only_comma => {
                        self.next()?;
                        need_separator = false;
                        continue;
                    }
                    TokenValue::Comma => {
                        self.next()?;
                        need_separator = false;
                        continue;
                    }
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
            need_separator = true;
        }

        Ok(AST::from(loc, ASTValue::ExprList(lst)))
    }

    fn parse_expression(&mut self) -> ParseResult {
        self.parse_binary_expressions()
    }

    fn parse_expression_with_stop(&mut self, stop: &[TokenValue]) -> ParseResult {
        self.parse_binary_expressions_with_stop(stop)
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

    fn parse_binary_expression_with_stop<N>(
        &mut self,
        operators: &'static [Operator],
        has_eq: bool,
        stop: &[TokenValue],
        mut next: N,
    ) -> ParseResult
    where
        N: FnMut(&mut Self) -> ParseResult,
    {
        let mut lhs = next(self)?;

        while !stop.contains(&self.cur.v)
            && match &self.cur.v {
                TokenValue::Op { op, has_equals } => {
                    *has_equals == has_eq && operators.contains(op)
                }
                _ => false,
            }
        {
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

    fn parse_binary_expressions_with_stop(&mut self, stop: &[TokenValue]) -> ParseResult {
        let parse_member = |s: &mut Self| {
            s.parse_binary_expression_with_stop(&[Operator::Dot], false, stop, Self::parse_factor)
        };

        let parse_mul = |s: &mut Self| {
            s.parse_binary_expression_with_stop(
                &[Operator::Mul, Operator::Divide],
                false,
                stop,
                parse_member,
            )
        };

        let parse_add = |s: &mut Self| {
            s.parse_binary_expression_with_stop(
                &[Operator::Add, Operator::Sub],
                false,
                stop,
                parse_mul,
            )
        };

        let parse_rel = |s: &mut Self| {
            // <, >, <=, >=
            s.parse_binary_expression_with_stop(
                &[Operator::LessThan, Operator::GreaterThan],
                true,
                stop,
                |s| {
                    s.parse_binary_expression_with_stop(
                        &[Operator::LessThan, Operator::GreaterThan],
                        false,
                        stop,
                        parse_add,
                    )
                },
            )
        };

        let parse_eq = |s: &mut Self| {
            // ==, !=
            s.parse_binary_expression_with_stop(
                &[Operator::Set, Operator::Not],
                true,
                stop,
                parse_rel,
            )
        };

        let parse_bitand = |s: &mut Self| {
            s.parse_binary_expression_with_stop(&[Operator::BinAnd], false, stop, parse_eq)
        };

        let parse_bitxor = |s: &mut Self| {
            s.parse_binary_expression_with_stop(&[Operator::BinXOR], false, stop, parse_bitand)
        };

        let parse_bitor = |s: &mut Self| {
            s.parse_binary_expression_with_stop(&[Operator::BinOr], false, stop, parse_bitxor)
        };

        let parse_and = |s: &mut Self| {
            s.parse_binary_expression_with_stop(&[Operator::And], false, stop, parse_bitor)
        };

        let parse_or = |s: &mut Self| {
            s.parse_binary_expression_with_stop(&[Operator::Or], false, stop, parse_and)
        };

        let parse_assign = |s: &mut Self| {
            s.parse_binary_expression_with_stop(&[Operator::Set], false, stop, parse_or)
        };

        let parse_assign_eq = |s: &mut Self| {
            s.parse_binary_expression_with_stop(
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
                stop,
                parse_assign,
            )
        };

        parse_assign_eq(self)
    }

    fn parse_factor(&mut self) -> ParseResult {
        let mut l = self.cur.location.clone();

        match &self.cur.v {
            TokenValue::Id(v) => {
                let should_parse_multi_id = self.enable_multi_id_parse
                    && match &self.next.v {
                        TokenValue::Colon => true,
                        TokenValue::Comma => self.should_parse_multi_id_from_comma()?,
                        _ => false,
                    };

                if should_parse_multi_id {
                    self.parse_multi_id()
                } else {
                    let node = AST::from(l, ASTValue::Id(v.clone()));
                    self.next()?;
                    Ok(node)
                }
            }
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
                Keyword::Fn => return self.parse_fn(),
                Keyword::Struct => return self.parse_struct(),
                Keyword::Union => return self.parse_union(),
                Keyword::RawUnion => return self.parse_raw_union(),
                Keyword::Newtype => return self.parse_newtype(),
                Keyword::Alias => return self.parse_alias(),
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

    fn parse_fn(&mut self) -> ParseResult {
        let mut loc = self.cur.location.clone();
        self.next()?; // consume 'fn'

        let generics = if Self::is_lt(&self.cur.v) {
            self.parse_generic_params()?
        } else {
            Vec::new()
        };
        self.consume_semicolons()?;

        let params = if self.cur.v == TokenValue::LParen {
            self.parse_fn_params()?
        } else {
            Vec::new()
        };
        self.consume_semicolons()?;

        let return_type = if self.cur.v == TokenValue::Arrow {
            self.next()?; // consume '->'
            Some(self.parse_type_inner()?)
        } else {
            None
        };
        self.consume_semicolons()?;

        let mut throws: Vec<Box<Type>> = Vec::new();
        if self.cur.v == TokenValue::Keyword(Keyword::Throws) {
            self.next()?; // consume 'throws'
            throws.push(self.parse_type_inner()?);
            loop {
                match self.cur.v {
                    TokenValue::Comma => {
                        self.next()?;
                        throws.push(self.parse_type_inner()?);
                    }
                    TokenValue::Semicolon => {
                        if self.token_starts_type(&self.next.v) {
                            self.next()?; // consume ';'
                            throws.push(self.parse_type_inner()?);
                        } else {
                            break;
                        }
                    }
                    _ => break,
                }
            }
        }
        self.consume_semicolons()?;

        let where_clause = if self.cur.v == TokenValue::Keyword(Keyword::Where) {
            self.next()?; // consume 'where'
            let stop = &[
                TokenValue::Keyword(Keyword::Ensures),
                TokenValue::Keyword(Keyword::Do),
                TokenValue::LSquirly,
                TokenValue::Semicolon,
            ];
            Some(self.parse_expression_with_stop(stop)?)
        } else {
            None
        };
        self.consume_semicolons()?;

        let mut ensures: Vec<EnsuresClause> = Vec::new();
        while self.cur.v == TokenValue::Keyword(Keyword::Ensures) {
            self.next()?; // consume 'ensures'
            let binders = if Self::is_lt(&self.cur.v) {
                self.parse_angle_binder_list()?
            } else {
                Vec::new()
            };
            let stop = &[
                TokenValue::Keyword(Keyword::Ensures),
                TokenValue::Keyword(Keyword::Do),
                TokenValue::LSquirly,
                TokenValue::Semicolon,
            ];
            let condition = self.parse_expression_with_stop(stop)?;
            ensures.push(EnsuresClause { binders, condition });
            self.consume_semicolons()?;
        }

        self.consume_semicolons()?;
        let body = if self.cur.v == TokenValue::Keyword(Keyword::Do) {
            self.next()?; // consume 'do'
            let stop = &[
                TokenValue::Semicolon,
                TokenValue::EOF,
                TokenValue::RSquirly,
                TokenValue::RParen,
                TokenValue::RBracket,
            ];
            FnBody::Expr(self.parse_expression_with_stop(stop)?)
        } else if self.cur.v == TokenValue::LSquirly {
            FnBody::Block(self.parse_block()?)
        } else {
            return Err(ParseError::ExpectedBody(self.cur.location.clone()));
        };

        loc.range.end = match &body {
            FnBody::Block(b) => b.location.range.end,
            FnBody::Expr(e) => e.location.range.end,
        };

        Ok(AST::from(
            loc,
            ASTValue::Fn {
                generics,
                params,
                return_type,
                throws,
                where_clause,
                ensures,
                body,
            },
        ))
    }

    fn parse_struct(&mut self) -> ParseResult {
        let mut loc = self.cur.location.clone();
        self.next()?; // consume 'struct'

        let generics = if Self::is_lt(&self.cur.v) {
            self.parse_generic_params()?
        } else {
            Vec::new()
        };

        let extends = if self.cur.v == TokenValue::Keyword(Keyword::Extends) {
            self.next()?; // consume 'extends'
            Some(self.parse_type_inner()?)
        } else {
            None
        };

        if self.cur.v != TokenValue::LSquirly {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::LSquirly,
            ));
        }

        let body = self.parse_block()?;
        loc.range.end = body.location.range.end;

        Ok(AST::from(
            loc,
            ASTValue::Struct {
                generics,
                extends,
                body,
            },
        ))
    }

    fn parse_union(&mut self) -> ParseResult {
        let mut loc = self.cur.location.clone();
        self.next()?; // consume 'union'

        let generics = if Self::is_lt(&self.cur.v) {
            self.parse_generic_params()?
        } else {
            Vec::new()
        };

        if self.cur.v != TokenValue::LSquirly {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::LSquirly,
            ));
        }
        self.next()?; // consume '{'

        let mut variants: Vec<Box<Type>> = Vec::new();
        while self.cur.v != TokenValue::RSquirly {
            variants.push(self.parse_type_inner()?);

            match self.cur.v {
                TokenValue::Semicolon | TokenValue::Comma => {
                    self.next()?;
                    if self.cur.v == TokenValue::RSquirly {
                        break;
                    }
                }
                TokenValue::RSquirly => break,
                _ => {
                    return Err(ParseError::ExpectedToken(
                        self.cur.clone(),
                        TokenValue::Semicolon,
                    ));
                }
            }
        }

        if self.cur.v != TokenValue::RSquirly {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::RSquirly,
            ));
        }
        loc.range.end = self.cur.location.range.end;
        self.next()?; // consume '}'

        Ok(AST::from(loc, ASTValue::Union { generics, variants }))
    }

    fn parse_raw_union(&mut self) -> ParseResult {
        let mut loc = self.cur.location.clone();
        self.next()?; // consume 'raw_union'

        let generics = if Self::is_lt(&self.cur.v) {
            self.parse_generic_params()?
        } else {
            Vec::new()
        };

        if self.cur.v != TokenValue::LSquirly {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::LSquirly,
            ));
        }

        let body = self.parse_block()?;
        loc.range.end = body.location.range.end;

        Ok(AST::from(loc, ASTValue::RawUnion { generics, body }))
    }

    fn parse_newtype(&mut self) -> ParseResult {
        let mut loc = self.cur.location.clone();
        self.next()?; // consume 'newtype'

        let underlying = self.parse_type_inner()?;
        let constraint = if self.cur.v == TokenValue::LParen {
            Some(self.collect_paren_text()?)
        } else {
            None
        };

        loc.range.end = self.cur.location.range.end;
        Ok(AST::from(
            loc,
            ASTValue::Newtype {
                underlying,
                constraint,
            },
        ))
    }

    fn parse_alias(&mut self) -> ParseResult {
        let mut loc = self.cur.location.clone();
        self.next()?; // consume 'alias'

        let underlying = self.parse_type_inner()?;
        loc.range.end = self.cur.location.range.end;

        Ok(AST::from(loc, ASTValue::Alias { underlying }))
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

    fn should_parse_multi_id_from_comma(&self) -> Result<bool, ParseError> {
        // Disambiguate:
        // - `a, b` (expression list) vs
        // - `a, b = ...` / `a, b : ...` (multi assign/decl)
        //
        // We only treat it as multi-id when we can see:
        // `id (',' id)+ ('=' | ':')`
        let mut probe = self.lexer.clone();

        // We know `self.cur` is an id and `self.next` is a comma.
        let mut tok = probe.next().map_err(ParseError::LexerError)?; // token after the comma
        match &tok.v {
            TokenValue::Id(_) => {}
            _ => return Ok(false),
        }

        loop {
            tok = probe.next().map_err(ParseError::LexerError)?; // token after id
            match &tok.v {
                TokenValue::Comma => {
                    tok = probe.next().map_err(ParseError::LexerError)?; // token after comma
                    if !matches!(tok.v, TokenValue::Id(_)) {
                        return Ok(false);
                    }
                }
                TokenValue::Colon => return Ok(true),
                TokenValue::Op {
                    op: Operator::Set,
                    has_equals: false,
                } => return Ok(true),
                _ => return Ok(false),
            }
        }
    }

    fn parse_type_inner(&mut self) -> Result<Box<Type>, ParseError> {
        match &self.cur.v {
            TokenValue::Keyword(Keyword::Fn) => self.parse_fn_type(),
            TokenValue::Op {
                op: Operator::BinAnd,
                has_equals: false,
            } => {
                // & [ 'a ] [ mut ] <type>
                self.next()?;

                let mut lifetime: Option<char> = None;
                if let TokenValue::Lifetime(c) = self.cur.v {
                    lifetime = Some(c);
                    self.next()?;
                }

                let is_mut = self.cur.v == TokenValue::Keyword(Keyword::Mut);
                if is_mut {
                    self.next()?;
                }

                Ok(Box::new(Type::Reference {
                    mutable: is_mut,
                    lifetime,
                    underlying: self.parse_type_inner()?,
                }))
            }

            TokenValue::Op {
                op: Operator::BinXOR,
                has_equals: false,
            } => {
                // ^<type>
                self.next()?;
                Ok(Box::new(Type::Pointer {
                    underlying: self.parse_type_inner()?,
                }))
            }

            TokenValue::LBracket => {
                // []T, [N]T, [^]T, [*]T
                self.next()?;
                match &self.cur.v {
                    TokenValue::RBracket => {
                        self.next()?;
                        Ok(Box::new(Type::Slice {
                            underlying: self.parse_type_inner()?,
                        }))
                    }
                    TokenValue::Op {
                        op: Operator::BinXOR,
                        has_equals: false,
                    }
                    | TokenValue::Op {
                        op: Operator::Mul,
                        has_equals: false,
                    } => {
                        self.next()?;
                        if self.cur.v != TokenValue::RBracket {
                            return Err(ParseError::ExpectedToken(
                                self.cur.clone(),
                                TokenValue::RBracket,
                            ));
                        }
                        self.next()?;
                        Ok(Box::new(Type::CArray {
                            underlying: self.parse_type_inner()?,
                        }))
                    }
                    _ => {
                        // [<expr>]T
                        let enable_multi_id_parse_prev = self.enable_multi_id_parse;
                        self.enable_multi_id_parse = false;
                        let size_expr = self.parse_expression()?;
                        self.enable_multi_id_parse = enable_multi_id_parse_prev;
                        let size = size_expr.to_string();
                        if self.cur.v != TokenValue::RBracket {
                            return Err(ParseError::ExpectedToken(
                                self.cur.clone(),
                                TokenValue::RBracket,
                            ));
                        }
                        self.next()?;
                        Ok(Box::new(Type::Array {
                            size,
                            underlying: self.parse_type_inner()?,
                        }))
                    }
                }
            }

            TokenValue::LParen => {
                // (T)
                self.next()?;
                let t = self.parse_type_inner()?;
                if self.cur.v != TokenValue::RBracket {
                    return Err(ParseError::ExpectedToken(
                        self.cur.clone(),
                        TokenValue::RBracket,
                    ));
                }
                Ok(t)
            }

            TokenValue::Id(_) => self.parse_type_idish(),

            _ => Err(ParseError::UnexpectedToken(
                self.cur.clone(),
                TokenValue::Id("type".to_string()),
            )),
        }
    }

    fn parse_fn_type(&mut self) -> Result<Box<Type>, ParseError> {
        // fn(<type>, ...) ['->' <type>]
        if self.cur.v != TokenValue::Keyword(Keyword::Fn) {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::Keyword(Keyword::Fn),
            ));
        }
        self.next()?; // consume 'fn'

        if self.cur.v != TokenValue::LParen {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::LParen,
            ));
        }
        self.next()?; // consume '('

        let enable_multi_id_parse_prev = self.enable_multi_id_parse;
        self.enable_multi_id_parse = false;

        let mut params: Vec<Box<Type>> = Vec::new();
        if self.cur.v != TokenValue::RParen {
            loop {
                params.push(self.parse_type_inner()?);
                if self.cur.v == TokenValue::Comma {
                    self.next()?;
                    continue;
                }
                break;
            }
        }

        if self.cur.v != TokenValue::RParen {
            self.enable_multi_id_parse = enable_multi_id_parse_prev;
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::RParen,
            ));
        }
        self.next()?; // consume ')'

        let return_type = if self.cur.v == TokenValue::Arrow {
            self.next()?; // consume '->'
            Some(self.parse_type_inner()?)
        } else {
            None
        };

        self.enable_multi_id_parse = enable_multi_id_parse_prev;
        Ok(Box::new(Type::Fn {
            params,
            return_type,
        }))
    }

    fn parse_type_idish(&mut self) -> Result<Box<Type>, ParseError> {
        // <segment> ('.' <segment>)* ['<' ... '>']
        let mut full = match &self.cur.v {
            TokenValue::Id(name) => name.clone(),
            _ => {
                return Err(ParseError::ExpectedToken(
                    self.cur.clone(),
                    TokenValue::Id("type".to_string()),
                ));
            }
        };
        self.next()?;

        while matches!(
            self.cur.v,
            TokenValue::Op {
                op: Operator::Dot,
                has_equals: false
            }
        ) {
            self.next()?; // consume '.'
            match &self.cur.v {
                TokenValue::Id(seg) => {
                    full.push('.');
                    full.push_str(seg);
                    self.next()?;
                }
                _ => {
                    return Err(ParseError::ExpectedToken(
                        self.cur.clone(),
                        TokenValue::Id("type".to_string()),
                    ));
                }
            }
        }

        if matches!(
            self.cur.v,
            TokenValue::Op {
                op: Operator::LessThan,
                has_equals: false
            }
        ) {
            let args = self.parse_generic_args()?;
            return Ok(Box::new(Type::Generic { base: full, args }));
        }

        Ok(Box::new(match full.as_str() {
            "bool" => Type::Bool,
            "rune" => Type::Rune,
            s => {
                if let Some(t) = Self::type_from_numeric_name(s) {
                    t
                } else {
                    Type::Id(s.to_string())
                }
            }
        }))
    }

    fn type_from_numeric_name(name: &str) -> Option<Type> {
        fn parse_bits(s: &str) -> Option<u8> {
            match s.parse::<u16>().ok()? {
                8 | 16 | 32 | 64 => Some(s.parse::<u8>().ok()?),
                _ => None,
            }
        }

        if let Some(rest) = name.strip_prefix('u') {
            return Some(Type::Integer {
                bit_size: parse_bits(rest)?,
                signed: false,
            });
        }
        if let Some(rest) = name.strip_prefix('i') {
            return Some(Type::Integer {
                bit_size: parse_bits(rest)?,
                signed: true,
            });
        }
        if let Some(rest) = name.strip_prefix('f') {
            return Some(Type::Float {
                bit_size: parse_bits(rest)?,
            });
        }
        None
    }

    fn snapshot(&self) -> (Lexer, Token, Token, bool) {
        (
            self.lexer.clone(),
            self.cur.clone(),
            self.next.clone(),
            self.enable_multi_id_parse,
        )
    }

    fn restore(&mut self, snap: (Lexer, Token, Token, bool)) {
        let (lx, cur, next, enable_multi_id_parse) = snap;
        *self.lexer = lx;
        self.cur = cur;
        self.next = next;
        self.enable_multi_id_parse = enable_multi_id_parse;
    }

    fn parse_generic_args(&mut self) -> Result<Vec<GenericArg>, ParseError> {
        // cur is '<'
        if !matches!(
            self.cur.v,
            TokenValue::Op {
                op: Operator::LessThan,
                has_equals: false
            }
        ) {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::Op {
                    op: Operator::LessThan,
                    has_equals: false,
                },
            ));
        }
        self.next()?; // consume '<'

        let enable_multi_id_parse_prev = self.enable_multi_id_parse;
        self.enable_multi_id_parse = false;

        let mut args: Vec<GenericArg> = Vec::new();
        if matches!(
            self.cur.v,
            TokenValue::Op {
                op: Operator::GreaterThan,
                has_equals: false
            }
        ) {
            self.next()?; // consume '>'
            self.enable_multi_id_parse = enable_multi_id_parse_prev;
            return Ok(args);
        }

        loop {
            let arg = self.parse_generic_arg()?;
            args.push(arg);

            if self.cur.v == TokenValue::Comma {
                self.next()?;
                continue;
            }

            if matches!(
                self.cur.v,
                TokenValue::Op {
                    op: Operator::GreaterThan,
                    has_equals: false
                }
            ) {
                self.next()?; // consume '>'
                break;
            }

            self.enable_multi_id_parse = enable_multi_id_parse_prev;
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::Op {
                    op: Operator::GreaterThan,
                    has_equals: false,
                },
            ));
        }

        self.enable_multi_id_parse = enable_multi_id_parse_prev;
        Ok(args)
    }

    fn parse_generic_arg(&mut self) -> Result<GenericArg, ParseError> {
        // Heuristic:
        // - If it clearly starts like a type-only construct, parse as Type.
        // - Otherwise, try parsing as Type first; if it's just a "name-like" type (no generics,
        //   not a builtin), keep it as `Name` so later phases can disambiguate value-vs-type params.
        // - If type parse fails, parse as expression.
        let starts_type_only = matches!(
            self.cur.v,
            TokenValue::Op {
                op: Operator::BinAnd,
                has_equals: false
            } | TokenValue::Op {
                op: Operator::BinXOR,
                has_equals: false
            } | TokenValue::LBracket
        );

        if starts_type_only {
            return Ok(GenericArg::Type(self.parse_type_inner()?));
        }

        let snap = self.snapshot();
        if let Ok(t) = self.parse_type_inner() {
            let terminates = self.cur.v == TokenValue::Comma
                || matches!(
                    self.cur.v,
                    TokenValue::Op {
                        op: Operator::GreaterThan,
                        has_equals: false
                    }
                );
            if terminates {
                return Ok(match t.as_ref() {
                    Type::Bool
                    | Type::Rune
                    | Type::Integer { .. }
                    | Type::Float { .. }
                    | Type::Generic { .. }
                    | Type::Fn { .. }
                    | Type::Pointer { .. }
                    | Type::Slice { .. }
                    | Type::Array { .. }
                    | Type::CArray { .. }
                    | Type::Reference { .. } => GenericArg::Type(t),
                    Type::Id(name) => GenericArg::Name(name.clone()),
                });
            }
        }
        self.restore(snap);

        let gt = TokenValue::Op {
            op: Operator::GreaterThan,
            has_equals: false,
        };
        let expr = self.parse_expression_with_stop(&[TokenValue::Comma, gt.clone()])?;
        let terminates = self.cur.v == TokenValue::Comma || self.cur.v == gt;
        if !terminates {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::Comma,
            ));
        }
        Ok(GenericArg::Expr(expr.to_string()))
    }

    fn parse_type(&mut self) -> ParseResult {
        let loc = self.cur.location.clone();
        let t = self.parse_type_inner()?;
        Ok(AST::from(loc, ASTValue::Type(t)))
    }

    fn parse_multi_id(&mut self) -> ParseResult {
        let mut loc = self.cur.location.clone();

        let mut names = Vec::<String>::new();
        while let TokenValue::Id(id) = &self.cur.v {
            names.push(id.clone());
            loc.range.end = self.cur.location.range.end;
            self.next()?;
            match self.cur.v {
                TokenValue::Colon
                | TokenValue::Op {
                    op: Operator::Set,
                    has_equals: false,
                } => break,
                _ => {}
            }

            if self.cur.v != TokenValue::Comma {
                return Err(ParseError::UnexpectedToken(
                    self.cur.clone(),
                    TokenValue::Comma,
                ));
            }
            self.next()?; // consume comma
        }

        if names.is_empty() {
            return Err(ParseError::ExpectedExpression(self.cur.clone()));
        }

        enum Kind {
            Assign,
            Declare,
        }

        let kind = match &self.cur.v {
            TokenValue::Op {
                op: Operator::Set,
                has_equals: false,
            } => Kind::Assign,
            TokenValue::Colon => Kind::Declare,
            _ => {
                return Err(ParseError::UnexpectedToken(
                    self.cur.clone(),
                    TokenValue::Colon,
                ));
            }
        };

        let enable_multi_id_parse_prev = self.enable_multi_id_parse;
        self.enable_multi_id_parse = true;

        let ast = match kind {
            Kind::Assign => {
                self.next()?; // consume '='

                let rhs_list = self.parse_expression_list(
                    &[
                        TokenValue::Semicolon,
                        TokenValue::EOF,
                        TokenValue::RSquirly,
                        TokenValue::RParen,
                        TokenValue::RBracket,
                    ],
                    true,
                )?;
                loc.range.end = rhs_list.location.range.end;

                let values = match rhs_list.v {
                    ASTValue::ExprList(values) => values,
                    _ => unreachable!("expression_list always returns ExprList"),
                };

                if names.len() == 1 && values.len() == 1 {
                    AST::from(
                        loc,
                        ASTValue::Set(names[0].clone(), values.into_iter().next().unwrap()),
                    )
                } else {
                    AST::from(loc, ASTValue::SetMulti { names, values })
                }
            }
            Kind::Declare => {
                self.next()?; // consume first ':'

                let mut types = Vec::<Box<Type>>::new();
                if !matches!(
                    self.cur.v,
                    TokenValue::Colon
                        | TokenValue::Op {
                            op: Operator::Set,
                            has_equals: false
                        }
                ) {
                    loop {
                        types.push(self.parse_type_inner()?);
                        if self.cur.v == TokenValue::Comma {
                            self.next()?;
                            continue;
                        }
                        break;
                    }
                }

                let (constexpr, values) = match &self.cur.v {
                    TokenValue::Op {
                        op: Operator::Set,
                        has_equals: false,
                    } => {
                        self.next()?; // consume '='
                        let rhs_list = self.parse_expression_list(
                            &[
                                TokenValue::Semicolon,
                                TokenValue::EOF,
                                TokenValue::RSquirly,
                                TokenValue::RParen,
                                TokenValue::RBracket,
                            ],
                            true,
                        )?;
                        let values = match rhs_list.v {
                            ASTValue::ExprList(values) => values,
                            _ => unreachable!("expression_list always returns ExprList"),
                        };
                        loc.range.end = rhs_list.location.range.end;
                        (false, Some(values))
                    }
                    TokenValue::Colon => {
                        self.next()?; // consume second ':'
                        let rhs_list = self.parse_expression_list(
                            &[
                                TokenValue::Semicolon,
                                TokenValue::EOF,
                                TokenValue::RSquirly,
                                TokenValue::RParen,
                                TokenValue::RBracket,
                            ],
                            true,
                        )?;
                        let values = match rhs_list.v {
                            ASTValue::ExprList(values) => values,
                            _ => unreachable!("expression_list always returns ExprList"),
                        };
                        loc.range.end = rhs_list.location.range.end;
                        (true, Some(values))
                    }
                    _ => {
                        if types.is_empty() {
                            return Err(ParseError::InvalidDeclarationType(self.cur.clone()));
                        }
                        (false, None)
                    }
                };

                AST::from(
                    loc,
                    ASTValue::DeclarationMulti {
                        names,
                        types,
                        values,
                        constexpr,
                    },
                )
            }
        };

        self.enable_multi_id_parse = enable_multi_id_parse_prev;
        Ok(ast)
    }

    fn is_lt(v: &TokenValue) -> bool {
        matches!(
            v,
            TokenValue::Op {
                op: Operator::LessThan,
                has_equals: false
            }
        )
    }

    fn is_gt(v: &TokenValue) -> bool {
        matches!(
            v,
            TokenValue::Op {
                op: Operator::GreaterThan,
                has_equals: false
            }
        )
    }

    fn parse_angle_binder_list(&mut self) -> Result<Vec<String>, ParseError> {
        if !Self::is_lt(&self.cur.v) {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::Op {
                    op: Operator::LessThan,
                    has_equals: false,
                },
            ));
        }
        self.next()?; // consume '<'

        let mut binders: Vec<String> = Vec::new();
        if Self::is_gt(&self.cur.v) {
            self.next()?; // consume '>'
            return Ok(binders);
        }

        loop {
            match &self.cur.v {
                TokenValue::Id(name) => {
                    binders.push(name.clone());
                    self.next()?;
                }
                _ => {
                    return Err(ParseError::ExpectedToken(
                        self.cur.clone(),
                        TokenValue::Id("name".to_string()),
                    ));
                }
            }

            if self.cur.v == TokenValue::Comma {
                self.next()?;
                continue;
            }

            if Self::is_gt(&self.cur.v) {
                self.next()?; // consume '>'
                break;
            }

            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::Op {
                    op: Operator::GreaterThan,
                    has_equals: false,
                },
            ));
        }

        Ok(binders)
    }

    fn parse_generic_params(&mut self) -> Result<Vec<GenericParam>, ParseError> {
        if !Self::is_lt(&self.cur.v) {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::Op {
                    op: Operator::LessThan,
                    has_equals: false,
                },
            ));
        }
        self.next()?; // consume '<'

        let enable_multi_id_parse_prev = self.enable_multi_id_parse;
        self.enable_multi_id_parse = false;

        let mut params: Vec<GenericParam> = Vec::new();
        if Self::is_gt(&self.cur.v) {
            self.next()?; // consume '>'
            self.enable_multi_id_parse = enable_multi_id_parse_prev;
            return Ok(params);
        }

        while !Self::is_gt(&self.cur.v) {
            match &self.cur.v {
                TokenValue::Lifetime(c) => {
                    params.push(GenericParam::Lifetime(*c));
                    self.next()?;
                }
                TokenValue::Id(_) => {
                    let mut names: Vec<String> = Vec::new();
                    loop {
                        match &self.cur.v {
                            TokenValue::Id(name) => {
                                names.push(name.clone());
                                self.next()?;
                            }
                            _ => {
                                self.enable_multi_id_parse = enable_multi_id_parse_prev;
                                return Err(ParseError::ExpectedToken(
                                    self.cur.clone(),
                                    TokenValue::Id("name".to_string()),
                                ));
                            }
                        }

                        if self.cur.v == TokenValue::Comma {
                            self.next()?;
                            continue;
                        }
                        break;
                    }

                    let constraint = if self.cur.v == TokenValue::Colon {
                        self.next()?; // consume ':'
                        Some(self.parse_type_inner()?)
                    } else {
                        None
                    };

                    let is_type_param = match constraint.as_ref() {
                        None => true,
                        Some(t) => matches!(t.as_ref(), Type::Id(name) if name == "type"),
                    };

                    if is_type_param {
                        params.push(GenericParam::Type { names, constraint });
                    } else {
                        params.push(GenericParam::Value {
                            names,
                            ty: constraint.expect("value param requires type"),
                        });
                    }
                }
                _ => {
                    self.enable_multi_id_parse = enable_multi_id_parse_prev;
                    return Err(ParseError::UnexpectedToken(
                        self.cur.clone(),
                        TokenValue::Id("generic parameter".to_string()),
                    ));
                }
            }

            match &self.cur.v {
                TokenValue::Comma | TokenValue::Semicolon => {
                    self.next()?;
                    continue;
                }
                v if Self::is_gt(v) => break,
                _ => {
                    self.enable_multi_id_parse = enable_multi_id_parse_prev;
                    return Err(ParseError::ExpectedToken(
                        self.cur.clone(),
                        TokenValue::Op {
                            op: Operator::GreaterThan,
                            has_equals: false,
                        },
                    ));
                }
            }
        }

        if !Self::is_gt(&self.cur.v) {
            self.enable_multi_id_parse = enable_multi_id_parse_prev;
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::Op {
                    op: Operator::GreaterThan,
                    has_equals: false,
                },
            ));
        }
        self.next()?; // consume '>'

        self.enable_multi_id_parse = enable_multi_id_parse_prev;
        Ok(params)
    }

    fn parse_fn_params(&mut self) -> Result<Vec<FnParam>, ParseError> {
        if self.cur.v != TokenValue::LParen {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::LParen,
            ));
        }
        self.next()?; // consume '('

        let mut params: Vec<FnParam> = Vec::new();
        if self.cur.v == TokenValue::RParen {
            self.next()?; // consume ')'
            return Ok(params);
        }

        loop {
            while self.cur.v == TokenValue::Semicolon {
                self.next()?;
            }
            if self.cur.v == TokenValue::RParen {
                self.next()?; // consume ')'
                break;
            }

            let mut using_param = false;
            if self.cur.v == TokenValue::Keyword(Keyword::Using) {
                using_param = true;
                self.next()?;
            }

            let mut names: Vec<String> = Vec::new();
            match &self.cur.v {
                TokenValue::Id(name) => {
                    names.push(name.clone());
                    self.next()?;
                }
                _ => {
                    return Err(ParseError::ExpectedToken(
                        self.cur.clone(),
                        TokenValue::Id("param".to_string()),
                    ));
                }
            }

            while self.cur.v == TokenValue::Comma {
                if !self.should_parse_names_share_type()? {
                    return Err(ParseError::ExpectedToken(
                        self.cur.clone(),
                        TokenValue::Semicolon,
                    ));
                }
                self.next()?; // consume comma
                match &self.cur.v {
                    TokenValue::Id(name) => {
                        names.push(name.clone());
                        self.next()?;
                    }
                    _ => {
                        return Err(ParseError::ExpectedToken(
                            self.cur.clone(),
                            TokenValue::Id("param".to_string()),
                        ));
                    }
                }
            }

            let ty = if self.cur.v == TokenValue::Colon {
                self.next()?; // consume ':'
                Some(self.parse_type_inner()?)
            } else {
                None
            };

            let default = if matches!(
                self.cur.v,
                TokenValue::Op {
                    op: Operator::Set,
                    has_equals: false
                }
            ) {
                self.next()?; // consume '='
                let stop = &[TokenValue::Semicolon, TokenValue::RParen];
                Some(self.parse_expression_with_stop(stop)?)
            } else {
                None
            };

            params.push(FnParam {
                using: using_param,
                names,
                ty,
                default,
            });

            match &self.cur.v {
                TokenValue::Semicolon => {
                    self.next()?;
                    continue;
                }
                TokenValue::RParen => {
                    self.next()?; // consume ')'
                    break;
                }
                _ => {
                    return Err(ParseError::ExpectedToken(
                        self.cur.clone(),
                        TokenValue::Semicolon,
                    ));
                }
            }
        }

        Ok(params)
    }

    fn should_parse_names_share_type(&self) -> Result<bool, ParseError> {
        // Disambiguate within fn param lists:
        // - `a, b: T` (shared type) vs
        // - `a, b` (separate params)
        //
        // We only treat it as shared-type if we can see:
        // `id (',' id)+ ':'`
        if self.cur.v != TokenValue::Comma {
            return Ok(false);
        }

        // We know current is a comma. `self.next` is the token after the comma.
        if !matches!(self.next.v, TokenValue::Id(_)) {
            return Ok(false);
        }

        // The underlying lexer is positioned after `self.next`, so start probing from there.
        let mut probe = self.lexer.clone();
        let mut tok = probe.next().map_err(ParseError::LexerError)?; // token after the id

        loop {
            match &tok.v {
                TokenValue::Comma => {
                    tok = probe.next().map_err(ParseError::LexerError)?; // token after comma
                    if !matches!(tok.v, TokenValue::Id(_)) {
                        return Ok(false);
                    }
                    tok = probe.next().map_err(ParseError::LexerError)?; // token after id
                }
                TokenValue::Colon => return Ok(true),
                _ => return Ok(false),
            }
        }
    }

    fn token_to_sourceish(v: &TokenValue) -> String {
        match v {
            TokenValue::Id(s) => s.clone(),
            TokenValue::String(s) => format!("\"{}\"", s.escape_default()),
            TokenValue::Char(c) => format!("'{}'", c.escape_default()),
            TokenValue::Keyword(k) => format!("{:?}", k),
            TokenValue::Integer(n) => n.to_string(),
            TokenValue::Float(fl) => fl.to_string(),
            TokenValue::Lifetime(c) => format!("'{}", c),
            TokenValue::Colon => ":".to_string(),
            TokenValue::Semicolon => ";".to_string(),
            TokenValue::Comma => ",".to_string(),
            TokenValue::Arrow => "->".to_string(),
            TokenValue::Ellipsis => "..".to_string(),
            TokenValue::ListInit => ".{".to_string(),
            TokenValue::LParen => "(".to_string(),
            TokenValue::RParen => ")".to_string(),
            TokenValue::LSquirly => "{".to_string(),
            TokenValue::RSquirly => "}".to_string(),
            TokenValue::LBracket => "[".to_string(),
            TokenValue::RBracket => "]".to_string(),
            TokenValue::EOF => "<eof>".to_string(),
            TokenValue::Op { op, has_equals } => {
                let mut s = match op {
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
                if *has_equals {
                    s.push('=');
                }
                s
            }
        }
    }

    fn collect_paren_text(&mut self) -> Result<String, ParseError> {
        if self.cur.v != TokenValue::LParen {
            return Err(ParseError::ExpectedToken(
                self.cur.clone(),
                TokenValue::LParen,
            ));
        }
        self.next()?; // consume '('

        let mut parts: Vec<String> = Vec::new();
        while self.cur.v != TokenValue::RParen {
            if self.cur.v == TokenValue::EOF {
                return Err(ParseError::ExpectedToken(
                    self.cur.clone(),
                    TokenValue::RParen,
                ));
            }
            parts.push(Self::token_to_sourceish(&self.cur.v));
            self.next()?;
        }
        self.next()?; // consume ')'

        Ok(parts.join(" "))
    }

    fn consume_semicolons(&mut self) -> Result<(), ParseError> {
        while self.cur.v == TokenValue::Semicolon {
            self.next()?;
        }
        Ok(())
    }

    fn token_starts_type(&self, v: &TokenValue) -> bool {
        matches!(v, TokenValue::Id(_))
            || matches!(v, TokenValue::Keyword(Keyword::Fn))
            || matches!(
                v,
                TokenValue::Op {
                    op: Operator::BinAnd | Operator::BinXOR,
                    has_equals: false
                }
            )
            || matches!(v, TokenValue::LBracket)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::read_to_string;

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

    fn parse_all(mut lx: Lexer) -> Result<Vec<Box<AST>>, ParseError> {
        let mut p = Parser::new(&mut lx).expect("parser init");
        let ast = p.parse()?;
        match *ast {
            AST {
                v: ASTValue::ExprList(exprs),
                ..
            } => Ok(exprs),
            _ => unreachable!("parser should wrap expressions in ExprList"),
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

    fn expect_set_multi<'a>(ast: &'a AST) -> (&'a [String], &'a [Box<AST>]) {
        match &ast.v {
            ASTValue::SetMulti { names, values } => (names.as_slice(), values.as_slice()),
            _ => panic!("expected SetMulti"),
        }
    }

    fn expect_declaration_multi<'a>(
        ast: &'a AST,
    ) -> (
        bool,
        &'a [String],
        &'a [Box<crate::frontend::Type>],
        Option<&'a [Box<AST>]>,
    ) {
        match &ast.v {
            ASTValue::DeclarationMulti {
                constexpr,
                names,
                types,
                values,
            } => (
                *constexpr,
                names.as_slice(),
                types.as_slice(),
                values.as_ref().map(|v| v.as_slice()),
            ),
            _ => panic!("expected DeclarationMulti"),
        }
    }

    fn expect_type_ref(t: &crate::frontend::Type) -> (&crate::frontend::Type, bool, Option<char>) {
        match t {
            crate::frontend::Type::Reference {
                underlying,
                mutable,
                lifetime,
            } => (underlying.as_ref(), *mutable, *lifetime),
            _ => panic!("expected Reference type"),
        }
    }

    fn expect_type_array<'a>(t: &'a crate::frontend::Type) -> (&'a str, &'a crate::frontend::Type) {
        match t {
            crate::frontend::Type::Array { size, underlying } => {
                (size.as_str(), underlying.as_ref())
            }
            _ => panic!("expected Array type"),
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
    fn expr_list_ids_with_comma() {
        let mut lx = Lexer::new("foo, bar".to_string(), "<test>".to_string());
        let mut p = Parser::new(&mut lx).expect("parser init");
        let ast = p.parse().expect("ok");
        match &ast.v {
            ASTValue::ExprList(exprs) => {
                assert_eq!(exprs.len(), 2);
                expect_id(exprs[0].as_ref(), "foo");
                expect_id(exprs[1].as_ref(), "bar");
            }
            _ => panic!("expected ExprList"),
        }
    }

    #[test]
    fn multi_assign_swap() {
        let ast =
            parse_expr(Lexer::new("a, b = b, a".to_string(), "<test>".to_string())).expect("ok");

        let (names, values) = expect_set_multi(ast.as_ref());
        assert_eq!(names, ["a".to_string(), "b".to_string()]);
        assert_eq!(values.len(), 2);
        expect_id(values[0].as_ref(), "b");
        expect_id(values[1].as_ref(), "a");
    }

    #[test]
    fn decl_default_init_requires_types() {
        let ast =
            parse_expr(Lexer::new("a, b: T, U".to_string(), "<test>".to_string())).expect("ok");

        let (constexpr, names, types, values) = expect_declaration_multi(ast.as_ref());
        assert!(!constexpr);
        assert_eq!(names, ["a".to_string(), "b".to_string()]);
        assert_eq!(types.len(), 2);
        assert_eq!(
            *types[0].as_ref(),
            crate::frontend::Type::Id("T".to_string())
        );
        assert_eq!(
            *types[1].as_ref(),
            crate::frontend::Type::Id("U".to_string())
        );
        assert!(values.is_none());
    }

    #[test]
    fn decl_infer_from_initializer() {
        let ast =
            parse_expr(Lexer::new("a, b := 1, 2".to_string(), "<test>".to_string())).expect("ok");

        let (constexpr, names, types, values) = expect_declaration_multi(ast.as_ref());
        assert!(!constexpr);
        assert_eq!(names, ["a".to_string(), "b".to_string()]);
        assert!(types.is_empty());
        let values = values.expect("expected initializer list");
        assert_eq!(values.len(), 2);
        expect_integer(values[0].as_ref(), 1);
        expect_integer(values[1].as_ref(), 2);
    }

    #[test]
    fn decl_constexpr_without_type() {
        let ast = parse_expr(Lexer::new("x :: 1".to_string(), "<test>".to_string())).expect("ok");

        let (constexpr, names, types, values) = expect_declaration_multi(ast.as_ref());
        assert!(constexpr);
        assert_eq!(names, ["x".to_string()]);
        assert!(types.is_empty());
        let values = values.expect("expected constexpr value list");
        assert_eq!(values.len(), 1);
        expect_integer(values[0].as_ref(), 1);
    }

    #[test]
    fn decl_type_reference_array_and_pointer() {
        let ast = parse_expr(Lexer::new(
            "x: &'a mut [M]rune, ^T".to_string(),
            "<test>".to_string(),
        ))
        .expect("ok");

        let (_constexpr, names, types, values) = expect_declaration_multi(ast.as_ref());
        assert_eq!(names, ["x".to_string()]);
        assert!(values.is_none());
        assert_eq!(types.len(), 2);

        let (inner, is_mut, lt) = expect_type_ref(types[0].as_ref());
        assert!(is_mut);
        assert_eq!(lt, Some('a'));
        let (size, inner) = expect_type_array(inner);
        assert_eq!(size, "M");
        assert_eq!(*inner, crate::frontend::Type::Rune);

        match types[1].as_ref() {
            crate::frontend::Type::Pointer { underlying } => {
                assert_eq!(
                    *underlying.as_ref(),
                    crate::frontend::Type::Id("T".to_string())
                );
            }
            _ => panic!("expected Pointer type"),
        }
    }

    #[test]
    fn decl_type_slice_and_carray() {
        let ast = parse_expr(Lexer::new(
            "x: []u32, [*]u8".to_string(),
            "<test>".to_string(),
        ))
        .expect("ok");

        let (_constexpr, _names, types, _values) = expect_declaration_multi(ast.as_ref());
        assert_eq!(types.len(), 2);
        match types[0].as_ref() {
            crate::frontend::Type::Slice { underlying } => {
                assert_eq!(
                    *underlying.as_ref(),
                    crate::frontend::Type::Integer {
                        bit_size: 32,
                        signed: false
                    }
                );
            }
            _ => panic!("expected Slice type"),
        }
        match types[1].as_ref() {
            crate::frontend::Type::CArray { underlying } => {
                assert_eq!(
                    *underlying.as_ref(),
                    crate::frontend::Type::Integer {
                        bit_size: 8,
                        signed: false
                    }
                );
            }
            _ => panic!("expected CArray type"),
        }
    }

    #[test]
    fn decl_type_qualified_with_generics_is_accepted() {
        let ast = parse_expr(Lexer::new(
            "x: core.math.Vector<&'a T, M>".to_string(),
            "<test>".to_string(),
        ))
        .expect("ok");

        let (_constexpr, _names, types, _values) = expect_declaration_multi(ast.as_ref());
        assert_eq!(types.len(), 1);
        match types[0].as_ref() {
            crate::frontend::Type::Generic { base, args } => {
                assert_eq!(base, "core.math.Vector");
                assert_eq!(args.len(), 2);
                match &args[0] {
                    crate::frontend::GenericArg::Type(t) => match t.as_ref() {
                        crate::frontend::Type::Reference {
                            mutable,
                            lifetime,
                            underlying,
                        } => {
                            assert!(!*mutable);
                            assert_eq!(*lifetime, Some('a'));
                            assert_eq!(
                                *underlying.as_ref(),
                                crate::frontend::Type::Id("T".to_string())
                            );
                        }
                        _ => panic!("expected reference type arg"),
                    },
                    _ => panic!("expected type arg"),
                }
                assert_eq!(args[1], crate::frontend::GenericArg::Name("M".to_string()));
            }
            _ => panic!("expected Generic type"),
        }
    }

    #[test]
    fn decl_type_generics_can_take_expr_args() {
        let ast = parse_expr(Lexer::new(
            "x: Array<T, N + 1>".to_string(),
            "<test>".to_string(),
        ))
        .expect("ok");

        let (_constexpr, _names, types, _values) = expect_declaration_multi(ast.as_ref());
        assert_eq!(types.len(), 1);
        match types[0].as_ref() {
            crate::frontend::Type::Generic { base, args } => {
                assert_eq!(base, "Array");
                assert_eq!(args.len(), 2);
                assert_eq!(args[0], crate::frontend::GenericArg::Name("T".to_string()));
                assert_eq!(
                    args[1],
                    crate::frontend::GenericArg::Expr(
                        "(BinExp [op: Add, has_eq: false] N 1)".to_string()
                    )
                );
            }
            _ => panic!("expected Generic type"),
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

    #[test]
    fn fn_do_expr_parses_params_and_return() {
        let lx = Lexer::new(
            "fn (a, b: int) -> int do a + b".to_string(),
            "<test>".to_string(),
        );
        let ast = parse_one(lx).expect("ok");

        match &ast.v {
            ASTValue::Fn {
                generics,
                params,
                return_type,
                throws,
                where_clause,
                ensures,
                body,
            } => {
                assert!(generics.is_empty());
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].names, vec!["a".to_string(), "b".to_string()]);
                assert_eq!(
                    params[0].ty.as_ref().map(|t| t.as_ref()),
                    Some(&crate::frontend::Type::Id("int".to_string()))
                );
                assert!(throws.is_empty());
                assert!(where_clause.is_none());
                assert!(ensures.is_empty());
                assert_eq!(
                    return_type.as_ref().map(|t| t.as_ref()),
                    Some(&crate::frontend::Type::Id("int".to_string()))
                );
                match body {
                    FnBody::Expr(expr) => {
                        let (lhs, rhs) = expect_binary(expr.as_ref(), Operator::Add, false);
                        expect_id(lhs, "a");
                        expect_id(rhs, "b");
                    }
                    _ => panic!("expected do-body expression"),
                }
            }
            _ => panic!("expected Fn"),
        }
    }

    #[test]
    fn fn_generic_params_parse_lifetimes_types_and_values() {
        let lx = Lexer::new(
            "fn<'a; T: type; N: usize>() do N".to_string(),
            "<test>".to_string(),
        );
        let ast = parse_one(lx).expect("ok");

        match &ast.v {
            ASTValue::Fn { generics, .. } => {
                assert_eq!(generics.len(), 3);
                assert_eq!(generics[0], GenericParam::Lifetime('a'));
                assert_eq!(
                    generics[1],
                    GenericParam::Type {
                        names: vec!["T".to_string()],
                        constraint: Some(Box::new(crate::frontend::Type::Id("type".to_string()))),
                    }
                );
                assert_eq!(
                    generics[2],
                    GenericParam::Value {
                        names: vec!["N".to_string()],
                        ty: Box::new(crate::frontend::Type::Id("usize".to_string())),
                    }
                );
            }
            _ => panic!("expected Fn"),
        }
    }

    #[test]
    fn struct_with_generics_and_extends_parses() {
        let lx = Lexer::new(
            "struct<T> extends Base { x: int }".to_string(),
            "<test>".to_string(),
        );
        let ast = parse_one(lx).expect("ok");

        match &ast.v {
            ASTValue::Struct {
                generics,
                extends,
                body,
            } => {
                assert_eq!(
                    generics,
                    &vec![GenericParam::Type {
                        names: vec!["T".to_string()],
                        constraint: None
                    }]
                );
                assert_eq!(
                    extends.as_ref().map(|t| t.as_ref()),
                    Some(&crate::frontend::Type::Id("Base".to_string()))
                );
                match &body.v {
                    ASTValue::ExprList(items) => assert_eq!(items.len(), 1),
                    _ => panic!("expected struct body expr list"),
                }
            }
            _ => panic!("expected Struct"),
        }
    }

    #[test]
    fn union_is_list_of_types() {
        let lx = Lexer::new("union<T> { int; T }".to_string(), "<test>".to_string());
        let ast = parse_one(lx).expect("ok");

        match &ast.v {
            ASTValue::Union {
                generics, variants, ..
            } => {
                assert_eq!(
                    generics,
                    &vec![GenericParam::Type {
                        names: vec!["T".to_string()],
                        constraint: None
                    }]
                );
                assert_eq!(variants.len(), 2);
                assert_eq!(
                    *variants[0].as_ref(),
                    crate::frontend::Type::Id("int".to_string())
                );
                assert_eq!(
                    *variants[1].as_ref(),
                    crate::frontend::Type::Id("T".to_string())
                );
            }
            _ => panic!("expected Union"),
        }
    }

    #[test]
    fn raw_union_body_parses_like_struct_body() {
        let lx = Lexer::new(
            "raw_union { x: int; y: u32 }".to_string(),
            "<test>".to_string(),
        );
        let ast = parse_one(lx).expect("ok");

        match &ast.v {
            ASTValue::RawUnion { body, .. } => match &body.v {
                ASTValue::ExprList(items) => assert_eq!(items.len(), 2),
                _ => panic!("expected raw_union body expr list"),
            },
            _ => panic!("expected RawUnion"),
        }
    }

    #[test]
    fn newtype_with_constraint_parses() {
        let lx = Lexer::new("newtype uint(0..<32)".to_string(), "<test>".to_string());
        let ast = parse_one(lx).expect("ok");

        match &ast.v {
            ASTValue::Newtype {
                underlying,
                constraint,
            } => {
                assert_eq!(
                    *underlying.as_ref(),
                    crate::frontend::Type::Id("uint".to_string())
                );
                assert_eq!(constraint.as_deref(), Some("0 .. < 32"));
            }
            _ => panic!("expected Newtype"),
        }
    }

    #[test]
    fn alias_parses_type() {
        let lx = Lexer::new("alias [Idx]Entity".to_string(), "<test>".to_string());
        let ast = parse_one(lx).expect("ok");

        match &ast.v {
            ASTValue::Alias { underlying } => match underlying.as_ref() {
                crate::frontend::Type::Array { size, underlying } => {
                    assert_eq!(size, "Idx");
                    assert_eq!(
                        *underlying.as_ref(),
                        crate::frontend::Type::Id("Entity".to_string())
                    );
                }
                _ => panic!("expected [Idx]Entity type"),
            },
            _ => panic!("expected Alias"),
        }
    }

    #[test]
    fn type_fn_parses_param_types_and_return() {
        let ast = parse_expr(Lexer::new(
            "f: fn(int, u32) -> int".to_string(),
            "<test>".to_string(),
        ))
        .expect("ok");

        let (_constexpr, names, types, values) = expect_declaration_multi(ast.as_ref());
        assert_eq!(names, ["f".to_string()]);
        assert!(values.is_none());
        assert_eq!(types.len(), 1);

        match types[0].as_ref() {
            crate::frontend::Type::Fn {
                params,
                return_type,
            } => {
                assert_eq!(params.len(), 2);
                assert_eq!(
                    *params[0].as_ref(),
                    crate::frontend::Type::Id("int".to_string())
                );
                assert_eq!(
                    *params[1].as_ref(),
                    crate::frontend::Type::Integer {
                        bit_size: 32,
                        signed: false
                    }
                );
                assert_eq!(
                    return_type.as_ref().map(|t| t.as_ref()),
                    Some(&crate::frontend::Type::Id("int".to_string()))
                );
            }
            _ => panic!("expected fn type"),
        }
    }

    #[test]
    fn type_fn_cannot_include_throws() {
        let err = parse_expr(Lexer::new(
            "f: fn() -> int throws core.Exception".to_string(),
            "<test>".to_string(),
        ))
        .err()
        .expect("expected error");
        match err {
            ParseError::ExpectedToken(_, _)
            | ParseError::UnexpectedToken(_, _)
            | ParseError::InvalidDeclarationType(_) => {}
            _ => panic!("expected a parse error about invalid type syntax"),
        }
    }

    #[test]
    fn fn_can_throw_multiple_with_comma_and_semicolon() {
        let lx = Lexer::new("fn() throws A, B; C do 1".to_string(), "<test>".to_string());
        let ast = parse_one(lx).expect("ok");

        match &ast.v {
            ASTValue::Fn { throws, body, .. } => {
                assert_eq!(throws.len(), 3);
                assert_eq!(
                    *throws[0].as_ref(),
                    crate::frontend::Type::Id("A".to_string())
                );
                assert_eq!(
                    *throws[1].as_ref(),
                    crate::frontend::Type::Id("B".to_string())
                );
                assert_eq!(
                    *throws[2].as_ref(),
                    crate::frontend::Type::Id("C".to_string())
                );
                match body {
                    FnBody::Expr(expr) => expect_integer(expr.as_ref(), 1),
                    _ => panic!("expected do-body expression"),
                }
            }
            _ => panic!("expected Fn"),
        }
    }

    #[test]
    fn fn_header_can_have_semicolon_before_do_or_block() {
        let lx = Lexer::new("fn() -> int; do 1".to_string(), "<test>".to_string());
        let ast = parse_one(lx).expect("ok");
        match &ast.v {
            ASTValue::Fn { body, .. } => match body {
                FnBody::Expr(expr) => expect_integer(expr.as_ref(), 1),
                _ => panic!("expected do-body expression"),
            },
            _ => panic!("expected Fn"),
        }

        let lx = Lexer::new("fn(); { 1 }".to_string(), "<test>".to_string());
        let ast = parse_one(lx).expect("ok");
        match &ast.v {
            ASTValue::Fn { body, .. } => match body {
                FnBody::Block(block) => match &block.v {
                    ASTValue::ExprList(items) => {
                        assert_eq!(items.len(), 1);
                        expect_integer(items[0].as_ref(), 1);
                    }
                    _ => panic!("expected block ExprList"),
                },
                _ => panic!("expected block body"),
            },
            _ => panic!("expected Fn"),
        }
    }

    #[test]
    fn top_level_structs_can_be_separated_by_semicolon() {
        let exprs = parse_all(Lexer::new(
            "Point :: struct { x: int }; Child :: struct { }".to_string(),
            "<test>".to_string(),
        ))
        .expect("ok");
        assert_eq!(exprs.len(), 2);
    }

    #[test]
    fn testing_he_parses() {
        let input = read_to_string("testing.he").expect("read testing.he");
        let mut lx = Lexer::new(input, "testing.he".to_string());
        let mut p = Parser::new(&mut lx).expect("parser init");
        p.parse().expect("testing.he should parse");
    }

    #[test]
    fn point_and_child_snippet_parses() {
        let src = r#"
// --- structs / unions ---
Point :: struct<T; N: usize> {
	x: T
	y: [N]T
};
;
Child :: struct extends Base {
};
"#;
        let exprs = parse_all(Lexer::new(src.to_string(), "<test>".to_string())).expect("ok");
        assert!(exprs.len() >= 2);
    }

    #[test]
    fn manual_two_decls_parse_progresses_over_semicolons() {
        let src = r#"
Point :: struct<T; N: usize> {
	x: T
	y: [N]T
};
;
Child :: struct extends Base {
};
"#;
        let mut lx = Lexer::new(src.to_string(), "<test>".to_string());
        let mut p = Parser::new(&mut lx).expect("parser init");

        while p.cur.v == TokenValue::Semicolon {
            p.next().expect("advance");
        }

        let _first = p.parse_expression().expect("first decl parses");
        assert!(matches!(p.cur.v, TokenValue::Semicolon));
        p.next().expect("consume sep");
        while p.cur.v == TokenValue::Semicolon {
            p.next().expect("advance");
        }
        assert!(matches!(p.cur.v, TokenValue::Id(ref s) if s == "Child"));
        let _second = p.parse_expression().expect("second decl parses");
    }

    #[test]
    fn point_decl_leaves_separator_token() {
        let src = r#"Point :: struct<T; N: usize> {
	x: T
	y: [N]T
};
Child :: struct extends Base { };
"#;
        let mut lx = Lexer::new(src.to_string(), "<test>".to_string());
        let mut p = Parser::new(&mut lx).expect("parser init");
        let _first = p.parse_expression().expect("parse first expr");
        assert!(
            matches!(p.cur.v, TokenValue::Semicolon),
            "expected semicolon after first decl, got {:?}",
            p.cur.v
        );
    }
}
