#![allow(clippy::result_large_err)]

use crate::frontend::{
	AST, ASTValue, EnsuresClause, EnumVariant, FnBody, FnParam, GenericArg, GenericParam,
	Keyword, Lexer, MatchBinder, MatchCase, MatchCasePattern, Operator, ParseError, PostClause,
	SourceLocation, Token, TokenValue, Type,
};

pub struct Parser<'a> {
	lexer: &'a mut Lexer,
	cur: Token,
	next: Token,

	enable_multi_id_parse: bool,
	errors: Vec<ParseError>,
}

type ParseResult = Result<Box<AST>, ParseError>;

impl<'a> Parser<'a> {
	pub fn new(lexer: &'a mut Lexer) -> Result<Self, ParseError> {
		let mut ret = Self {
			lexer,
			cur: Token::new(),
			next: Token::new(),
			enable_multi_id_parse: true,
			errors: Vec::new(),
		};
		ret.next()?;
		ret.next()?;
		Ok(ret)
	}

	#[allow(clippy::should_implement_trait)]
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
		let mut ast = self.parse_expression_list(&[TokenValue::EOF], false)?;
		ast.trivia = self.lexer.take_trivia();
		if self.errors.is_empty() {
			Ok(ast)
		} else {
			Err(self.errors[0].clone())
		}
	}

	fn parse_expression_list(
		&mut self,
		list_end: &[TokenValue],
		only_comma: bool,
	) -> ParseResult {
		let mut lst = Vec::<Box<AST>>::new();
		let mut loc = self.cur.location.clone();
		let mut need_separator = false;

		while !list_end.contains(&self.cur.v) {
			if need_separator {
				match self.cur.v {
					TokenValue::Semicolon if !only_comma => {
						if let Err(e) = self.next() {
							self.record_error(e.clone());
							return Err(e);
						}
						need_separator = false;
						continue;
					}
					TokenValue::Comma => {
						if let Err(e) = self.next() {
							self.record_error(e.clone());
							return Err(e);
						}
						need_separator = false;
						continue;
					}
					_ => {
						self.record_error(ParseError::ExpectedToken(
							self.cur.clone(),
							if only_comma {
								TokenValue::Comma
							} else {
								TokenValue::Semicolon
							},
						));
						need_separator = false;
						continue;
					}
				}
			}

			if self.cur.v == TokenValue::Semicolon {
				if only_comma {
					self.record_error(ParseError::UnexpectedToken(
						self.cur.clone(),
						TokenValue::Comma,
					));
					if let Err(e) = self.next() {
						self.record_error(e.clone());
						return Err(e);
					}
					continue;
				}
				if let Err(e) = self.next() {
					self.record_error(e.clone());
					return Err(e);
				}
				continue;
			}
			if self.cur.v == TokenValue::Comma {
				if let Err(e) = self.next() {
					self.record_error(e.clone());
					return Err(e);
				}
				continue;
			}

			let v = match self.parse_expression() {
				Ok(v) => v,
				Err(e) => {
					if matches!(e, ParseError::LexerError(_)) {
						self.record_error(e.clone());
						return Err(e);
					}
					if !matches!(
						(&e, self.errors.last()),
						(
							ParseError::InvalidArraySize(loc),
							Some(ParseError::InvalidArraySize(prev))
						) if loc == prev
					) {
						self.record_error(e);
					}
					let recover = match self.errors.last() {
						Some(ParseError::InvalidArraySize(_)) => self
							.recover_after_invalid_array_size(
								only_comma,
							),
						_ => self.recover_to(list_end, only_comma),
					};
					if let Err(e) = recover {
						self.record_error(e.clone());
						return Err(e);
					}
					need_separator = false;
					continue;
				}
			};
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
			TokenValue::Op { op, has_equals } => {
				*has_equals == has_eq && operators.contains(op)
			}
			_ => false,
		} {
			let (op, _) = match &self.cur.v {
				TokenValue::Op { op, has_equals } => (op.clone(), *has_equals),
				_ => unreachable!(),
			};

			self.next()?;
			let rhs = next(self)?;

			let mut loc = lhs.location.clone();
			loc.range.end = rhs.location.range.end;

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
			} {
			let (op, _) = match &self.cur.v {
				TokenValue::Op { op, has_equals } => (op.clone(), *has_equals),
				_ => unreachable!(),
			};

			self.next()?;
			let rhs = next(self)?;

			let mut loc = lhs.location.clone();
			loc.range.end = rhs.location.range.end;

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
		let parse_mul = |s: &mut Self| {
			s.parse_binary_expression(
				&[Operator::Mul, Operator::Divide],
				false,
				Self::parse_factor,
			)
		};

		let parse_add = |s: &mut Self| {
			s.parse_binary_expression(&[Operator::Add, Operator::Sub], false, parse_mul)
		};

		let parse_rel = |s: &mut Self| {
			// <, >, <=, >=
			s.parse_binary_expression(
				&[Operator::LessThan, Operator::GreaterThan],
				true,
				|s| {
					s.parse_binary_expression(
						&[Operator::LessThan, Operator::GreaterThan],
						false,
						parse_add,
					)
				},
			)
		};

		let parse_eq = |s: &mut Self| {
			// ==, !=
			s.parse_binary_expression(&[Operator::Set, Operator::Not], true, parse_rel)
		};

		let parse_bitand = |s: &mut Self| {
			s.parse_binary_expression(&[Operator::BinAnd], false, parse_eq)
		};

		let parse_bitxor = |s: &mut Self| {
			s.parse_binary_expression(&[Operator::BinXOR], false, parse_bitand)
		};

		let parse_bitor = |s: &mut Self| {
			s.parse_binary_expression(&[Operator::BinOr], false, parse_bitxor)
		};

		let parse_and = |s: &mut Self| {
			s.parse_binary_expression(&[Operator::And], false, parse_bitor)
		};

		let parse_or =
			|s: &mut Self| s.parse_binary_expression(&[Operator::Or], false, parse_and);

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
		let parse_mul = |s: &mut Self| {
			s.parse_binary_expression_with_stop(
				&[Operator::Mul, Operator::Divide],
				false,
				stop,
				Self::parse_factor,
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
			s.parse_binary_expression_with_stop(
				&[Operator::BinAnd],
				false,
				stop,
				parse_eq,
			)
		};

		let parse_bitxor = |s: &mut Self| {
			s.parse_binary_expression_with_stop(
				&[Operator::BinXOR],
				false,
				stop,
				parse_bitand,
			)
		};

		let parse_bitor = |s: &mut Self| {
			s.parse_binary_expression_with_stop(
				&[Operator::BinOr],
				false,
				stop,
				parse_bitxor,
			)
		};

		let parse_and = |s: &mut Self| {
			s.parse_binary_expression_with_stop(
				&[Operator::And],
				false,
				stop,
				parse_bitor,
			)
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

	fn parse_primary(&mut self) -> ParseResult {
		let l = self.cur.location.clone();

		if self.cur.v == TokenValue::Keyword(Keyword::Operator)
			&& self.enable_multi_id_parse
		{
			return self.parse_multi_id();
		}

		if self.token_starts_type(&self.cur.v) {
			let snap = self.snapshot();
			let snap_starts_bracket = matches!(snap.1.v, TokenValue::LBracket);
			let type_start = self.cur.location.clone();
			match self.parse_type_inner() {
				Ok(ty) => {
					if self.cur.v == TokenValue::ListInit {
						let list_ast = self.parse_initializer_list()?;
						let items = match list_ast.v {
							ASTValue::InitializerList(items) => items,
							_ => {
								unreachable!(
									"initializer list parse must produce InitializerList"
								)
							}
						};
						let mut loc = type_start;
						loc.range.end = list_ast.location.range.end;
						return Ok(AST::from(
							loc,
							ASTValue::TypedInitializerList {
								ty,
								items,
							},
						));
					}
				}
				Err(err) => {
					if snap_starts_bracket {
						return Err(err);
					}
				}
			}
			self.restore(snap);
		}

		let node = match &self.cur.v {
			TokenValue::Id(v) => {
				let should_parse_multi_id = self.enable_multi_id_parse
					&& match &self.next.v {
						TokenValue::Colon => true,
						TokenValue::Comma => {
							self.should_parse_multi_id_from_comma()?
						}
						_ => false,
					};

				if should_parse_multi_id {
					self.parse_multi_id()?
				} else {
					let node = AST::from(l, ASTValue::Id(v.clone()));
					self.next()?;
					node
				}
			}
			TokenValue::String(v) => {
				let node = AST::from(l, ASTValue::String(v.clone()));
				self.next()?;
				node
			}
			TokenValue::Char(v) => {
				let node = AST::from(l, ASTValue::Char(*v));
				self.next()?;
				node
			}
			TokenValue::Integer(v) => {
				let node = AST::from(l, ASTValue::Integer(*v));
				self.next()?;
				node
			}
			TokenValue::Float(v) => {
				let node = AST::from(l, ASTValue::Float(*v));
				self.next()?;
				node
			}

			TokenValue::Keyword(k) => match k {
				Keyword::Package => return self.parse_package(),
				Keyword::Use => return self.parse_use(),
				Keyword::If => return self.parse_if(),
				Keyword::While => return self.parse_while(),
				Keyword::For => return self.parse_for(),
				Keyword::Return => return self.parse_return(),
				Keyword::Defer => return self.parse_defer(),
				Keyword::Match => return self.parse_match(),
				Keyword::Fn => return self.parse_fn(),
				Keyword::Struct => return self.parse_struct(),
				Keyword::Enum => return self.parse_enum(),
				Keyword::Union => return self.parse_union(),
				Keyword::RawUnion => return self.parse_raw_union(),
				Keyword::Newtype => return self.parse_newtype(),
				Keyword::Alias => return self.parse_alias(),
				Keyword::Pub => return self.parse_pub(),
				Keyword::Hide => return self.parse_hide(),
				Keyword::Operator => {
					return Err(ParseError::UnexpectedToken(
						self.cur.clone(),
						TokenValue::Id("expression".to_string()),
					));
				}
				_ => todo!("Unimplemented keyword: {:?}", k),
			},

			TokenValue::Op {
				op: Operator::Dot,
				has_equals: false,
			} => {
				self.next()?; // consume '.'
				let TokenValue::Id(name) = &self.cur.v else {
					return Err(ParseError::ExpectedToken(
						self.cur.clone(),
						TokenValue::Id("<identifier>".to_string()),
					));
				};
				let node = AST::from(l, ASTValue::DotId(name.clone()));
				self.next()?; // consume id
				node
			}
			TokenValue::Op { .. } => {
				return Err(ParseError::InvalidFactorToken(self.cur.clone()));
			}

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
				expr.location.range.end = end;
				expr
			}
			TokenValue::ListInit => return self.parse_initializer_list(),
			TokenValue::LSquirly => return self.parse_block(),

			_ => return Err(ParseError::InvalidFactorToken(self.cur.clone())),
		};

		Ok(node)
	}

	fn parse_initializer_list(&mut self) -> ParseResult {
		#[derive(Copy, Clone, Eq, PartialEq)]
		enum InitStyle {
			Positional,
			Named,
		}

		let mut loc = self.cur.location.clone();
		if self.cur.v != TokenValue::ListInit {
			return Err(ParseError::ExpectedToken(
				self.cur.clone(),
				TokenValue::ListInit,
			));
		}
		self.next()?; // consume '.{'

		let enable_multi_id_parse_prev = self.enable_multi_id_parse;
		self.enable_multi_id_parse = false;

		let mut items: Vec<crate::frontend::InitializerItem> = Vec::new();
		let mut style: Option<InitStyle> = None;
		while self.cur.v != TokenValue::RSquirly {
			if matches!(self.cur.v, TokenValue::Semicolon | TokenValue::Comma) {
				self.next()?;
				continue;
			}
			if self.cur.v == TokenValue::EOF {
				self.enable_multi_id_parse = enable_multi_id_parse_prev;
				return Err(ParseError::ExpectedToken(
					self.cur.clone(),
					TokenValue::RSquirly,
				));
			}

			if self.cur.v == TokenValue::Semicolon {
				self.next()?;
				continue;
			}

			let is_named_item = matches!(
				(&self.cur.v, &self.next.v),
				(
					TokenValue::Op {
						op: Operator::Dot,
						has_equals: false
					},
					TokenValue::Id(_)
				)
			);

			let item_style = if is_named_item {
				InitStyle::Named
			} else {
				InitStyle::Positional
			};

			match style {
				Some(InitStyle::Named)
					if matches!(
						(&self.cur.v, &self.next.v),
						(
							TokenValue::Id(_),
							TokenValue::Op {
								op: Operator::Set,
								has_equals: false
							}
						) | (TokenValue::Id(_), TokenValue::Comma) | (
							TokenValue::Id(_),
							TokenValue::Semicolon
						) | (TokenValue::Id(_), TokenValue::RSquirly)
					) =>
				{
					let err_tok = self.cur.clone();
					self.enable_multi_id_parse = enable_multi_id_parse_prev;
					return Err(ParseError::MissingInitializerDot(err_tok));
				}
				None => style = Some(item_style),
				Some(existing) if existing != item_style => {
					let err_tok = self.cur.clone();
					self.enable_multi_id_parse = enable_multi_id_parse_prev;
					return Err(ParseError::MixedInitializerListStyles(
						err_tok,
					));
				}
				_ => {}
			}

			if item_style == InitStyle::Named {
				self.next()?; // consume '.'
				let TokenValue::Id(name) = &self.cur.v else {
					unreachable!("named style requires identifier");
				};
				match &self.next.v {
					TokenValue::Op {
						op: Operator::Set,
						has_equals: false,
					} => {
						let name = name.clone();
						self.next()?; // consume name
						self.next()?; // consume '='
						let stop = &[
							TokenValue::Comma,
							TokenValue::Semicolon,
							TokenValue::RSquirly,
						];
						let value =
							self.parse_expression_with_stop(stop)?;
						items.push(
							crate::frontend::InitializerItem::Named {
								name,
								value,
							},
						);
					}
					TokenValue::Comma
					| TokenValue::Semicolon
					| TokenValue::RSquirly => {
						let name = name.clone();
						let value = AST::from(
							self.cur.location.clone(),
							ASTValue::Id(name.clone()),
						);
						self.next()?; // consume name
						items.push(
							crate::frontend::InitializerItem::Named {
								name,
								value,
							},
						);
					}
					_ => unreachable!(
						"named style detection should have matched"
					),
				}
			} else {
				let stop = &[
					TokenValue::Comma,
					TokenValue::Semicolon,
					TokenValue::RSquirly,
				];
				let value = self.parse_expression_with_stop(stop)?;
				items.push(crate::frontend::InitializerItem::Positional(value));
			}

			match self.cur.v {
				TokenValue::Comma | TokenValue::Semicolon => {
					self.next()?;
					continue;
				}
				TokenValue::RSquirly => break,
				_ => {
					self.enable_multi_id_parse = enable_multi_id_parse_prev;
					return Err(ParseError::ExpectedToken(
						self.cur.clone(),
						TokenValue::Comma,
					));
				}
			}
		}

		if self.cur.v != TokenValue::RSquirly {
			self.enable_multi_id_parse = enable_multi_id_parse_prev;
			return Err(ParseError::ExpectedToken(
				self.cur.clone(),
				TokenValue::RSquirly,
			));
		}
		loc.range.end = self.cur.location.range.end;
		self.next()?; // consume '}'
		self.enable_multi_id_parse = enable_multi_id_parse_prev;

		Ok(AST::from(loc, ASTValue::InitializerList(items)))
	}

	fn parse_pub(&mut self) -> ParseResult {
		let mut loc = self.cur.location.clone();
		self.next()?; // consume 'pub'
		self.consume_semicolons()?;

		let inner = self.parse_primary()?;
		loc.range.end = inner.location.range.end;
		Ok(AST::from(loc, ASTValue::Pub(inner)))
	}

	fn ast_is_idish(node: &AST) -> bool {
		match &node.v {
			ASTValue::Id(_) => true,
			ASTValue::BinExpr {
				op: Operator::Dot,
				lhs,
				rhs,
				has_eq: false,
			} => Self::ast_is_idish(lhs.as_ref()) && matches!(rhs.v, ASTValue::Id(_)),
			_ => false,
		}
	}

	fn ast_last_ident(node: &AST) -> Option<&str> {
		match &node.v {
			ASTValue::Id(name) => Some(name.as_str()),
			ASTValue::BinExpr {
				op: Operator::Dot,
				rhs,
				has_eq: false,
				..
			} => match &rhs.v {
				ASTValue::Id(name) => Some(name.as_str()),
				_ => None,
			},
			_ => None,
		}
	}

	fn should_commit_generic_apply(target: &AST, args: &[GenericArg]) -> bool {
		let args_have_unambiguous_syntax =
			args.iter().any(|a| !matches!(a, GenericArg::Name(_)));
		if args_have_unambiguous_syntax {
			return true;
		}

		if args.len() > 1 {
			return true;
		}

		let target_has_dot = matches!(
			target.v,
			ASTValue::BinExpr {
				op: Operator::Dot,
				has_eq: false,
				..
			}
		);
		if target_has_dot {
			return true;
		}

		let Some(last) = Self::ast_last_ident(target) else {
			return false;
		};
		last.chars()
			.next()
			.map(|c| c.is_uppercase())
			.unwrap_or(false)
	}

	fn parse_postfix(&mut self) -> ParseResult {
		let mut node = self.parse_primary()?;

		loop {
			match self.cur.v {
				TokenValue::Op {
					op: Operator::Dot,
					has_equals: false,
				} => {
					self.next()?; // consume '.'
					let rhs_loc = self.cur.location.clone();
					let member =
						match &self.cur.v {
							TokenValue::Id(name) => name.clone(),
							_ => {
								return Err(ParseError::ExpectedToken(
                                self.cur.clone(),
                                TokenValue::Id("<identifier>".to_string()),
                            ));
							}
						};
					self.next()?; // consume id

					let rhs = AST::from(rhs_loc, ASTValue::Id(member));
					let mut loc = node.location.clone();
					loc.range.end = rhs.location.range.end;
					node = AST::from(
						loc,
						ASTValue::BinExpr {
							op: Operator::Dot,
							lhs: node,
							rhs,
							has_eq: false,
						},
					);
				}
				TokenValue::Op {
					op: Operator::LessThan,
					has_equals: false,
				} => {
					if !Self::ast_is_idish(node.as_ref()) {
						break;
					}

					let snap = self.snapshot();
					let generic_parse = self.parse_generic_args();
					let Ok((args, gt_end)) = generic_parse else {
						self.restore(snap);
						break;
					};

					if self.cur.v != TokenValue::LParen {
						self.restore(snap);
						break;
					}

					if !Self::should_commit_generic_apply(node.as_ref(), &args)
					{
						self.restore(snap);
						break;
					}

					let mut generic_loc = node.location.clone();
					generic_loc.range.end = gt_end;
					node = AST::from(
						generic_loc,
						ASTValue::GenericApply { target: node, args },
					);
				}
				TokenValue::LParen => {
					self.next()?; // consume '('
					let args = self.parse_call_args()?;
					let end = self.cur.location.range.end;
					self.next()?; // consume ')'

					let mut call_loc = node.location.clone();
					call_loc.range.end = end;
					node = AST::from(
						call_loc,
						ASTValue::Call { callee: node, args },
					);
				}
				TokenValue::LBracket => {
					self.next()?; // consume '['
					let indices_list = self.parse_expression_list(
						&[TokenValue::RBracket],
						true,
					)?;
					let indices = match indices_list.v {
						ASTValue::ExprList(v) => v,
						_ => unreachable!(
							"expression_list always returns ExprList"
						),
					};
					if self.cur.v != TokenValue::RBracket {
						return Err(ParseError::ExpectedToken(
							self.cur.clone(),
							TokenValue::RBracket,
						));
					}
					let end = self.cur.location.range.end;
					self.next()?; // consume ']'

					let mut index_loc = node.location.clone();
					index_loc.range.end = end;
					node = AST::from(
						index_loc,
						ASTValue::Index {
							target: node,
							indices,
						},
					);
				}
				TokenValue::Op {
					op: Operator::BinXOR,
					has_equals: false,
				} => {
					// Postfix dereference: (expr^)
					if Self::token_starts_factor(&self.next.v) {
						break;
					}
					let end = self.cur.location.range.end;
					self.next()?; // consume '^'
					let mut deref_loc = node.location.clone();
					deref_loc.range.end = end;
					node = AST::from(deref_loc, ASTValue::Deref(node));
				}
				TokenValue::Op {
					op: Operator::Add,
					has_equals: false,
				} if self.is_adjacent_duplicate_op(Operator::Add) => {
					return Err(ParseError::UnsupportedIncrement(
						self.cur.clone(),
					));
				}
				TokenValue::Op {
					op: Operator::Sub,
					has_equals: false,
				} if self.is_adjacent_duplicate_op(Operator::Sub) => {
					return Err(ParseError::UnsupportedDecrement(
						self.cur.clone(),
					));
				}
				_ => break,
			}
		}

		Ok(node)
	}

	fn parse_unary(&mut self) -> ParseResult {
		let mut l = self.cur.location.clone();

		match &self.cur.v {
			TokenValue::Keyword(Keyword::Mut) => {
				self.next()?; // consume `mut`
				let inner = self.parse_unary()?;
				l.range.end = inner.location.range.end;
				Ok(AST::from(l, ASTValue::Mut(inner)))
			}

			TokenValue::Op {
				op,
				has_equals: false,
			} => match op {
				Operator::Dot => self.parse_postfix(),
				Operator::Not => {
					self.next()?; // consume '!'
					let inner = self.parse_unary()?;
					l.range.end = inner.location.range.end;
					Ok(AST::from(l, ASTValue::Not(inner)))
				}
				Operator::BinAnd => {
					self.next()?; // consume '&'

					let is_mut = self.cur.v
						== TokenValue::Keyword(
							crate::frontend::Keyword::Mut,
						);
					if is_mut {
						self.next()?;
					}

					let inner = self.parse_unary()?;
					l.range.end = inner.location.range.end;
					Ok(AST::from(
						l,
						ASTValue::Ref {
							mutable: is_mut,
							v: inner,
						},
					))
				}
				Operator::BinXOR => {
					self.next()?; // consume '^'
					let inner = self.parse_unary()?;
					l.range.end = inner.location.range.end;
					Ok(AST::from(l, ASTValue::PtrOf(inner)))
				}
				Operator::Add => {
					self.next()?; // consume '+'
					let inner = self.parse_unary()?;
					l.range.end = inner.location.range.end;
					Ok(AST::from(l, ASTValue::UnaryPlus(inner)))
				}
				Operator::Sub => {
					self.next()?; // consume '-'
					let inner = self.parse_unary()?;
					l.range.end = inner.location.range.end;
					Ok(AST::from(l, ASTValue::UnaryMinus(inner)))
				}
				_ => Err(ParseError::InvalidUnaryOperator(self.cur.clone())),
			},

			TokenValue::Op {
				has_equals: true, ..
			} => Err(ParseError::InvalidUnaryOperator(self.cur.clone())),

			_ => self.parse_postfix(),
		}
	}

	#[allow(clippy::vec_box)]
	fn parse_call_args(&mut self) -> Result<Vec<Box<AST>>, ParseError> {
		let enable_multi_id_parse_prev = self.enable_multi_id_parse;
		self.enable_multi_id_parse = false;

		let mut args: Vec<Box<AST>> = Vec::new();
		if self.cur.v == TokenValue::RParen {
			self.enable_multi_id_parse = enable_multi_id_parse_prev;
			return Ok(args);
		}

		loop {
			if self.cur.v == TokenValue::EOF {
				self.enable_multi_id_parse = enable_multi_id_parse_prev;
				return Err(ParseError::ExpectedToken(
					self.cur.clone(),
					TokenValue::RParen,
				));
			}

			let arg = match (&self.cur.v, &self.next.v) {
				(TokenValue::Id(name), TokenValue::Colon) => {
					let mut loc = self.cur.location.clone();
					let name = name.clone();
					self.next()?; // consume name
					self.next()?; // consume ':'
					let value = self.parse_expression_with_stop(&[
						TokenValue::Comma,
						TokenValue::RParen,
					])?;
					loc.range.end = value.location.range.end;
					AST::from(loc, ASTValue::NamedArg { name, value })
				}
				_ => self.parse_expression_with_stop(&[
					TokenValue::Comma,
					TokenValue::RParen,
				])?,
			};
			args.push(arg);

			match self.cur.v {
				TokenValue::Comma => {
					self.next()?;
					continue;
				}
				TokenValue::RParen => break,
				_ => {
					self.enable_multi_id_parse = enable_multi_id_parse_prev;
					return Err(ParseError::ExpectedToken(
						self.cur.clone(),
						TokenValue::Comma,
					));
				}
			}
		}

		self.enable_multi_id_parse = enable_multi_id_parse_prev;
		Ok(args)
	}

	fn parse_factor(&mut self) -> ParseResult {
		match self.cur.v {
			TokenValue::Keyword(Keyword::Cast) => self.parse_cast_like(false),
			TokenValue::Keyword(Keyword::Transmute) => self.parse_cast_like(true),
			_ => self.parse_unary(),
		}
	}

	fn parse_cast_like(&mut self, is_transmute: bool) -> ParseResult {
		let mut loc = self.cur.location.clone();
		self.next()?; // consume keyword

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

		let ty = if matches!(
			self.cur.v,
			TokenValue::Op {
				op: Operator::GreaterThan,
				has_equals: false
			}
		) {
			None
		} else {
			Some(self.parse_type_inner()?)
		};

		if !matches!(
			self.cur.v,
			TokenValue::Op {
				op: Operator::GreaterThan,
				has_equals: false
			}
		) {
			return Err(ParseError::ExpectedToken(
				self.cur.clone(),
				TokenValue::Op {
					op: Operator::GreaterThan,
					has_equals: false,
				},
			));
		}
		self.next()?; // consume '>'

		let value = self.parse_unary()?;
		loc.range.end = value.location.range.end;

		if is_transmute {
			Ok(AST::from(loc, ASTValue::Transmute { ty, value }))
		} else {
			Ok(AST::from(loc, ASTValue::Cast { ty, value }))
		}
	}

	fn parse_return(&mut self) -> ParseResult {
		let mut loc = self.cur.location.clone();
		self.next()?; // consume 'return'

		let has_value = !matches!(
			self.cur.v,
			TokenValue::Semicolon
				| TokenValue::Comma | TokenValue::RSquirly
				| TokenValue::EOF
		);
		let v = if has_value {
			let expr = self.parse_expression()?;
			loc.range.end = expr.location.range.end;
			Some(expr)
		} else {
			loc.range.end = self.cur.location.range.end;
			None
		};

		Ok(AST::from(loc, ASTValue::Return(v)))
	}

	fn parse_defer(&mut self) -> ParseResult {
		let mut loc = self.cur.location.clone();
		self.next()?; // consume 'defer'

		let v = self.parse_expression()?;
		loc.range.end = v.location.range.end;
		Ok(AST::from(loc, ASTValue::Defer(v)))
	}

	fn parse_hide(&mut self) -> ParseResult {
		let mut loc = self.cur.location.clone();
		self.next()?; // consume 'hide'

		let TokenValue::Id(name) = &self.cur.v else {
			return Err(ParseError::ExpectedToken(
				self.cur.clone(),
				TokenValue::Id("identifier".to_string()),
			));
		};
		let name = name.clone();
		loc.range.end = self.cur.location.range.end;
		self.next()?;
		Ok(AST::from(loc, ASTValue::Hide(name)))
	}

	fn parse_match(&mut self) -> ParseResult {
		let mut loc = self.cur.location.clone();
		self.next()?; // consume 'match'

		let snapshot = self.snapshot();
		let binder = match self.try_parse_match_binder() {
			Ok(Some(b)) if self.cur.v == TokenValue::Keyword(Keyword::In) => {
				self.next()?; // consume 'in'
				Some(b)
			}
			Ok(_) => {
				self.restore(snapshot);
				None
			}
			Err(_) => {
				self.restore(snapshot);
				None
			}
		};

		let scrutinee = self.parse_expression()?;

		if self.cur.v != TokenValue::LSquirly {
			return Err(ParseError::ExpectedToken(
				self.cur.clone(),
				TokenValue::LSquirly,
			));
		}
		self.next()?; // consume '{'

		let mut cases: Vec<MatchCase> = Vec::new();
		self.consume_semicolons()?;

		while self.cur.v != TokenValue::RSquirly {
			if self.cur.v != TokenValue::Keyword(Keyword::Case) {
				return Err(ParseError::ExpectedToken(
					self.cur.clone(),
					TokenValue::Keyword(Keyword::Case),
				));
			}
			cases.push(self.parse_match_case(binder.is_some())?);
			self.consume_semicolons()?;
		}

		let end = self.cur.location.range.end;
		self.next()?; // consume '}'
		loc.range.end = end;

		Ok(AST::from(
			loc,
			ASTValue::Match {
				binder,
				scrutinee,
				cases,
			},
		))
	}

	fn parse_match_case(&mut self, allow_type_pattern: bool) -> Result<MatchCase, ParseError> {
		self.next()?; // consume 'case'

		let mut pattern = MatchCasePattern::Default;
		let mut guard: Option<Box<AST>> = None;

		if self.cur.v != TokenValue::Arrow {
			if allow_type_pattern && self.token_starts_type(&self.cur.v) {
				let ty = self.parse_type_inner()?;
				pattern = MatchCasePattern::Type(ty);
				if self.cur.v == TokenValue::Keyword(Keyword::If) {
					self.next()?; // consume 'if'
					guard = Some(self.parse_expression_with_stop(&[
						TokenValue::Arrow,
					])?);
				}
			} else {
				let expr = self.parse_expression_with_stop(&[
					TokenValue::Keyword(Keyword::If),
					TokenValue::Arrow,
				])?;
				pattern = MatchCasePattern::Exprs(vec![expr]);
				if self.cur.v == TokenValue::Keyword(Keyword::If) {
					self.next()?; // consume 'if'
					guard = Some(self.parse_expression_with_stop(&[
						TokenValue::Arrow,
					])?);
				}
			}
		}

		if self.cur.v != TokenValue::Arrow {
			return Err(ParseError::ExpectedToken(
				self.cur.clone(),
				TokenValue::Arrow,
			));
		}
		self.next()?; // consume '->'

		let body = self.parse_expression_with_stop(&[
			TokenValue::Semicolon,
			TokenValue::RSquirly,
		])?;

		Ok(MatchCase {
			pattern,
			guard,
			body,
		})
	}

	fn try_parse_match_binder(&mut self) -> Result<Option<MatchBinder>, ParseError> {
		match &self.cur.v {
			TokenValue::Id(name) => {
				let name = name.clone();
				self.next()?; // consume id
				Ok(Some(MatchBinder {
					by_ref: false,
					mutable: false,
					lifetime: None,
					name,
				}))
			}
			TokenValue::Op {
				op: Operator::BinAnd,
				has_equals: false,
			} => {
				self.next()?; // consume '&'
				let mut lifetime: Option<char> = None;
				if let TokenValue::Lifetime(c) = self.cur.v {
					lifetime = Some(c);
					self.next()?;
				}
				let mutable = self.cur.v == TokenValue::Keyword(Keyword::Mut);
				if mutable {
					self.next()?; // consume 'mut'
				}
				let TokenValue::Id(name) = &self.cur.v else {
					return Ok(None);
				};
				let name = name.clone();
				self.next()?; // consume id
				Ok(Some(MatchBinder {
					by_ref: true,
					mutable,
					lifetime,
					name,
				}))
			}
			_ => Ok(None),
		}
	}

	fn token_starts_factor(v: &TokenValue) -> bool {
		matches!(
			v,
			TokenValue::Id(_)
				| TokenValue::String(_) | TokenValue::Char(_)
				| TokenValue::Integer(_) | TokenValue::Float(_)
				| TokenValue::LParen | TokenValue::ListInit
				| TokenValue::Keyword(_)
		) || matches!(
			v,
			TokenValue::Op {
				op: Operator::BinAnd
					| Operator::BinXOR | Operator::Add | Operator::Sub,
				has_equals: false
			}
		)
	}

	fn parse_dotted_path(&mut self) -> Result<(Vec<String>, SourceLocation), ParseError> {
		let mut loc = self.cur.location.clone();
		let mut path: Vec<String> = Vec::new();

		let first = match &self.cur.v {
			TokenValue::Id(name) => name.clone(),
			_ => {
				return Err(ParseError::ExpectedToken(
					self.cur.clone(),
					TokenValue::Id("<identifier>".to_string()),
				));
			}
		};
		path.push(first);
		loc.range.end = self.cur.location.range.end;
		self.next()?; // consume id

		while matches!(
			self.cur.v,
			TokenValue::Op {
				op: Operator::Dot,
				has_equals: false
			}
		) {
			self.next()?; // consume '.'
			match &self.cur.v {
				TokenValue::Id(name) => {
					path.push(name.clone());
					loc.range.end = self.cur.location.range.end;
					self.next()?; // consume id
				}
				_ => {
					return Err(ParseError::ExpectedToken(
						self.cur.clone(),
						TokenValue::Id("<identifier>".to_string()),
					));
				}
			}
		}

		Ok((path, loc))
	}

	fn parse_package(&mut self) -> ParseResult {
		let mut loc = self.cur.location.clone();
		self.next()?; // consume 'package'
		self.consume_semicolons()?;

		let (path, path_loc) = self.parse_dotted_path()?;
		loc.range.end = path_loc.range.end;

		Ok(AST::from(loc, ASTValue::Package { path }))
	}

	fn parse_use(&mut self) -> ParseResult {
		let mut loc = self.cur.location.clone();
		self.next()?; // consume 'use'
		self.consume_semicolons()?;

		let (path, path_loc) = self.parse_dotted_path()?;
		loc.range.end = path_loc.range.end;

		let alias = if self.cur.v == TokenValue::Keyword(Keyword::As) {
			self.next()?; // consume 'as'
			self.consume_semicolons()?;
			match &self.cur.v {
				TokenValue::Id(name) => {
					loc.range.end = self.cur.location.range.end;
					let name = name.clone();
					self.next()?; // consume alias
					Some(name)
				}
				_ => {
					return Err(ParseError::ExpectedToken(
						self.cur.clone(),
						TokenValue::Id("<identifier>".to_string()),
					));
				}
			}
		} else {
			None
		};

		Ok(AST::from(loc, ASTValue::Use { path, alias }))
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

		let mut pre: Vec<Box<AST>> = Vec::new();
		let mut post: Option<PostClause> = None;

		let contract_stop = &[
			TokenValue::Keyword(Keyword::Pre),
			TokenValue::Keyword(Keyword::Post),
			TokenValue::Keyword(Keyword::Where),
			TokenValue::Keyword(Keyword::Ensures),
			TokenValue::Keyword(Keyword::Do),
			TokenValue::LSquirly,
			TokenValue::EOF,
		];

		while matches!(
			self.cur.v,
			TokenValue::Keyword(Keyword::Pre) | TokenValue::Keyword(Keyword::Post)
		) {
			match self.cur.v.clone() {
				TokenValue::Keyword(Keyword::Pre) => {
					self.next()?; // consume 'pre'
					let list_ast =
						self.parse_expression_list(contract_stop, false)?;
					let exprs = match list_ast.v {
						ASTValue::ExprList(exprs) => exprs,
						_ => unreachable!(
							"expression_list always returns ExprList"
						),
					};
					pre.extend(exprs);
				}
				TokenValue::Keyword(Keyword::Post) => {
					self.next()?; // consume 'post'
					let return_id = match (&self.cur.v, &self.next.v) {
						(TokenValue::Id(name), TokenValue::Colon) => {
							let return_id_tok = self.cur.clone();
							let name = name.clone();
							self.next()?; // consume id
							self.next()?; // consume ':'
							if post.as_ref()
								.and_then(|p| p.return_id.as_ref())
								.is_some()
							{
								return Err(ParseError::PostReturnIdAlreadyDefined(return_id_tok));
							}
							Some(name)
						}
						_ => {
							if self.cur.v != TokenValue::Colon {
								return Err(
									ParseError::ExpectedToken(
										self.cur.clone(),
										TokenValue::Colon,
									),
								);
							}
							self.next()?; // consume ':'
							None
						}
					};

					let list_ast =
						self.parse_expression_list(contract_stop, false)?;
					let conditions = match list_ast.v {
						ASTValue::ExprList(exprs) => exprs,
						_ => unreachable!(
							"expression_list always returns ExprList"
						),
					};

					match &mut post {
						None => {
							post = Some(PostClause {
								return_id,
								conditions,
							});
						}
						Some(existing) => {
							if existing.return_id.is_none() {
								existing.return_id = return_id;
							}
							existing.conditions.extend(conditions);
						}
					}
				}
				_ => unreachable!(),
			}
			self.consume_semicolons()?;
		}

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
			if self.cur.v == TokenValue::Semicolon {
				return Err(ParseError::ExpectedExpression(self.cur.clone()));
			}
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
				pre,
				post,
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

	fn parse_enum(&mut self) -> ParseResult {
		let mut loc = self.cur.location.clone();
		self.next()?; // consume 'enum'

		if self.cur.v != TokenValue::LSquirly {
			return Err(ParseError::ExpectedToken(
				self.cur.clone(),
				TokenValue::LSquirly,
			));
		}
		self.next()?; // consume '{'

		let mut variants: Vec<EnumVariant> = Vec::new();
		while self.cur.v != TokenValue::RSquirly {
			if self.cur.v == TokenValue::EOF {
				return Err(ParseError::ExpectedToken(
					self.cur.clone(),
					TokenValue::RSquirly,
				));
			}

			let name = match &self.cur.v {
				TokenValue::Id(name) => name.clone(),
				_ => {
					return Err(ParseError::ExpectedToken(
						self.cur.clone(),
						TokenValue::Id("enum_variant".to_string()),
					));
				}
			};
			self.next()?; // consume variant name

			let value = if matches!(
				self.cur.v,
				TokenValue::Op {
					op: Operator::Set,
					has_equals: false
				}
			) {
				self.next()?; // consume '='
				Some(self.parse_expression_with_stop(&[
					TokenValue::Comma,
					TokenValue::Semicolon,
					TokenValue::RSquirly,
				])?)
			} else {
				None
			};

			variants.push(EnumVariant { name, value });

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

		Ok(AST::from(loc, ASTValue::Enum { variants }))
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
		let mut methods: Vec<Box<AST>> = Vec::new();
		while self.cur.v != TokenValue::RSquirly {
			if matches!(self.cur.v, TokenValue::Semicolon | TokenValue::Comma) {
				self.next()?;
				continue;
			}
			let is_decl = matches!(self.cur.v, TokenValue::Keyword(Keyword::Pub))
				|| matches!(self.cur.v, TokenValue::Id(_))
					&& matches!(self.next.v, TokenValue::Colon);
			if is_decl {
				methods.push(self.parse_expression()?);
				if matches!(self.cur.v, TokenValue::Semicolon | TokenValue::Comma) {
					self.next()?;
				}
				continue;
			}

			variants.push(self.parse_type_inner()?);

			match self.cur.v {
				TokenValue::Semicolon | TokenValue::Comma => {
					self.next()?;
					if self.cur.v == TokenValue::RSquirly {
						break;
					}
				}
				TokenValue::RSquirly => break,
				TokenValue::Id(_) | TokenValue::Keyword(Keyword::Pub) => {}
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

		Ok(AST::from(
			loc,
			ASTValue::Union {
				generics,
				variants,
				methods,
			},
		))
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

		let body = self.parse_block_or_do(&[
			TokenValue::Semicolon,
			TokenValue::EOF,
			TokenValue::RSquirly,
			TokenValue::RParen,
			TokenValue::RBracket,
			TokenValue::Keyword(Keyword::Else),
		])?;
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
					return Err(ParseError::ExpectedBody(
						self.cur.location.clone(),
					));
				}

				let else_body = self.parse_block_or_do(&[
					TokenValue::Semicolon,
					TokenValue::EOF,
					TokenValue::RSquirly,
					TokenValue::RParen,
					TokenValue::RBracket,
				])?;
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

		if matches!(
			self.cur.v,
			TokenValue::LSquirly | TokenValue::Keyword(Keyword::Do)
		) {
			let body = self.parse_block_or_do(&[
				TokenValue::Semicolon,
				TokenValue::EOF,
				TokenValue::RSquirly,
			])?;
			loc.range.end = body.location.range.end;
			return Ok(AST::from(
				loc,
				ASTValue::ForLoop {
					init: None,
					cond: None,
					step: None,
					body,
				},
			));
		}

		#[allow(clippy::vec_box)]
		fn clause_from_exprs(exprs: Vec<Box<AST>>) -> Option<Box<AST>> {
			match exprs.len() {
				0 => None,
				1 => Some(exprs.into_iter().next().unwrap()),
				_ => {
					let mut clause_loc = exprs[0].location.clone();
					clause_loc.range.end =
						exprs.last().unwrap().location.range.end;
					Some(AST::from(clause_loc, ASTValue::ExprList(exprs)))
				}
			}
		}

		let header_ast = self.parse_expression_list(
			&[
				TokenValue::Keyword(Keyword::In),
				TokenValue::Keyword(Keyword::Do),
				TokenValue::LSquirly,
				TokenValue::Semicolon,
			],
			true,
		)?;

		let header_exprs = match *header_ast {
			AST {
				v: ASTValue::ExprList(exprs),
				..
			} => exprs,
			_ => unreachable!("header_ast is always ExprList"),
		};

		match self.cur.v.clone() {
			TokenValue::Keyword(Keyword::In) => {
				if header_exprs.is_empty() {
					return Err(ParseError::ExpectedExpression(
						self.cur.clone(),
					));
				}

				let mut bindings = Vec::with_capacity(header_exprs.len());
				for binding in header_exprs {
					match &binding.v {
						ASTValue::Id(_) => bindings.push(binding),
						ASTValue::Ref { v, .. }
							if matches!(v.v, ASTValue::Id(_)) =>
						{
							bindings.push(binding)
						}
						_ => {
							return Err(ParseError::InvalidForBinding(
								binding.location.clone(),
							));
						}
					}
				}

				self.next()?; // consume `in`

				if matches!(
					self.cur.v,
					TokenValue::LSquirly | TokenValue::Keyword(Keyword::Do)
				) {
					return Err(ParseError::ExpectedExpression(
						self.cur.clone(),
					));
				}

				let iter = self.parse_expression()?;
				if self.cur.v == TokenValue::Semicolon {
					return Err(ParseError::ExpectedBody(
						self.cur.location.clone(),
					));
				}

				let body = self.parse_block_or_do(&[
					TokenValue::Semicolon,
					TokenValue::EOF,
					TokenValue::RSquirly,
				])?;
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
			TokenValue::LSquirly | TokenValue::Keyword(Keyword::Do) => {
				if header_exprs.len() > 1 {
					return Err(ParseError::ExpectedToken(
						self.cur.clone(),
						TokenValue::Keyword(Keyword::In),
					));
				}

				let cond = header_exprs.into_iter().next();
				let body = self.parse_block_or_do(&[
					TokenValue::Semicolon,
					TokenValue::EOF,
					TokenValue::RSquirly,
				])?;
				loc.range.end = body.location.range.end;

				Ok(AST::from(
					loc,
					ASTValue::ForLoop {
						init: None,
						cond,
						step: None,
						body,
					},
				))
			}
			TokenValue::Semicolon => {
				let init = clause_from_exprs(header_exprs);
				self.next()?; // consume first `;`

				let cond = if matches!(
					self.cur.v,
					TokenValue::Semicolon | TokenValue::LSquirly
				) {
					None
				} else {
					Some(self.parse_expression_with_stop(&[
						TokenValue::Semicolon,
						TokenValue::LSquirly,
					])?)
				};

				if matches!(
					self.cur.v,
					TokenValue::LSquirly | TokenValue::Keyword(Keyword::Do)
				) {
					let body = self.parse_block_or_do(&[
						TokenValue::Semicolon,
						TokenValue::EOF,
						TokenValue::RSquirly,
					])?;
					loc.range.end = body.location.range.end;
					return Ok(AST::from(
						loc,
						ASTValue::ForLoop {
							init,
							cond,
							step: None,
							body,
						},
					));
				}

				if self.cur.v != TokenValue::Semicolon {
					return Err(ParseError::ExpectedToken(
						self.cur.clone(),
						TokenValue::Semicolon,
					));
				}
				self.next()?; // consume second `;`

				let step = if matches!(
					self.cur.v,
					TokenValue::LSquirly | TokenValue::Keyword(Keyword::Do)
				) {
					None
				} else {
					Some(self.parse_expression_with_stop(&[
						TokenValue::LSquirly,
					])?)
				};

				if self.cur.v == TokenValue::Semicolon {
					return Err(ParseError::ExpectedBody(
						self.cur.location.clone(),
					));
				}

				let body = self.parse_block_or_do(&[
					TokenValue::Semicolon,
					TokenValue::EOF,
					TokenValue::RSquirly,
				])?;
				loc.range.end = body.location.range.end;

				Ok(AST::from(
					loc,
					ASTValue::ForLoop {
						init,
						cond,
						step,
						body,
					},
				))
			}
			_ => unreachable!("parse_expression_list must stop on `in`, `{{`, or `;`"),
		}
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
		let mut ast = ast;
		ast.location.range.end = self.cur.location.range.end;
		self.next()?;

		Ok(ast)
	}

	fn parse_block_or_do(&mut self, stop: &[TokenValue]) -> ParseResult {
		if self.cur.v == TokenValue::Keyword(Keyword::Do) {
			self.next()?; // consume 'do'
			if self.cur.v == TokenValue::Semicolon {
				return Err(ParseError::ExpectedExpression(self.cur.clone()));
			}
			self.parse_expression_with_stop(stop)
		} else {
			self.parse_block()
		}
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
			TokenValue::Keyword(Keyword::Void) => {
				self.next()?;
				Ok(Box::new(Type::Void))
			}
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
						// [<id>]T or [<id>.<id>...]T
						let size =
							match self.parse_array_size_idish() {
								Ok(size) => size,
								Err(err) => {
									let _ = self.recover_to(&[TokenValue::RBracket], false);
									if self.cur.v == TokenValue::RBracket {
                                    let _ = self.next();
                                }
									return Err(err);
								}
							};
						if self.cur.v != TokenValue::RBracket {
							let err = ParseError::InvalidArraySize(
								self.cur.location.clone(),
							);
							let _ = self.recover_to(
								&[TokenValue::RBracket],
								false,
							);
							if self.cur.v == TokenValue::RBracket {
								let _ = self.next();
							}
							return Err(err);
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

	fn parse_array_size_idish(&mut self) -> Result<String, ParseError> {
		let mut size = String::new();

		let TokenValue::Id(name) = &self.cur.v else {
			return Err(ParseError::InvalidArraySize(self.cur.location.clone()));
		};
		size.push_str(name);
		self.next()?;

		while matches!(
			self.cur.v,
			TokenValue::Op {
				op: Operator::Dot,
				has_equals: false
			}
		) {
			self.next()?; // consume '.'
			let TokenValue::Id(name) = &self.cur.v else {
				return Err(ParseError::InvalidArraySize(
					self.cur.location.clone(),
				));
			};
			size.push('.');
			size.push_str(name);
			self.next()?;
		}

		Ok(size)
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
		let start_tok = self.cur.clone();
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
			let (args, _end) = self.parse_generic_args()?;
			return Ok(Box::new(Type::Generic { base: full, args }));
		}
		if Self::is_invalid_numeric_type_name(full.as_str()) {
			return Err(ParseError::InvalidNumericType(start_tok));
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
			let bits = s.parse::<u16>().ok()?;
			if bits == 0 || bits > 128 {
				return None;
			}
			if bits == 128 || bits <= 64 {
				return Some(bits as u8);
			}
			None
		}

		if let Some(rest) = name.strip_prefix('u') {
			let bit_size = parse_bits(rest)?;
			if bit_size == 1 {
				return None;
			}
			return Some(Type::Integer {
				bit_size,
				signed: false,
			});
		}
		if let Some(rest) = name.strip_prefix('i') {
			let bit_size = parse_bits(rest)?;
			if bit_size == 1 {
				return None;
			}
			return Some(Type::Integer {
				bit_size,
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

	fn is_invalid_numeric_type_name(name: &str) -> bool {
		for prefix in ['u', 'i', 'f'] {
			if let Some(rest) = name.strip_prefix(prefix)
				&& rest.chars().all(|c| c.is_ascii_digit())
				&& Self::type_from_numeric_name(name).is_none()
			{
				return true;
			}
		}
		false
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

	fn record_error(&mut self, err: ParseError) {
		self.errors.push(err);
	}

	fn recover_to(
		&mut self,
		list_end: &[TokenValue],
		only_comma: bool,
	) -> Result<(), ParseError> {
		loop {
			if list_end.contains(&self.cur.v) || self.cur.v == TokenValue::EOF {
				break;
			}
			let is_separator = if only_comma {
				self.cur.v == TokenValue::Comma
			} else {
				matches!(self.cur.v, TokenValue::Comma | TokenValue::Semicolon)
			};
			if is_separator {
				self.next()?;
				break;
			}
			self.next()?;
		}
		Ok(())
	}

	fn recover_after_invalid_array_size(&mut self, only_comma: bool) -> Result<(), ParseError> {
		loop {
			if self.cur.v == TokenValue::EOF {
				break;
			}
			let is_separator = if only_comma {
				self.cur.v == TokenValue::Comma
			} else {
				matches!(self.cur.v, TokenValue::Comma | TokenValue::Semicolon)
			};
			if is_separator {
				self.next()?;
				break;
			}
			if matches!(self.cur.v, TokenValue::RSquirly | TokenValue::RParen) {
				break;
			}
			self.next()?;
		}
		Ok(())
	}

	pub fn take_errors(&mut self) -> Vec<ParseError> {
		std::mem::take(&mut self.errors)
	}

	fn parse_generic_args(&mut self) -> Result<(Vec<GenericArg>, (i32, i32)), ParseError> {
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
			let end = self.cur.location.range.end;
			self.next()?; // consume '>'
			self.enable_multi_id_parse = enable_multi_id_parse_prev;
			return Ok((args, end));
		}

		let end = loop {
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
				let end = self.cur.location.range.end;
				self.next()?; // consume '>'
				break end;
			}

			self.enable_multi_id_parse = enable_multi_id_parse_prev;
			return Err(ParseError::ExpectedToken(
				self.cur.clone(),
				TokenValue::Op {
					op: Operator::GreaterThan,
					has_equals: false,
				},
			));
		};

		self.enable_multi_id_parse = enable_multi_id_parse_prev;
		Ok((args, end))
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
					| Type::Void
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

	fn parse_multi_id(&mut self) -> ParseResult {
		let mut loc = self.cur.location.clone();

		let mut names = Vec::<String>::new();
		while matches!(
			self.cur.v,
			TokenValue::Id(_) | TokenValue::Keyword(Keyword::Operator)
		) {
			let (name, end_loc) = self.parse_decl_name()?;
			names.push(name);
			loc.range.end = end_loc.range.end;
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
					_ => unreachable!(
						"expression_list always returns ExprList"
					),
				};

				if names.len() == 1 && values.len() == 1 {
					AST::from(
						loc,
						ASTValue::Set(
							names[0].clone(),
							values.into_iter().next().unwrap(),
						),
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
							_ => unreachable!(
								"expression_list always returns ExprList"
							),
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
							_ => unreachable!(
								"expression_list always returns ExprList"
							),
						};
						loc.range.end = rhs_list.location.range.end;
						(true, Some(values))
					}
					_ => {
						if types.is_empty() {
							return Err(
								ParseError::InvalidDeclarationType(
									self.cur.clone(),
								),
							);
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

	fn parse_decl_name(&mut self) -> Result<(String, SourceLocation), ParseError> {
		match &self.cur.v {
			TokenValue::Id(name) => {
				let end_loc = self.cur.location.clone();
				let name = name.clone();
				self.next()?;
				Ok((name, end_loc))
			}
			TokenValue::Keyword(Keyword::Operator) => {
				self.next()?; // consume 'operator'
				match &self.cur.v {
					TokenValue::Op { op, has_equals } => {
						let end_loc = self.cur.location.clone();
						let sym = Self::operator_symbol(op, *has_equals);
						self.next()?;
						Ok((format!("operator{sym}"), end_loc))
					}
					TokenValue::LBracket => {
						self.next()?; // consume '['
						if self.cur.v != TokenValue::RBracket {
							return Err(
								ParseError::UnclosedOperatorName(
									self.cur.clone(),
									TokenValue::RBracket,
								),
							);
						}
						let end_loc = self.cur.location.clone();
						self.next()?; // consume ']'
						Ok(("operator[]".to_string(), end_loc))
					}
					TokenValue::LParen => {
						self.next()?; // consume '('
						if self.cur.v != TokenValue::RParen {
							return Err(
								ParseError::UnclosedOperatorName(
									self.cur.clone(),
									TokenValue::RParen,
								),
							);
						}
						let end_loc = self.cur.location.clone();
						self.next()?; // consume ')'
						Ok(("operator()".to_string(), end_loc))
					}
					_ => Err(ParseError::InvalidOperatorName(self.cur.clone())),
				}
			}
			_ => Err(ParseError::ExpectedToken(
				self.cur.clone(),
				TokenValue::Id("name".to_string()),
			)),
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
		#[derive(Copy, Clone, PartialEq, Eq)]
		enum GenericSection {
			Lifetime,
			NonLifetime,
		}
		let mut section: Option<GenericSection> = None;
		if Self::is_gt(&self.cur.v) {
			self.next()?; // consume '>'
			self.enable_multi_id_parse = enable_multi_id_parse_prev;
			return Ok(params);
		}

		while !Self::is_gt(&self.cur.v) {
			if let Some(section_kind) = section {
				let current_kind = match &self.cur.v {
					TokenValue::Lifetime(_) => GenericSection::Lifetime,
					TokenValue::Id(_) => GenericSection::NonLifetime,
					_ => section_kind,
				};
				if current_kind != section_kind {
					self.enable_multi_id_parse = enable_multi_id_parse_prev;
					return Err(ParseError::UnexpectedToken(
						self.cur.clone(),
						TokenValue::Semicolon,
					));
				}
			}

			match &self.cur.v {
				TokenValue::Lifetime(c) => {
					section.get_or_insert(GenericSection::Lifetime);
					params.push(GenericParam::Lifetime(*c));
					self.next()?;
				}
				TokenValue::Id(_) => {
					section.get_or_insert(GenericSection::NonLifetime);
					let name = match &self.cur.v {
						TokenValue::Id(name) => name.clone(),
						_ => unreachable!("checked by match"),
					};
					self.next()?;

					let constraint = if self.cur.v == TokenValue::Colon {
						self.next()?; // consume ':'
						Some(self.parse_type_inner()?)
					} else {
						None
					};

					let is_type_param = match constraint.as_ref() {
						None => true,
						Some(t) => {
							matches!(t.as_ref(), Type::Id(name) if name == "type")
						}
					};

					if is_type_param {
						let constraint = Some(Box::new(Type::Id(
							"type".to_string(),
						)));
						params.push(GenericParam::Type {
							names: vec![name],
							constraint,
						});
					} else {
						params.push(GenericParam::Value {
							names: vec![name],
							ty: constraint.expect(
								"value param requires type",
							),
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
				TokenValue::Comma => {
					self.next()?;
					if Self::is_gt(&self.cur.v) {
						break;
					}
					continue;
				}
				TokenValue::Semicolon => {
					self.next()?;
					section = None;
					if Self::is_gt(&self.cur.v) {
						break;
					}
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

			params.push(FnParam { names, ty, default });

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

	fn is_adjacent_duplicate_op(&self, op: Operator) -> bool {
		let TokenValue::Op {
			op: cur_op,
			has_equals: false,
		} = &self.cur.v
		else {
			return false;
		};
		if *cur_op != op {
			return false;
		}
		let TokenValue::Op {
			op: next_op,
			has_equals: false,
		} = &self.next.v
		else {
			return false;
		};
		if *next_op != op {
			return false;
		}
		self.cur.location.range.end.1 == self.next.location.range.begin.1
			&& self.cur.location.range.end.0 == self.next.location.range.begin.0
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
			) || matches!(v, TokenValue::LBracket)
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
				let first = iter.next().expect(
					"expected parser to produce at least one expression",
				);
				assert!(
					iter.next().is_none(),
					"expected a single expression in the input"
				);
				Ok(first)
			}
			_ => panic!("parser should wrap expressions in ExprList"),
		}
	}

	#[test]
	fn package_directive_parses_dotted_path() {
		let ast = parse_expr(Lexer::new("package main".to_string(), "<test>".to_string()))
			.expect("ok");
		match &ast.v {
			ASTValue::Package { path } => assert_eq!(path, &["main".to_string()]),
			_ => panic!("expected Package, got {}", ast),
		}
	}

	#[test]
	fn use_directive_parses_dotted_path() {
		let ast = parse_expr(Lexer::new("use core.io".to_string(), "<test>".to_string()))
			.expect("ok");
		match &ast.v {
			ASTValue::Use { path, alias } => {
				assert_eq!(path, &["core".to_string(), "io".to_string()]);
				assert!(alias.is_none());
			}
			_ => panic!("expected Use, got {}", ast),
		}
	}

	#[test]
	fn use_directive_parses_alias() {
		let ast = parse_expr(Lexer::new(
			"use core.io as cio".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");
		match &ast.v {
			ASTValue::Use { path, alias } => {
				assert_eq!(path, &["core".to_string(), "io".to_string()]);
				assert_eq!(alias.as_deref(), Some("cio"));
			}
			_ => panic!("expected Use, got {}", ast),
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
			ASTValue::Id(name) => {
				assert_eq!(name, expected, "expected id {}", expected)
			}
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

	fn expect_call<'a>(ast: &'a AST) -> (&'a AST, &'a [Box<AST>]) {
		match &ast.v {
			ASTValue::Call { callee, args } => (callee.as_ref(), args.as_slice()),
			_ => panic!("expected Call, got {}", ast),
		}
	}

	fn expect_named_arg<'a>(ast: &'a AST) -> (&'a str, &'a AST) {
		match &ast.v {
			ASTValue::NamedArg { name, value } => (name.as_str(), value.as_ref()),
			_ => panic!("expected NamedArg, got {}", ast),
		}
	}

	fn expect_generic_apply<'a>(ast: &'a AST) -> (&'a AST, &'a [GenericArg]) {
		match &ast.v {
			ASTValue::GenericApply { target, args } => {
				(target.as_ref(), args.as_slice())
			}
			_ => panic!("expected GenericApply, got {}", ast),
		}
	}

	fn expect_pub<'a>(ast: &'a AST) -> &'a AST {
		match &ast.v {
			ASTValue::Pub(inner) => inner.as_ref(),
			_ => panic!("expected Pub, got {}", ast),
		}
	}

	fn expect_initializer_list<'a>(ast: &'a AST) -> &'a [crate::frontend::InitializerItem] {
		match &ast.v {
			ASTValue::InitializerList(items) => items.as_slice(),
			_ => panic!("expected InitializerList, got {}", ast),
		}
	}

	fn expect_typed_initializer_list<'a>(
		ast: &'a AST,
	) -> (
		&'a crate::frontend::Type,
		&'a [crate::frontend::InitializerItem],
	) {
		match &ast.v {
			ASTValue::TypedInitializerList { ty, items } => {
				(ty.as_ref(), items.as_slice())
			}
			_ => panic!("expected TypedInitializerList, got {}", ast),
		}
	}

	#[test]
	fn defer_parses_as_expression() {
		let ast = parse_expr(Lexer::new(
			"defer cleanup()".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");
		match &ast.v {
			ASTValue::Defer(inner) => {
				let has_call = matches!(inner.v, ASTValue::Call { .. })
					|| matches!(
						inner.v,
						ASTValue::BinExpr {
							op: Operator::Dot,
							rhs: _,
							..
						}
					) && inner.to_string().contains("(Call");
				assert!(
					has_call,
					"expected defer payload to contain a call, got {}",
					inner
				);
			}
			_ => panic!("expected Defer, got {}", ast),
		}
	}

	#[test]
	fn match_parses_with_cases_and_default() {
		let ast = parse_expr(Lexer::new(
			"match r { case \"x\" if 0 < N -> 0; case -> e; }".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");

		match &ast.v {
			ASTValue::Match {
				binder,
				scrutinee,
				cases,
			} => {
				assert!(binder.is_none());
				expect_id(scrutinee, "r");
				assert_eq!(cases.len(), 2);

				match &cases[0].pattern {
					MatchCasePattern::Exprs(pats) => assert_eq!(pats.len(), 1),
					_ => panic!("expected Exprs pattern"),
				}
				assert!(cases[0].guard.is_some());
				expect_integer(&cases[0].body, 0);

				assert!(matches!(cases[1].pattern, MatchCasePattern::Default));
				assert!(cases[1].guard.is_none());
			}
			_ => panic!("expected Match, got {}", ast),
		}
	}

	#[test]
	fn match_parses_with_binder_and_type_cases() {
		let ast = parse_expr(Lexer::new(
			"match &v in expr { case core.Foo -> 1; case -> 0; }".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");

		match &ast.v {
			ASTValue::Match {
				binder,
				scrutinee,
				cases,
			} => {
				let b = binder.as_ref().expect("expected binder");
				assert!(b.by_ref);
				assert_eq!(b.name, "v");
				expect_id(scrutinee, "expr");
				assert_eq!(cases.len(), 2);
				assert!(matches!(cases[0].pattern, MatchCasePattern::Type(_)));
				assert!(matches!(cases[1].pattern, MatchCasePattern::Default));
			}
			_ => panic!("expected Match, got {}", ast),
		}
	}

	#[test]
	fn pointer_prefix_and_postfix_parse() {
		let ast = parse_expr(Lexer::new("(^x)^".to_string(), "<test>".to_string()))
			.expect("ok");
		match &ast.v {
			ASTValue::Deref(inner) => match &inner.v {
				ASTValue::PtrOf(ptr_inner) => {
					expect_id(ptr_inner, "x");
				}
				_ => panic!("expected PtrOf inside Deref, got {}", inner),
			},
			_ => panic!("expected Deref, got {}", ast),
		}
	}

	#[test]
	fn pointer_of_binds_tighter_than_equality() {
		fn expect_ptr_of_deref_id(ast: &AST, name: &str) {
			match &ast.v {
				ASTValue::PtrOf(inner) => match &inner.v {
					ASTValue::Deref(inner) => expect_id(inner, name),
					_ => panic!("expected Deref inside PtrOf, got {}", inner),
				},
				_ => panic!("expected PtrOf, got {}", ast),
			}
		}

		let ast = parse_expr(Lexer::new("^p^ == ^(p^)".to_string(), "<test>".to_string()))
			.expect("ok");

		match &ast.v {
			ASTValue::BinExpr {
				op: Operator::Set,
				has_eq: true,
				lhs,
				rhs,
			} => {
				expect_ptr_of_deref_id(lhs, "p");
				expect_ptr_of_deref_id(rhs, "p");
			}
			_ => panic!("expected == expression, got {}", ast),
		}
	}

	#[test]
	fn pointer_of_can_wrap_member_expressions() {
		fn expect_ptr_of_deref_dot_deref_id(ast: &AST, lhs_name: &str, rhs_name: &str) {
			match &ast.v {
				ASTValue::PtrOf(inner) => match &inner.v {
					ASTValue::Deref(inner) => match &inner.v {
						ASTValue::BinExpr {
							op: Operator::Dot,
							has_eq: false,
							lhs,
							rhs,
						} => {
							match &lhs.v {
								ASTValue::Deref(inner) => {
									expect_id(inner, lhs_name)
								}
								_ => panic!(
									"expected Deref on dot lhs, got {}",
									lhs
								),
							}
							expect_id(rhs, rhs_name);
						}
						_ => panic!(
							"expected Dot inside Deref, got {}",
							inner
						),
					},
					_ => panic!("expected Deref inside PtrOf, got {}", inner),
				},
				_ => panic!("expected PtrOf, got {}", ast),
			}
		}

		let ast = parse_expr(Lexer::new(
			"^p^.b^ == ^(p^.b^)".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");

		match &ast.v {
			ASTValue::BinExpr {
				op: Operator::Set,
				has_eq: true,
				lhs,
				rhs,
			} => {
				expect_ptr_of_deref_dot_deref_id(lhs, "p", "b");
				expect_ptr_of_deref_dot_deref_id(rhs, "p", "b");
			}
			_ => panic!("expected == expression, got {}", ast),
		}
	}

	#[test]
	fn ref_binds_tighter_than_equality() {
		let ast = parse_expr(Lexer::new("&a == &(a)".to_string(), "<test>".to_string()))
			.expect("ok");

		match &ast.v {
			ASTValue::BinExpr {
				op: Operator::Set,
				has_eq: true,
				lhs,
				rhs,
			} => {
				match &lhs.v {
					ASTValue::Ref { v, .. } => expect_id(v, "a"),
					_ => panic!("expected Ref on lhs, got {}", lhs),
				}
				match &rhs.v {
					ASTValue::Ref { v, .. } => expect_id(v, "a"),
					_ => panic!("expected Ref on rhs, got {}", rhs),
				}
			}
			_ => panic!("expected == expression, got {}", ast),
		}
	}

	#[test]
	fn generic_params_require_semicolon_to_switch_kinds() {
		let err = match parse_expr(Lexer::new(
			"fn<T, 'a>() do 1".to_string(),
			"<test>".to_string(),
		)) {
			Ok(ast) => panic!("expected error, got {}", ast),
			Err(e) => e,
		};
		match err {
			ParseError::UnexpectedToken(tok, TokenValue::Semicolon) => match tok.v {
				TokenValue::Lifetime('a') => {}
				_ => panic!("expected lifetime token, got {:?}", tok.v),
			},
			_ => panic!("expected UnexpectedToken(..., Semicolon)"),
		}

		let ok = parse_expr(Lexer::new(
			"fn<T; 'a>() do 1".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");
		match &ok.v {
			ASTValue::Fn { generics, .. } => assert_eq!(generics.len(), 2),
			_ => panic!("expected Fn, got {}", ok),
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
			ASTValue::SetMulti { names, values } => {
				(names.as_slice(), values.as_slice())
			}
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

	fn expect_type_ref(
		t: &crate::frontend::Type,
	) -> (&crate::frontend::Type, bool, Option<char>) {
		match t {
			crate::frontend::Type::Reference {
				underlying,
				mutable,
				lifetime,
			} => (underlying.as_ref(), *mutable, *lifetime),
			_ => panic!("expected Reference type"),
		}
	}

	fn expect_type_array<'a>(
		t: &'a crate::frontend::Type,
	) -> (&'a str, &'a crate::frontend::Type) {
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
			ASTValue::While { decl, cond, body } => {
				(decl.as_deref(), cond.as_ref(), body.as_ref())
			}
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

	fn expect_for_loop<'a>(
		ast: &'a AST,
	) -> (Option<&'a AST>, Option<&'a AST>, Option<&'a AST>, &'a AST) {
		match &ast.v {
			ASTValue::ForLoop {
				init,
				cond,
				step,
				body,
			} => (
				init.as_deref(),
				cond.as_deref(),
				step.as_deref(),
				body.as_ref(),
			),
			_ => panic!("expected ForLoop expression"),
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
				ASTValue::UnaryPlus(inner) => match inner.v {
					ASTValue::Integer(n) => assert_eq!(n, 42),
					_ => panic!("expected Integer after unary +"),
				},
				_ => panic!("expected UnaryPlus after unary +"),
			},
		}
	}

	#[test]
	fn unary_minus_is_unary_op() {
		let lx = Lexer::new("-7".to_string(), "<test>".to_string());
		let ast = parse_one(lx).expect("ok");
		match *ast {
			AST { ref v, .. } => match v {
				ASTValue::UnaryMinus(inner) => match inner.v {
					ASTValue::Integer(n) => assert_eq!(n, 7),
					_ => panic!("expected Integer after unary -"),
				},
				_ => panic!("expected UnaryMinus after unary -"),
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
		let ast = parse_expr(Lexer::new("a, b = b, a".to_string(), "<test>".to_string()))
			.expect("ok");

		let (names, values) = expect_set_multi(ast.as_ref());
		assert_eq!(names, ["a".to_string(), "b".to_string()]);
		assert_eq!(values.len(), 2);
		expect_id(values[0].as_ref(), "b");
		expect_id(values[1].as_ref(), "a");
	}

	#[test]
	fn decl_default_init_requires_types() {
		let ast = parse_expr(Lexer::new("a, b: T, U".to_string(), "<test>".to_string()))
			.expect("ok");

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
		let ast = parse_expr(Lexer::new("a, b := 1, 2".to_string(), "<test>".to_string()))
			.expect("ok");

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
		let ast = parse_expr(Lexer::new("x :: 1".to_string(), "<test>".to_string()))
			.expect("ok");

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
								crate::frontend::Type::Id(
									"T".to_string()
								)
							);
						}
						_ => panic!("expected reference type arg"),
					},
					_ => panic!("expected type arg"),
				}
				assert_eq!(
					args[1],
					crate::frontend::GenericArg::Name("M".to_string())
				);
			}
			_ => panic!("expected Generic type"),
		}
	}

	#[test]
	fn call_with_generics_disambiguates_from_comparison() {
		let ast = parse_expr(Lexer::new(
			"core.math.Vector<&'a T, M>()".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");

		let (callee, args) = expect_call(ast.as_ref());
		assert!(args.is_empty());

		let (target, gen_args) = expect_generic_apply(callee);
		assert_eq!(gen_args.len(), 2);
		match &gen_args[0] {
			GenericArg::Type(t) => match t.as_ref() {
				Type::Reference {
					mutable,
					lifetime,
					underlying,
				} => {
					assert!(!*mutable);
					assert_eq!(*lifetime, Some('a'));
					assert_eq!(underlying.as_ref(), &Type::Id("T".to_string()));
				}
				_ => panic!("expected reference type arg"),
			},
			_ => panic!("expected type arg"),
		}
		assert_eq!(gen_args[1], GenericArg::Name("M".to_string()));

		match &target.v {
			ASTValue::BinExpr {
				op: Operator::Dot,
				lhs,
				rhs,
				has_eq: false,
			} => {
				expect_id(rhs.as_ref(), "Vector");
				match &lhs.v {
					ASTValue::BinExpr {
						op: Operator::Dot,
						lhs,
						rhs,
						has_eq: false,
					} => {
						expect_id(lhs.as_ref(), "core");
						expect_id(rhs.as_ref(), "math");
					}
					_ => panic!("expected core.math qualifier, got {}", lhs),
				}
			}
			_ => panic!("expected dotted target, got {}", target),
		}
	}

	#[test]
	fn comparison_wins_over_ambiguous_single_name_generic_call() {
		let ast = parse_expr(Lexer::new("a < b > (c)".to_string(), "<test>".to_string()))
			.expect("ok");
		let s = ast.to_string();
		assert!(
			s.contains("BinExp [op: LessThan") && s.contains("BinExp [op: GreaterThan"),
			"expected chained comparisons, got {s}"
		);
		assert!(!s.contains("GenericApply"), "unexpected generic apply: {s}");
	}

	#[test]
	fn pub_can_wrap_declarations() {
		let ast = parse_expr(Lexer::new(
			"pub main := 1".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");

		let inner = expect_pub(ast.as_ref());
		let (constexpr, names, types, values) = expect_declaration_multi(inner);
		assert!(!constexpr);
		assert_eq!(names, &["main".to_string()]);
		assert!(types.is_empty());
		let values = values.expect("values");
		assert_eq!(values.len(), 1);
		expect_integer(values[0].as_ref(), 1);
	}

	#[test]
	fn initializer_list_positional_parses() {
		let ast = parse_expr(Lexer::new(".{1, 2, 3}".to_string(), "<test>".to_string()))
			.expect("ok");
		let items = expect_initializer_list(ast.as_ref());
		assert_eq!(items.len(), 3);
		match &items[0] {
			crate::frontend::InitializerItem::Positional(v) => {
				expect_integer(v.as_ref(), 1)
			}
			_ => panic!("expected positional item"),
		}
	}

	#[test]
	fn initializer_list_named_uses_equals() {
		let ast = parse_expr(Lexer::new(
			".{ .x = 1, .y = 2 }".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");
		let items = expect_initializer_list(ast.as_ref());
		assert_eq!(items.len(), 2);
		match &items[0] {
			crate::frontend::InitializerItem::Named { name, value } => {
				assert_eq!(name, "x");
				expect_integer(value.as_ref(), 1);
			}
			_ => panic!("expected named item"),
		}
	}

	#[test]
	fn initializer_list_named_allows_semicolons() {
		let ast = parse_expr(Lexer::new(
			".{ .x = 1; .y = 2 }".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");
		let items = expect_initializer_list(ast.as_ref());
		assert_eq!(items.len(), 2);
		match &items[1] {
			crate::frontend::InitializerItem::Named { name, value } => {
				assert_eq!(name, "y");
				expect_integer(value.as_ref(), 2);
			}
			_ => panic!("expected named item"),
		}
	}

	#[test]
	fn initializer_list_named_shorthand_uses_identifier_value() {
		let ast = parse_expr(Lexer::new(".{ .x }".to_string(), "<test>".to_string()))
			.expect("ok");
		let items = expect_initializer_list(ast.as_ref());
		assert_eq!(items.len(), 1);
		match &items[0] {
			crate::frontend::InitializerItem::Named { name, value } => {
				assert_eq!(name, "x");
				expect_id(value.as_ref(), "x");
			}
			_ => panic!("expected named shorthand item"),
		}
	}

	#[test]
	fn initializer_list_mixed_shorthand_and_explicit_named() {
		let ast = parse_expr(Lexer::new(
			".{ .x, .y = 2 }".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");
		let items = expect_initializer_list(ast.as_ref());
		assert_eq!(items.len(), 2);
		match &items[0] {
			crate::frontend::InitializerItem::Named { name, value } => {
				assert_eq!(name, "x");
				expect_id(value.as_ref(), "x");
			}
			_ => panic!("expected named shorthand item"),
		}
		match &items[1] {
			crate::frontend::InitializerItem::Named { name, value } => {
				assert_eq!(name, "y");
				expect_integer(value.as_ref(), 2);
			}
			_ => panic!("expected named item"),
		}
	}

	#[test]
	fn initializer_list_cannot_mix_positional_and_named() {
		let err = parse_expr(Lexer::new(
			".{ 1, .x = 2 }".to_string(),
			"<test>".to_string(),
		))
		.err()
		.expect("should be Err");
		match err {
			ParseError::MixedInitializerListStyles(_) => {}
			other => panic!("expected MixedInitializerListStyles, got {:?}", other),
		}

		let err = parse_expr(Lexer::new(
			".{ .x = 1, 2 }".to_string(),
			"<test>".to_string(),
		))
		.err()
		.expect("should be Err");
		match err {
			ParseError::MixedInitializerListStyles(_) => {}
			other => panic!("expected MixedInitializerListStyles, got {:?}", other),
		}
	}

	#[test]
	fn initializer_list_can_be_call_argument() {
		let ast = parse_expr(Lexer::new(
			"Vector<f32, 3>(.{1, 2, 3})".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");
		let (_callee, args) = expect_call(ast.as_ref());
		assert_eq!(args.len(), 1);
		let items = expect_initializer_list(args[0].as_ref());
		assert_eq!(items.len(), 3);
	}

	#[test]
	fn call_allows_named_args() {
		let ast = parse_expr(Lexer::new(
			"make(width: 640, height: 480)".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");
		let (_callee, args) = expect_call(ast.as_ref());
		assert_eq!(args.len(), 2);
		let (name, value) = expect_named_arg(args[0].as_ref());
		assert_eq!(name, "width");
		expect_integer(value, 640);
	}

	#[test]
	fn typed_initializer_list_parses() {
		let ast = parse_expr(Lexer::new(
			"Point .{ .x = 1, .y = 2 }".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");
		let (ty, items) = expect_typed_initializer_list(ast.as_ref());
		match ty {
			crate::frontend::Type::Id(name) => assert_eq!(name, "Point"),
			_ => panic!("expected Point type"),
		}
		assert_eq!(items.len(), 2);
	}

	#[test]
	fn typed_initializer_list_allows_dotted_type() {
		let ast = parse_expr(Lexer::new(
			"core.math.Point .{ .x = 1 }".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");
		let (ty, items) = expect_typed_initializer_list(ast.as_ref());
		match ty {
			crate::frontend::Type::Id(name) => assert_eq!(name, "core.math.Point"),
			_ => panic!("expected dotted type"),
		}
		assert_eq!(items.len(), 1);
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
				assert_eq!(
					args[0],
					crate::frontend::GenericArg::Name("T".to_string())
				);
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
				ASTValue::Ref { mutable, v } => {
					assert_eq!(*mutable, false);
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
	fn not_is_unary_operator() {
		let lx = Lexer::new("!x".to_string(), "<test>".to_string());
		let ast = parse_one(lx).expect("ok");
		match &ast.v {
			ASTValue::Not(inner) => expect_id(inner.as_ref(), "x"),
			_ => panic!("expected Not"),
		}
	}

	#[test]
	fn leading_dot_identifier_parses() {
		let lx = Lexer::new(".UP".to_string(), "<test>".to_string());
		let ast = parse_one(lx).expect("ok");
		match &ast.v {
			ASTValue::DotId(name) => assert_eq!(name, "UP"),
			_ => panic!("expected DotId"),
		}
	}

	#[test]
	fn empty_index_parse() {
		let lx = Lexer::new("value[]".to_string(), "<test>".to_string());
		let ast = parse_one(lx).expect("ok");
		match &ast.v {
			ASTValue::Index { target, indices } => {
				expect_id(target.as_ref(), "value");
				assert!(indices.is_empty(), "expected empty index list");
			}
			_ => panic!("expected Index"),
		}
	}

	#[test]
	fn block_is_expression() {
		let lx = Lexer::new("{ 1; 2 }".to_string(), "<test>".to_string());
		let ast = parse_one(lx).expect("ok");
		match &ast.v {
			ASTValue::ExprList(items) => assert_eq!(items.len(), 2),
			_ => panic!("expected ExprList"),
		}
	}

	#[test]
	fn nested_blocks_parse() {
		let lx = Lexer::new("{{}}".to_string(), "<test>".to_string());
		let ast = parse_one(lx).expect("ok");
		match &ast.v {
			ASTValue::ExprList(items) => {
				assert_eq!(items.len(), 1);
				match &items[0].v {
					ASTValue::ExprList(inner) => assert!(inner.is_empty()),
					_ => panic!("expected inner ExprList"),
				}
			}
			_ => panic!("expected ExprList"),
		}
	}

	#[test]
	fn block_in_binary_expression() {
		let lx = Lexer::new("1 * { 3 * 4 }".to_string(), "<test>".to_string());
		let ast = parse_expr(lx).expect("ok");
		let (lhs, rhs) = expect_binary(ast.as_ref(), Operator::Mul, false);
		expect_integer(lhs, 1);
		match &rhs.v {
			ASTValue::ExprList(items) => assert_eq!(items.len(), 1),
			_ => panic!("expected ExprList"),
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
			Ok(_) => {
				panic!("parser should reject missing separator between expressions")
			}
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
	fn if_allows_do_body() {
		let source = "if foo do bar else do baz";
		let lx = Lexer::new(source.to_string(), "<test>".to_string());
		let ast = parse_expr(lx).expect("ok");
		let (_decl, cond, body, else_) = expect_if(ast.as_ref());

		expect_id(cond, "foo");
		expect_id(body, "bar");
		let else_body = else_.expect("else body should exist");
		expect_id(else_body, "baz");
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
			ASTValue::Ref { mutable, v } => {
				assert!(!*mutable, "binding should not be mutable");
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
		let mut lx =
			Lexer::new("for 123 in iter { foo }".to_string(), "<test>".to_string());
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
	fn for_loop_without_header_is_infinite() {
		let source = "for { foo }";
		let lx = Lexer::new(source.to_string(), "<test>".to_string());
		let ast = parse_expr(lx).expect("ok");
		let (init, cond, step, body) = expect_for_loop(ast.as_ref());

		assert!(init.is_none(), "expected no init clause");
		assert!(cond.is_none(), "expected no condition clause");
		assert!(step.is_none(), "expected no step clause");
		expect_expr_list_ids(body, &["foo"]);
	}

	#[test]
	fn for_loop_with_condition_parses() {
		let source = "for foo { bar }";
		let lx = Lexer::new(source.to_string(), "<test>".to_string());
		let ast = parse_expr(lx).expect("ok");
		let (init, cond, step, body) = expect_for_loop(ast.as_ref());

		assert!(init.is_none(), "expected no init clause");
		assert!(step.is_none(), "expected no step clause");
		expect_id(cond.expect("expected condition"), "foo");
		expect_expr_list_ids(body, &["bar"]);
	}

	#[test]
	fn for_loop_allows_do_body() {
		let source = "for foo do bar";
		let lx = Lexer::new(source.to_string(), "<test>".to_string());
		let ast = parse_expr(lx).expect("ok");
		let (init, cond, step, body) = expect_for_loop(ast.as_ref());

		assert!(init.is_none(), "expected no init clause");
		assert!(step.is_none(), "expected no step clause");
		expect_id(cond.expect("expected condition"), "foo");
		expect_id(body, "bar");
	}

	#[test]
	fn for_loop_c_style_full_header_parses() {
		let source = "for i = 0; i < 10; i += 1 { i }";
		let lx = Lexer::new(source.to_string(), "<test>".to_string());
		let ast = parse_expr(lx).expect("ok");
		let (init, cond, step, body) = expect_for_loop(ast.as_ref());

		let init_ast = init.expect("expected init clause");
		let (lhs, rhs) = expect_binary(init_ast, Operator::Set, false);
		expect_id(lhs, "i");
		match rhs.v {
			ASTValue::Integer(n) => assert_eq!(n, 0),
			_ => panic!("expected integer literal"),
		}

		let cond_ast = cond.expect("expected condition clause");
		let (lhs, rhs) = expect_binary(cond_ast, Operator::LessThan, false);
		expect_id(lhs, "i");
		match rhs.v {
			ASTValue::Integer(n) => assert_eq!(n, 10),
			_ => panic!("expected integer literal"),
		}

		let step_ast = step.expect("expected step clause");
		let (lhs, rhs) = expect_binary(step_ast, Operator::Add, true);
		expect_id(lhs, "i");
		match rhs.v {
			ASTValue::Integer(n) => assert_eq!(n, 1),
			_ => panic!("expected integer literal"),
		}

		expect_expr_list_ids(body, &["i"]);
	}

	#[test]
	fn for_loop_c_style_allows_missing_clauses() {
		let source = "for ; foo ; { }";
		let lx = Lexer::new(source.to_string(), "<test>".to_string());
		let ast = parse_expr(lx).expect("ok");
		let (init, cond, step, body) = expect_for_loop(ast.as_ref());

		assert!(init.is_none(), "expected no init clause");
		expect_id(cond.expect("expected condition clause"), "foo");
		assert!(step.is_none(), "expected no step clause");
		expect_expr_list_ids(body, &[]);
	}

	#[test]
	fn for_loop_allows_initializer_and_condition_without_step() {
		let source = "for i = 0; i < 10 { }";
		let lx = Lexer::new(source.to_string(), "<test>".to_string());
		let ast = parse_expr(lx).expect("ok");
		let (init, cond, step, body) = expect_for_loop(ast.as_ref());

		assert!(step.is_none(), "expected no step clause");
		assert!(init.is_some(), "expected init clause");
		assert!(cond.is_some(), "expected condition clause");
		expect_expr_list_ids(body, &[]);
	}

	#[test]
	fn for_requires_in_keyword() {
		let mut lx = Lexer::new("for foo, bar { foo }".to_string(), "<test>".to_string());
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
				pre,
				post,
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
				assert!(pre.is_empty());
				assert!(post.is_none());
				assert!(where_clause.is_none());
				assert!(ensures.is_empty());
				assert_eq!(
					return_type.as_ref().map(|t| t.as_ref()),
					Some(&crate::frontend::Type::Id("int".to_string()))
				);
				match body {
					FnBody::Expr(expr) => {
						let (lhs, rhs) = expect_binary(
							expr.as_ref(),
							Operator::Add,
							false,
						);
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
						constraint: Some(Box::new(
							crate::frontend::Type::Id(
								"type".to_string()
							)
						)),
					}
				);
				assert_eq!(
					generics[2],
					GenericParam::Value {
						names: vec!["N".to_string()],
						ty: Box::new(crate::frontend::Type::Id(
							"usize".to_string()
						)),
					}
				);
			}
			_ => panic!("expected Fn"),
		}
	}

	#[test]
	fn fn_generic_params_are_pretty_printed() {
		let lx = Lexer::new(
			"fn<T, Y: type; N: usize>() do N".to_string(),
			"<test>".to_string(),
		);
		let ast = parse_one(lx).expect("ok");
		let s = ast.to_string();
		assert!(
			s.contains(
				"(GenericList (Generic \"T\" \"type\") (Generic \"Y\" \"type\") (Generic \"N\" \"usize\"))"
			),
			"expected generics to be printed, got: {s}"
		);
	}

	#[test]
	fn fn_generic_params_allow_commas_between_params() {
		let lx = Lexer::new(
			"fn<T, Y: type; N: usize>() do N".to_string(),
			"<test>".to_string(),
		);
		let ast = parse_one(lx).expect("ok");

		match &ast.v {
			ASTValue::Fn { generics, .. } => {
				assert_eq!(generics.len(), 3);
				assert_eq!(
					generics[0],
					GenericParam::Type {
						names: vec!["T".to_string()],
						constraint: Some(Box::new(
							crate::frontend::Type::Id(
								"type".to_string()
							)
						)),
					}
				);
				assert_eq!(
					generics[1],
					GenericParam::Type {
						names: vec!["Y".to_string()],
						constraint: Some(Box::new(
							crate::frontend::Type::Id(
								"type".to_string()
							)
						)),
					}
				);
				assert_eq!(
					generics[2],
					GenericParam::Value {
						names: vec!["N".to_string()],
						ty: Box::new(crate::frontend::Type::Id(
							"usize".to_string()
						)),
					}
				);
			}
			_ => panic!("expected Fn"),
		}
	}

	#[test]
	fn fn_params_are_pretty_printed() {
		let lx = Lexer::new(
			"fn(a: int; ctx: Context; b = 1) do a".to_string(),
			"<test>".to_string(),
		);
		let ast = parse_one(lx).expect("ok");
		let s = ast.to_string();
		assert!(
			s.contains("(ParamList"),
			"expected ParamList to be printed, got: {s}"
		);
		assert!(
			s.contains("(Param \"a\" \"int\")"),
			"expected a typed param, got: {s}"
		);
		assert!(
			s.contains("(Param \"ctx\" \"Context\")"),
			"expected a typed param, got: {s}"
		);
		assert!(
			s.contains("(Param \"b\" \"infer\" (Default 1))"),
			"expected a defaulted param, got: {s}"
		);
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
						constraint: Some(Box::new(
							crate::frontend::Type::Id(
								"type".to_string()
							)
						)),
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
	fn operator_declaration_parses() {
		let ast = parse_expr(Lexer::new(
			"operator+ :: fn(a, b: int) -> int do a + b".to_string(),
			"<test>".to_string(),
		))
		.expect("ok");
		let (_constexpr, names, _types, _values) = expect_declaration_multi(ast.as_ref());
		assert_eq!(names, &["operator+".to_string()]);
	}

	#[test]
	fn enum_parses_variants_with_optional_values() {
		let lx = Lexer::new(
			"enum { Up; Down = 2; Left }".to_string(),
			"<test>".to_string(),
		);
		let ast = parse_one(lx).expect("ok");

		match &ast.v {
			ASTValue::Enum { variants } => {
				assert_eq!(variants.len(), 3);
				assert_eq!(variants[0].name, "Up");
				assert!(variants[0].value.is_none());
				assert_eq!(variants[1].name, "Down");
				match variants[1].value.as_deref() {
					Some(AST {
						v: ASTValue::Integer(value),
						..
					}) => assert_eq!(*value, 2),
					_ => panic!("expected enum variant value"),
				}
				assert_eq!(variants[2].name, "Left");
				assert!(variants[2].value.is_none());
			}
			_ => panic!("expected Enum"),
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
						constraint: Some(Box::new(
							crate::frontend::Type::Id(
								"type".to_string()
							)
						)),
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
	fn array_size_must_be_idish() {
		let err = parse_expr(Lexer::new(
			"x: [Direction^]int".to_string(),
			"<test>".to_string(),
		))
		.err()
		.expect("expected error");
		match err {
			ParseError::InvalidArraySize(_) => {}
			_ => panic!("expected InvalidArraySize"),
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
	fn fn_contract_pre_parses_expression_list() {
		let lx = Lexer::new("fn() pre 1, 2; 3 do 0".to_string(), "<test>".to_string());
		let ast = parse_one(lx).expect("ok");

		match &ast.v {
			ASTValue::Fn { pre, body, .. } => {
				assert_eq!(pre.len(), 3);
				match body {
					FnBody::Expr(expr) => expect_integer(expr.as_ref(), 0),
					_ => panic!("expected do-body expression"),
				}
			}
			_ => panic!("expected Fn"),
		}
	}

	#[test]
	fn return_statement_parses_with_and_without_value() {
		let lx = Lexer::new(
			"fn() { return 1; return; }".to_string(),
			"<test>".to_string(),
		);
		let ast = parse_one(lx).expect("ok");

		match &ast.v {
			ASTValue::Fn { body, .. } => match body {
				FnBody::Block(block) => match &block.v {
					ASTValue::ExprList(items) => {
						assert_eq!(items.len(), 2);
						match &items[0].v {
							ASTValue::Return(Some(v)) => {
								expect_integer(v.as_ref(), 1)
							}
							_ => panic!("expected Return(Some(_))"),
						}
						match &items[1].v {
							ASTValue::Return(None) => {}
							_ => panic!("expected Return(None)"),
						}
					}
					_ => panic!("expected block ExprList"),
				},
				_ => panic!("expected braced body"),
			},
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
	fn fn_do_semicolon_requires_expression() {
		let mut lx = Lexer::new("fn() do;".to_string(), "<test>".to_string());
		let mut parser = Parser::new(&mut lx).expect("parser init");
		let err = match parser.parse() {
			Ok(_) => panic!("missing do-body expression should not parse"),
			Err(err) => err,
		};
		match err {
			ParseError::ExpectedExpression(tok) => match tok.v {
				TokenValue::Semicolon => {}
				other => panic!("expected ';' token, got {:?}", other),
			},
			other => panic!("expected ExpectedExpression, got {:?}", other),
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
		let exprs =
			parse_all(Lexer::new(src.to_string(), "<test>".to_string())).expect("ok");
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
