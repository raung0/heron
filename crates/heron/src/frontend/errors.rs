use crate::frontend::{CtfeError, LexerError, SourceLocation, Token, TokenValue};

#[derive(Debug, Clone)]
pub struct SourceLocationBlock {
	pub location: SourceLocation,
	pub label: Option<String>,
}

#[derive(Debug, Clone)]
pub enum ParseError {
	LexerError(LexerError),
	InvalidUnaryOperator(Token),
	InvalidBinaryOperator(Token),
	UnexpectedToken(Token, TokenValue),
	ExpectedToken(Token, TokenValue),
	UnclosedOperatorName(Token, TokenValue),
	InvalidOperatorName(Token),
	InvalidFactorToken(Token),
	ExpectedConditionExpression(Token),
	InvalidForBinding(SourceLocation),
	ExpectedBody(SourceLocation),
	ExpectedExpression(Token),
	InvalidDeclarationType(Token),
	InvalidArraySize(SourceLocation),
	InvalidNumericType(Token),
	PostReturnIdAlreadyDefined(Token),
	MixedInitializerListStyles(Token),
	MissingInitializerDot(Token),
	UnsupportedIncrement(Token),
	UnsupportedDecrement(Token),
}

#[derive(Debug)]
pub enum FrontendError {
	ParseError(ParseError),
	InvalidEnumeratedArrayEnum(SourceLocation),
	InitializerListHasDuplicateFields {
		first_found_definition: SourceLocation,
		conflicting_definition: SourceLocation,
	},
	DuplicateValueDeclaration {
		name: String,
		first_found_definition: SourceLocation,
		conflicting_definition: SourceLocation,
	},
	DuplicateFieldDeclaration {
		name: String,
		first_found_definition: SourceLocation,
		conflicting_definition: SourceLocation,
	},
	HideOutsideScope {
		location: SourceLocation,
		name: String,
	},
	InvalidStructMember {
		location: SourceLocation,
	},
	InvalidInterfaceMember {
		location: SourceLocation,
	},
	InterfaceFunctionMustBeUninitialized {
		location: SourceLocation,
	},
	InlineStructTypeNotAllowed {
		location: SourceLocation,
	},
	GenericOperatorConstraint {
		location: SourceLocation,
		call_location: SourceLocation,
		operator: String,
		lhs: String,
		rhs: String,
	},
	GenericMemberConstraint {
		location: SourceLocation,
		call_location: SourceLocation,
		member: String,
		lhs: String,
	},
	StructOrUnionNotInComptimeDeclaration(SourceLocation),
	InvalidDeclarationArity(SourceLocation),
	MissingPackage(SourceLocation),
	PackageMismatch {
		location: SourceLocation,
		expected: String,
		found: String,
	},
	ModuleNotFound {
		location: SourceLocation,
		module: String,
	},
	DuplicateModuleAlias {
		location: SourceLocation,
		alias: String,
	},
	DuplicateModuleImport {
		location: SourceLocation,
		module: String,
	},
	DuplicateExport {
		location: SourceLocation,
		name: String,
	},
	UnknownType {
		location: SourceLocation,
		name: String,
		hint: Option<String>,
	},
	UnknownValue {
		location: SourceLocation,
		name: String,
	},
	TypeMismatch {
		location: SourceLocation,
		expected: String,
		found: String,
	},
	InvalidOperator {
		location: SourceLocation,
		operator: String,
		lhs: String,
		rhs: Option<String>,
	},
	InvalidCall {
		location: SourceLocation,
		callee: String,
	},
	PositionalAfterNamedArgument {
		location: SourceLocation,
		callee: String,
	},
	UnknownNamedArgument {
		location: SourceLocation,
		callee: String,
		name: String,
	},
	DuplicateNamedArgument {
		location: SourceLocation,
		callee: String,
		name: String,
	},
	DuplicateArgument {
		location: SourceLocation,
		callee: String,
		name: String,
	},
	MissingNamedArgument {
		location: SourceLocation,
		callee: String,
		name: String,
	},
	DefaultParamOrder {
		location: SourceLocation,
		name: String,
	},
	AssignToImmutable {
		location: SourceLocation,
		name: String,
	},
	AssignWhileBorrowed {
		location: SourceLocation,
		name: String,
		borrowed_at: Option<SourceLocation>,
	},
	UseAfterMove {
		location: SourceLocation,
		name: String,
		moved_at: Option<SourceLocation>,
	},
	MoveWhileBorrowed {
		location: SourceLocation,
		name: String,
		borrowed_at: Option<SourceLocation>,
	},
	AccessWhileMutBorrowed {
		location: SourceLocation,
		name: String,
		borrowed_at: Option<SourceLocation>,
	},
	BorrowSharedWhileMut {
		location: SourceLocation,
		name: String,
		borrowed_at: Option<SourceLocation>,
	},
	BorrowMutWhileShared {
		location: SourceLocation,
		name: String,
		borrowed_at: Option<SourceLocation>,
	},
	MutBorrowOfImmutable {
		location: SourceLocation,
		name: String,
	},
	NonStaticModuleMut {
		location: SourceLocation,
		name: String,
	},
	PointerInComptime {
		location: SourceLocation,
	},
	PointerRequiresUnsafe {
		location: SourceLocation,
	},
	MissingOperatorSelf {
		location: SourceLocation,
		operator: String,
		hint: String,
	},
	MissingField {
		location: SourceLocation,
		type_name: String,
		field: String,
	},
	CyclicTypeDefinition {
		location: SourceLocation,
		type_name: String,
		cycle: String,
		cycle_locations: Vec<(SourceLocation, String)>,
	},
	InvalidIndex {
		location: SourceLocation,
		target: String,
		index: String,
	},
	NonBoolCondition {
		location: SourceLocation,
		found: String,
	},
	UntypedLiteralNeedsContext {
		location: SourceLocation,
		kind: String,
	},
	DuplicateUnionVariantType {
		location: SourceLocation,
		ty: String,
	},
	UnusedValue {
		location: SourceLocation,
		hint: String,
	},
	InaccessibleMember {
		location: SourceLocation,
		type_name: String,
		member: String,
	},
	RuntimeCallInComptime {
		location: SourceLocation,
		callee: String,
	},
	CtfeError(CtfeError),
}

#[derive(Debug, Clone)]
pub enum FrontendWarning {
	ModuleConflict {
		location: SourceLocation,
		module: String,
		chosen_path: String,
		ignored_paths: Vec<String>,
	},
}

impl FrontendError {
	pub fn get_location(&self) -> Option<&SourceLocation> {
		match self {
			FrontendError::ParseError(_) => None,
			FrontendError::InvalidEnumeratedArrayEnum(location)
			| FrontendError::StructOrUnionNotInComptimeDeclaration(location)
			| FrontendError::InvalidDeclarationArity(location)
			| FrontendError::MissingPackage(location) => Some(location),
			FrontendError::InitializerListHasDuplicateFields {
				conflicting_definition,
				..
			}
			| FrontendError::DuplicateValueDeclaration {
				conflicting_definition,
				..
			}
			| FrontendError::DuplicateFieldDeclaration {
				conflicting_definition,
				..
			} => Some(conflicting_definition),
			FrontendError::HideOutsideScope { location, .. }
			| FrontendError::InvalidStructMember { location }
			| FrontendError::InvalidInterfaceMember { location }
			| FrontendError::InterfaceFunctionMustBeUninitialized { location }
			| FrontendError::InlineStructTypeNotAllowed { location }
			| FrontendError::GenericOperatorConstraint { location, .. }
			| FrontendError::GenericMemberConstraint { location, .. }
			| FrontendError::PackageMismatch { location, .. }
			| FrontendError::ModuleNotFound { location, .. }
			| FrontendError::DuplicateModuleAlias { location, .. }
			| FrontendError::DuplicateModuleImport { location, .. }
			| FrontendError::DuplicateExport { location, .. }
			| FrontendError::UnknownType { location, .. }
			| FrontendError::UnknownValue { location, .. }
			| FrontendError::TypeMismatch { location, .. }
			| FrontendError::InvalidOperator { location, .. }
			| FrontendError::InvalidCall { location, .. }
			| FrontendError::PositionalAfterNamedArgument { location, .. }
			| FrontendError::UnknownNamedArgument { location, .. }
			| FrontendError::DuplicateNamedArgument { location, .. }
			| FrontendError::DuplicateArgument { location, .. }
			| FrontendError::MissingNamedArgument { location, .. }
			| FrontendError::DefaultParamOrder { location, .. }
			| FrontendError::AssignToImmutable { location, .. }
			| FrontendError::AssignWhileBorrowed { location, .. }
			| FrontendError::UseAfterMove { location, .. }
			| FrontendError::MoveWhileBorrowed { location, .. }
			| FrontendError::AccessWhileMutBorrowed { location, .. }
			| FrontendError::BorrowSharedWhileMut { location, .. }
			| FrontendError::BorrowMutWhileShared { location, .. }
			| FrontendError::MutBorrowOfImmutable { location, .. }
			| FrontendError::NonStaticModuleMut { location, .. }
			| FrontendError::PointerInComptime { location }
			| FrontendError::PointerRequiresUnsafe { location }
			| FrontendError::MissingOperatorSelf { location, .. }
			| FrontendError::MissingField { location, .. }
			| FrontendError::CyclicTypeDefinition { location, .. }
			| FrontendError::InvalidIndex { location, .. }
			| FrontendError::NonBoolCondition { location, .. }
			| FrontendError::UntypedLiteralNeedsContext { location, .. }
			| FrontendError::DuplicateUnionVariantType { location, .. }
			| FrontendError::UnusedValue { location, .. }
			| FrontendError::InaccessibleMember { location, .. }
			| FrontendError::RuntimeCallInComptime { location, .. }
			| FrontendError::CtfeError(CtfeError { location, .. }) => Some(location),
		}
	}
}
