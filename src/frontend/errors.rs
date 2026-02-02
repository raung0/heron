use crate::frontend::{LexerError, SourceLocation, Token, TokenValue};

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
