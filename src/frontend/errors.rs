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

pub enum FrontendError {
    ParseError(ParseError),
    InvalidEnumeratedArrayEnum(SourceLocation),
    InitializerListHasDuplicateFields {
        first_found_definition: SourceLocation,
        conflicting_definition: SourceLocation,
    },
}
