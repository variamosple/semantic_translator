class ParseError(Exception):
    """Raised when parser input is invalid"""


class LexicalError(ParseError):
    """Raised when lexical analysis fails."""


class SyntacticError(ParseError):
    """Raised when syntax is invalid."""


class SemanticError(ParseError):
    """Raised when semantics are invalid."""
