from .lexer import tokenize
from .sexpr import List, SExpr, Symbol
from .tokens import Token, TokenType


class SExprParseError(Exception):
    """Exception raised when parsing an S-expression fails."""


class SExprParser:
    """Parser for S-expressions."""

    def __init__(self, tokens: list[Token]):
        self.tokens = tokens
        self.pos = 0

    def parse(self) -> list[SExpr]:
        """Parse a list of S-expressions from the token stream."""
        sexpr_list = []
        while self._current.type != TokenType.EOF:
            sexpr_list.append(self._parse_expr())
        self._expect(TokenType.EOF)
        return sexpr_list

    @property
    def _current(self) -> Token:
        """Return the current token."""
        if self.pos >= len(self.tokens):
            raise SExprParseError("Input stream exhausted before EOF")
        return self.tokens[self.pos]

    def _advance(self) -> None:
        """Advance to the next token."""
        token = self._current
        if token.type != TokenType.EOF:
            self.pos += 1
        else:
            raise SExprParseError("Unexpected end of input (EOF)")

    def _expect(self, typ: TokenType) -> None:
        """Expect the current token to be of the given type."""
        token = self._current
        if token.type != typ:
            raise SExprParseError(f"Expected {typ.name}, got {token.type.name} at {token.location}")

    def _parse_expr(self) -> SExpr:
        """Parse a single S-expression."""
        token = self._current

        match token.type:
            case TokenType.SYMBOL:
                self._advance()
                return Symbol(
                    location=token.location,
                    value=token.value,
                )

            case TokenType.LPAREN:
                return self._parse_list()

            case _:
                raise SExprParseError(f"Unexpected token {token.type.name} at {token.location}")

    def _parse_list(self) -> List:
        """Parse a list S-expression."""
        start = self._current.location
        self._expect(TokenType.LPAREN)
        self._advance()

        items = []
        while self._current.type not in (TokenType.RPAREN, TokenType.EOF):
            items.append(self._parse_expr())

        self._expect(TokenType.RPAREN)
        self._advance()

        return List(
            location=start,
            items=items,
        )


def parse_sexpr(sexpr_str: str) -> list[SExpr]:
    """Parse a list of S-expressions from a string."""
    tokens = tokenize(sexpr_str)
    parser = SExprParser(tokens)
    return parser.parse()
