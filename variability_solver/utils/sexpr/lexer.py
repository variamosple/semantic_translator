"""
Lexer for S-expression parser.

This module provides the Lexer class that converts raw text input into a stream
of tokens (symbols, strings, parentheses, etc.) for the parser to consume.
"""

from variability_solver.utils.errors import LexicalError
from variability_solver.utils.location import Location

from .tokens import Token, TokenType


class Lexer:
    def __init__(self, text: str):
        self.text: str = text
        self.pos: int = 0
        self.line: int = 1
        self.column: int = 1

    def _current(self) -> str | None:
        """Return the current character or None if end of input."""
        if self.pos >= len(self.text):
            return None
        return self.text[self.pos]

    def _advance(self) -> None:
        """Move to the next character, updating line/column tracking."""
        if self._current() is None:
            return

        if self._current() == "\n":
            self.line += 1
            self.column = 1
        else:
            self.column += 1

        self.pos += 1

    def _skip_whitespace(self) -> bool:
        """Skip over whitespace characters."""
        if (ch := self._current()) is not None and ch.isspace():
            self._advance()
            return True
        return False

    def _skip_comment(self) -> bool:
        """Skip a CLIF line comment beginning with ';'."""
        if (ch := self._current()) is not None and ch == ";":
            while (ch := self._current()) is not None and ch != "\n":
                self._advance()
            if ch == "\n":
                self._advance()
            return True
        return False

    def _skip_layout(self) -> bool:
        """Skip over non-significant characters."""
        return self._skip_whitespace() or self._skip_comment()

    def _read_symbol(self) -> Token:
        """Read a symbol token"""
        location = Location(self.line, self.column)

        chars = []
        while (ch := self._current()) is not None and not ch.isspace() and ch not in "()":
            chars.append(ch)
            self._advance()

        return Token(TokenType.SYMBOL, "".join(chars), location)

    def _read_string(self) -> Token:
        """Read a string token, which is a sequence of characters enclosed in double quotes."""
        location = Location(self.line, self.column)

        self._advance()  # consume opening quote

        chars = []
        while (ch := self._current()) is not None and ch != '"':
            chars.append(ch)
            self._advance()

        if self._current() is None:
            raise LexicalError(
                f"Unterminated string at line {location.line}, column {location.column}"
            )

        self._advance()  # consume closing quote

        # Strings are treated as symbols
        return Token(TokenType.SYMBOL, '"' + "".join(chars) + '"', location)

    def _next_token(self) -> Token:
        """Return the next token from the input stream."""
        if self._skip_layout():
            return self._next_token()

        match self._current():
            case None:
                location = Location(self.line, self.column)
                return Token(TokenType.EOF, "", location)

            case "(":
                location = Location(self.line, self.column)
                self._advance()
                return Token(TokenType.LPAREN, "(", location)

            case ")":
                location = Location(self.line, self.column)
                self._advance()
                return Token(TokenType.RPAREN, ")", location)

            case '"':
                return self._read_string()

            case _:
                return self._read_symbol()

    def tokenize(self) -> list[Token]:
        """Return a list of tokens from the input text."""
        tokens = []

        token = self._next_token()
        while token.type != TokenType.EOF:
            tokens.append(token)
            token = self._next_token()
        tokens.append(token)

        return tokens


def tokenize(sexpr_str: str) -> list[Token]:
    """Tokenize a string into a list of tokens."""
    lexer = Lexer(sexpr_str)
    return lexer.tokenize()
