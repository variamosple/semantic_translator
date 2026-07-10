"""
Token definitions for S-expression parser.

This module defines the token types and their corresponding data structures
used by the lexer to break input text into meaningful units.
"""

from dataclasses import dataclass
from enum import Enum, auto

from ..location import Location


class TokenType(Enum):
    """Enumeration of token types used by the S-expression parser."""

    LPAREN = auto()
    RPAREN = auto()
    SYMBOL = auto()
    EOF = auto()


@dataclass(frozen=True)
class Token:
    """Represents a token produced by the lexer."""

    type: TokenType
    value: str
    location: Location
