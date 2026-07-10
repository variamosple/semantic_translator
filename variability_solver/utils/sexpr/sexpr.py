"""
S-expression Abstract Syntax Tree (AST) for CLIF-like syntax.

This module defines the data structures that represent parsed S-expressions,
including symbols, strings, and nested lists. These types are used by the
parser to build an abstract syntax tree from raw text input.
"""

from dataclasses import dataclass

from variability_solver.utils.location import Location


@dataclass(frozen=True)
class SExpr:
    """Base class for all S-expression nodes."""
    location: Location


@dataclass(frozen=True)
class Symbol(SExpr):
    """Represents a symbolic identifier in an S-expression."""
    value: str


@dataclass(frozen=True)
class List(SExpr):
    """Represents a nested list structure in an S-expression."""
    items: list[SExpr]
