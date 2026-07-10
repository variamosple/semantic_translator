"""
Location tracking for parser.
Used to track source positions during parsing.
"""

from dataclasses import dataclass


@dataclass(frozen=True)
class Location:
    line: int
    column: int

    def __str__(self) -> str:
        return f"line {self.line}, column {self.column}"
