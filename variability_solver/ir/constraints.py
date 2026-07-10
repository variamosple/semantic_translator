from __future__ import annotations

from pydantic import BaseModel


# ======================================================================
# Base classes
# ======================================================================

class Node(BaseModel, frozen=True):
    pass


class Formula(Node, frozen=True):
    pass


class Term(Node, frozen=True):
    pass


# ======================================================================
# Formulas
# ======================================================================

# ----------------------------------------------------------------------
# Atomic Formulas
# ----------------------------------------------------------------------

class AtomicFormula(Formula, frozen=True):
    pass


# Comparisons

class Comparison(AtomicFormula, frozen=True):
    left: Term
    right: Term


class Equality(Comparison, frozen=True):
    pass


class LessThan(Comparison, frozen=True):
    pass


class LessEqual(Comparison, frozen=True):
    pass


# ----------------------------------------------------------------------
# Composite Formulas
# ----------------------------------------------------------------------


class CompositeFormula(Formula, frozen=True):
    pass


class Negation(CompositeFormula, frozen=True):
    operand: Formula


class Conjunction(CompositeFormula, frozen=True):
    operands: tuple[Formula, ...]


class Disjunction(CompositeFormula, frozen=True):
    operands: tuple[Formula, ...]


class Implication(CompositeFormula, frozen=True):
    left: Formula
    right: Formula


class Biconditional(CompositeFormula, frozen=True):
    left: Formula
    right: Formula
    

# ======================================================================
# Terms
# ======================================================================

class VariableRef(Term, frozen=True):
    var_id: int


# ---------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------

class Constant(Term, frozen=True):
    """Represents a constant value in the constraint (ex: 5, True)."""
    value: object


# Boolean constants are also atomic formulas
class BooleanConstant(AtomicFormula, Constant, frozen=True):
    value: bool


TRUE = BooleanConstant(value=True)
FALSE = BooleanConstant(value=False)


class IntConstant(Constant, frozen=True):
    value: int


# ---------------------------------------------------------------------
# Arithmetic Operations
# ---------------------------------------------------------------------


class Addition(Term, frozen=True):
    operands: tuple[Term, ...]


class Subtraction(Term, frozen=True):
    left: Term
    right: Term


class Multiplication(Term, frozen=True):
    operands: tuple[Term, ...]


class Division(Term, frozen=True):
    left: Term
    right: Term
