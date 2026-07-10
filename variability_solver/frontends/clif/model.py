"""
CLIF Abstract Syntax Tree (AST) - ISO/IEC 24707 Compliant

This module defines the AST for CLIF (Common Logic Interchange Format),
following the ISO/IEC 24707 standard for Common Logic.

The AST represents the syntactic structure of CLIF formulas and terms,
with explicit support for the semantic constructs defined in the standard.
"""

from __future__ import annotations

from pydantic import BaseModel

# ============================================================================
# Base Node
# ============================================================================


class Node(BaseModel, frozen=True):
    """
    Base class for all CLIF AST nodes.

    In ISO Common Logic (CL), all syntactic constructs ultimately belong to the
    category of *well-formed expressions* (WFE). This base node corresponds to
    that universal syntactic foundation.
    """


class Term(Node, frozen=True):
    """
    A term denoting an individual.

    This corresponds to the CL notion of Term (§3.16, §6.1.1.16).

    Key semantic property:
      - A term denotes an element of the universe of discourse (UDI)
      - Terms are either:
          * names (constants)
          * functional terms (operator applied to arguments)

    In CL, unlike classical FOL:
      - no strict separation between function symbols and predicate symbols
      - terms are fully compositional expressions
    """


# ============================================================================
# Sequence Marker
# ============================================================================


class SequenceMarker(Node, frozen=True):
    """
    A sequence marker in a quantified formula.

    Sequence markers correspond to Smark in the CL abstract syntax (§4.1, §6.1.2.1).
    They represent placeholder elements for sequences of arguments in:
      - quantified sentences
      - term sequences

    Unlike names, sequence markers are mapped to finite sequences in interpretations
    (seqI mapping in §6.2), and are NOT part of the universe of discourse.
    """

    name: str


# ============================================================================
# Term Sequence
# ============================================================================


class TermSequence(Node, frozen=True):
    """
    A sequence of terms or sequence markers.

    This corresponds to TermSequence in §6.1.1.18 and §6.1.2.2.

    A term sequence:
      - is a finite ordered collection
      - may include both Terms and SequenceMarkers
      - may be empty
      - is used in functional terms and atomic sentences

    Semantically, term sequences are interpreted as finite sequences in UDI*
    (finite sequences over the universe of discourse).
    """

    elements: list[Term | SequenceMarker]


# ============================================================================
# Terms
# ============================================================================


class Name(Term, frozen=True):
    """
    A named constant term.

    Names correspond to elements of the vocabulary V (§4.1, §6.1.2.1).

    Semantic mapping:
      intI(name) ∈ URI

    In classical interpretations:
      - names may denote individuals, relations, or higher-order entities
      - no syntactic restriction enforces “constant vs predicate” roles
    """

    name: str


class Function(Term, frozen=True):
    """
    A functional term.

    Corresponds to FunctionalTerm (§6.1.1.17, §6.1.2.2).

    Structure:
      operator : Term
      arguments: TermSequence

    Semantics:
      - The operator denotes a function in funI
      - Arguments evaluate to elements of UDI*
      - Result is a single element of URI/UDI depending on interpretation

    This reflects CL's key design choice:
      functions are first-class interpreted objects (via URI → functions mapping).
    """

    operator: Term
    arguments: TermSequence


# ============================================================================
# Formulas
# ============================================================================


class Formula(Node, frozen=True):
    """
    A logical formula.

    Corresponds to Sentence (§3.15, §6.1.1.12).

    In CL:
      - formulas evaluate to truth values under an interpretation
      - they are the primary carrier of semantics (satisfaction relation)
      - they include atomic, boolean, equality, and quantified forms
    """

    pass


class Atom(Formula, frozen=True):
    """
    Atomic predicate application.

    Corresponds to Atomic sentences (§6.1.1.15, §6.1.2.3).

    Structure:
      predicate : Term
      arguments : TermSequence

    Semantics:
      - predicate denotes a relation via relI
      - arguments are evaluated terms
      - truth is determined by membership of argument tuple in relation extension
    """

    predicate: Term
    arguments: TermSequence


class Equality(Formula, frozen=True):
    """
    Equality between two terms.

    Equality is part of simple sentences (§6.1.1.15).

    Semantics:
      - interpreted as identity over the universe of discourse
      - both left and right are evaluated terms
    """

    left: Term
    right: Term


class Not(Formula, frozen=True):
    """
    Logical negation.

    Boolean sentence (Neg) (§6.1.1.14, §6.1.2.3).

    Semantics:
      - truth-functional negation over sentence interpretation
    """

    term: Formula


class And(Formula, frozen=True):
    """
    Logical conjunction.

    Conjunctive Boolean sentence (§6.1.1.14).

    Semantics:
      - true iff all component formulas are true
    """

    terms: list[Formula]


class Or(Formula, frozen=True):
    """
    Logical disjunction.

    Disjunctive Boolean sentence (§6.1.1.14).

    Semantics:
      - true iff at least one component formula is true
    """

    terms: list[Formula]


class Implies(Formula, frozen=True):
    """
    Logical implication.

    Conditional Boolean sentence (§6.1.1.14).

    Semantics:
      - classical material implication:
        antecedent → consequent
    """

    antecedent: Formula
    consequent: Formula


class Iff(Formula, frozen=True):
    """
    Logical equivalence.

    Biconditional Boolean sentence (§6.1.1.14).

    Semantics:
      - true iff both formulas have identical truth values
    """

    left: Formula
    right: Formula


Binding = Name | SequenceMarker


class ForAll(Formula, frozen=True):
    """
    Universal quantification.

    UQuant (§6.1.1.13, §6.1.2.3).

    Semantics:
      - evaluates over all assignments in the universe of discourse (UDI)
      - bindings introduce scope for variables (names or sequence markers)

    Note:
      CL does not enforce a strict variable category distinction.
    """

    bindings: list[Binding]
    body: Formula


class Exists(Formula, frozen=True):
    """
    Existential quantification.

    EQuant (§6.1.1.13, §6.1.2.3).

    Semantics:
      - true iff at least one assignment in UDI satisfies the body formula
    """

    bindings: list[Binding]
    body: Formula


# ============================================================================
# Theory
# ============================================================================


class Theory(Node, frozen=True):
    formulas: list[Formula]
