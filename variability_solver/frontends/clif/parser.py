"""
Parser from S-expression AST to CLIF AST.

This parser transforms a parsed S-expression tree into the strongly-typed
CLIF abstract syntax tree.

The parser performs structural validation and raises ParseError whenever
the input is not a well-formed CLIF expression.
"""

from __future__ import annotations

from variability_solver.utils.errors import SemanticError, SyntacticError
from variability_solver.utils.sexpr.parser import parse_sexpr
from variability_solver.utils.sexpr.sexpr import List, SExpr, Symbol

from . import model as clif


def parse_clif(clif_str: str) -> clif.Theory:
    """Parse a CLIF theory from a string."""
    sexpr_list = parse_sexpr(clif_str)
    return Parser().parse(sexpr_list)


class Parser:
    """Recursive-descent parser from S-expression AST to CLIF AST."""

    def parse(self, sexpr_list: list[SExpr]) -> clif.Theory:
        """Parse a top-level CLIF theory."""
        return self._parse_theory(sexpr_list)

    # -------------------------------------------------------------------------
    # Theory parsing
    # -------------------------------------------------------------------------

    def _parse_theory(self, sexpr_list: list[SExpr]) -> clif.Theory:
        """Parse a CLIF theory from a list of S-expressions."""
        return clif.Theory(formulas=[self._parse_formula(item) for item in sexpr_list])

    def _parse_formula(self, expr: SExpr) -> clif.Formula:
        """Parse a CLIF formula from an S-expression."""
        match expr:
            # Equality
            case List(
                location=_,
                items=[Symbol(value="="), left, right],
            ):
                return clif.Equality(
                    left=self._parse_term(left),
                    right=self._parse_term(right),
                )

            # Equality with wrong number of arguments
            case List(
                location=loc,
                items=[Symbol(value="="), *args],
            ):
                raise SyntacticError(
                    f"Equality must have exactly 2 arguments,got {len(args)} at {loc}"
                )

            # Negation
            case List(
                location=_,
                items=[Symbol(value="not"), body],
            ):
                return clif.Not(
                    term=self._parse_formula(body),
                )

            # Negation with wrong number of arguments
            case List(
                location=_,
                items=[Symbol(value="not"), *args],
            ):
                raise SyntacticError(f"Negation must have exactly 1 argument, got {len(args)}")

            # Conjunction
            case List(
                location=_,
                items=[Symbol(value="and"), *terms],
            ):
                return clif.And(
                    terms=[self._parse_formula(term) for term in terms],
                )

            # Disjunction
            case List(
                location=_,
                items=[Symbol(value="or"), *terms],
            ):
                return clif.Or(
                    terms=[self._parse_formula(term) for term in terms],
                )

            # Implication
            case List(
                location=_,
                items=[Symbol(value="if"), antecedent, consequent],
            ):
                return clif.Implies(
                    antecedent=self._parse_formula(antecedent),
                    consequent=self._parse_formula(consequent),
                )

            # Implication with wrong number of arguments
            case List(
                location=_,
                items=[Symbol(value="if"), *args],
            ):
                raise SyntacticError(
                    f"Implication must have exactly 2 arguments, got {len(args)} at {expr.location}"
                )

            # Biconditional
            case List(
                location=_,
                items=[Symbol(value="iff"), left, right],
            ):
                return clif.Iff(
                    left=self._parse_formula(left),
                    right=self._parse_formula(right),
                )

            # Biconditional with wrong number of arguments
            case List(
                location=_,
                items=[Symbol(value="iff"), *args],
            ):
                raise SyntacticError(
                    f"Biconditional must have exactly 2 arguments, "
                    f"got {len(args)} at {expr.location}"
                )

            # Universal quantification
            case List(
                location=_,
                items=[Symbol(value="forall"), List(items=bindings), body],
            ):
                return clif.ForAll(
                    bindings=[self._parse_binding(binding) for binding in bindings],
                    body=self._parse_formula(body),
                )

            # Universal quantification with wrong number of arguments
            case List(
                location=_,
                items=[Symbol(value="forall"), *args],
            ):
                raise SyntacticError(
                    f"Universal quantification must have exactly 2 arguments, "
                    f"got {len(args)} at {expr.location}"
                )

            # Existential quantification
            case List(
                location=_,
                items=[Symbol(value="exists"), List(items=bindings), body],
            ):
                return clif.Exists(
                    bindings=[self._parse_binding(binding) for binding in bindings],
                    body=self._parse_formula(body),
                )

            # Existential quantification with wrong number of arguments
            case List(
                location=_,
                items=[Symbol(value="exists"), *args],
            ):
                raise SyntacticError(
                    f"Existential quantification must have exactly 2 arguments, "
                    f"got {len(args)} at {expr.location}"
                )

            # Atomic sentence
            case List(
                location=_,
                items=[predicate, *arguments],
            ):
                return clif.Atom(
                    predicate=self._parse_term(predicate),
                    arguments=self._parse_term_sequence(arguments),
                )

            # Atomic (standalone)
            case Symbol():
                return clif.Atom(
                    predicate=clif.Name(name=expr.value),
                    arguments=clif.TermSequence(elements=[]),
                )

            case _:
                raise SyntacticError(f"Expected formula at {expr.location}")

    # -------------------------------------------------------------------------
    # Term parsing
    # -------------------------------------------------------------------------

    def _parse_term(self, expr: SExpr) -> clif.Term:
        """Parse a CLIF term from an S-expression."""
        match expr:
            # Name
            case Symbol():
                term = self._parse_term_sequence_element(expr)

                if isinstance(term, clif.SequenceMarker):
                    raise SemanticError(
                        f"Sequence marker '@{term.name}' "
                        f"cannot appear as a standalone term "
                        f"at {expr.location}"
                    )

                return term

            # Functional term
            case List(location=loc, items=[]):
                raise SyntacticError(
                    "Empty list cannot be used as a term. "
                    "Expected a valid term expression (e.g., '(predicate arg1 arg2)'). "
                    f"at {loc}"
                )

            case List(
                location=_,
                items=[operator, *arguments],
            ):
                return clif.Function(
                    operator=self._parse_term(operator),
                    arguments=self._parse_term_sequence(arguments),
                )

            case _:
                raise SyntacticError(f"Expected term at {expr.location}")

    # -------------------------------------------------------------------------
    # Term sequences parsing
    # -------------------------------------------------------------------------

    def _parse_term_sequence(self, exprs: list[SExpr]) -> clif.TermSequence:
        """Parse a sequence of terms from S-expressions."""
        return clif.TermSequence(
            elements=[self._parse_term_sequence_element(expr) for expr in exprs],
        )

    def _parse_term_sequence_element(self, expr: SExpr) -> clif.Term | clif.SequenceMarker:
        """Parse a single element of a term sequence."""
        match expr:
            case Symbol(location=loc, value=name):
                if name.startswith("@"):
                    if len(name) == 1:
                        raise SyntacticError(f"Empty sequence marker at {loc}")

                    return clif.SequenceMarker(
                        name=name[1:],
                    )

                return clif.Name(name=name)

            case _:
                return self._parse_term(expr)

    # -------------------------------------------------------------------------
    # Bindings parsing
    # -------------------------------------------------------------------------

    def _parse_binding(self, expr: SExpr) -> clif.Binding:
        """Parse a CLIF binding from an S-expression."""
        match expr:
            case Symbol(location=loc, value=name):
                if name.startswith("@"):
                    if len(name) == 1:
                        raise SyntacticError(f"Empty sequence marker at {loc}")

                    return clif.SequenceMarker(
                        name=name[1:],
                    )

                return clif.Name(name=name)

            case _:
                raise SyntacticError(
                    f"Expected name or sequence marker as binding at {expr.location}"
                )
