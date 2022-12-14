from __future__ import annotations
from abc import ABC, abstractmethod
from typing import Optional
from utils.exceptions import SemanticException
from grammars import clif
from enum import Enum, unique
from targets.solver_model import SolverModel


class StrEnum(str, Enum):
    pass


@unique
class ArithmeticPredicate(StrEnum):
    LT = " < "
    LTE = " <= "
    GT = " > "
    GTE = " >= "
    EQ = " == "
    NEQ = " != "


@unique
class ReificationPredicate(StrEnum):
    IMP = " -> "
    BIMP = " <-> "


class MZNModel(SolverModel):
    __delimiter = ";"

    def __init__(self) -> None:
        self.var_decls: dict[str, MZNVarDecl] = dict()
        self.constraint_decls: list[MZNConstraintDecl] = []

    def add_var_decl(self, var_decl: MZNVarDecl) -> None:
        self.var_decls[var_decl.var] = var_decl

    def add_constraint_decl(self, constraint_decl: MZNConstraintDecl) -> None:
        self.constraint_decls.append(constraint_decl)

    def fix_variable(self, variable: str, value: int):
        try:
            var_decl = self.var_decls[variable]
            var_decl.fix_variable(value)
        except KeyError:
            raise RuntimeError(
                "You are trying to set a variable that does not exist"
            )

    def generate_program(self) -> list[str]:
        strs: list[str] = []
        constraints: list[MZNExpression] = [
            *self.var_decls.values(),
            *self.constraint_decls,
        ]
        for cons in constraints:
            strs.append(cons.to_string() + self.__delimiter)
        return strs


class MZNExpression(ABC):
    pass

    @abstractmethod
    def to_string(self) -> str:
        pass


class MZNVarDecl(MZNExpression):
    def __init__(
        self, var: str, type: str, upper: int = 1, lower: int = 0
    ) -> None:
        super().__init__()
        self.var = var
        self.type = type
        if self.var != "bool":
            self.upper, self.lower = upper, lower
        else:
            self.upper, self.lower = 1, 0

    def fix_variable(self, value):
        if self.lower <= value and value <= self.upper:
            self.lower, self.upper = value, value
        else:
            raise SemanticException("This value violates the variable's bounds")

    def quote_var(self) -> str:
        return "'" + self.var + "'"

    def to_string(self) -> str:
        qvar = self.quote_var()
        if self.type == "bool":
            return f"var 0..1:{qvar}"
        else:
            return f"var int:{qvar}"


class MZNConstraintDecl(MZNExpression):
    def __init__(
        self,
        arithmetic_predicate: Optional[str] = None,
        reification_predicate: Optional[str] = None,
        terms: Optional[list[str | int | clif.ArithmeticExpr]] = None,
        sub_constraints: Optional[list[MZNConstraintDecl]] = None,
        top_level: bool = True,
    ) -> None:
        self.arithmetic_predicate = arithmetic_predicate
        self.reification_predicate = reification_predicate
        self.terms = terms
        self.sub_constraints = sub_constraints
        self.top_level = top_level

    def render_expr(self, term) -> str:
        if isinstance(term, int):
            return str(term)
        elif isinstance(term, str):
            return "'" + term + "'"
        elif isinstance(term, clif.ArithmeticExpr):
            return term.model_str("minizinc")
        else:
            raise SemanticException(
                "Something went wrong parsing constraint terms"
            )

    def to_string(self) -> str:
        if self.terms is not None:
            if self.arithmetic_predicate is not None:
                if len(self.terms) != 2:
                    raise SemanticException(
                        "Arithmetic predicates are binary only"
                    )
                else:
                    rhs, lhs = (
                        self.render_expr(self.terms[0]),
                        self.render_expr(self.terms[1]),
                    )
                    return f"constraint {rhs}{self.arithmetic_predicate}{lhs}"

            else:
                raise NotImplementedError("No handling of reification yet...")
        else:
            raise NotImplementedError(
                "No handling for more complex stuff yet..."
            )


def handle_bool_sentence(
    sentence: clif.BoolSentence, top_level: bool
) -> list[MZNExpression]:
    # first check if "sentences" has been bound
    # to determine we are in conjunction/disjunction case
    if len(sentence.sentences) > 0:
        if sentence.operator == "and":
            # conjunction is handled natively by mzn, they are simply additional
            # constraints and/or var decls
            exprs: list[MZNExpression] = []
            for s in sentence.sentences:
                if isinstance(s, clif.AtomSentence):
                    exprs.append(handle_atom_sentence(s, True))
                elif isinstance(s, clif.BoolSentence):
                    exprs.append(*handle_bool_sentence(s, True))
                elif isinstance(s, clif.QuantSentence):
                    raise NotImplementedError(
                        "No handling for quantification as inner constraint yet"
                    )
            return exprs
        # We should, in theory have the guarantee that op is then "or"
        else:
            raise NotImplementedError(
                "Native disjunction is not yet handled..."
            )
    # implication and biconditional case
    elif sentence.antecedent is not None and sentence.consequent is not None:
        raise NotImplementedError("Conditionals unsupported")
    # Negation case
    elif sentence.sentence is not None:
        raise NotImplementedError("Negation currently unsupported")
    else:
        raise SemanticException("invalid boolean expression")


def handle_atom_sentence(
    sentence: clif.AtomSentence, top_level: bool
) -> MZNExpression:
    # Atom case
    if (atom := sentence.atom) is not None:
        # Complex predicate, unhandled in mzn for now...
        if isinstance((pred := atom.pred), str):
            raise NotImplementedError("No handling yet for complex predicates")
        # case where the atom has an arithmetic predicate
        elif isinstance(pred, clif.ArithmeticPred):
            if len(atom.terms) != 2:
                raise SemanticException(
                    "Arithmetic predicates can only have two terms"
                )
            else:
                arithmetic_pred = (
                    ArithmeticPredicate.GT.value
                    if pred.gt
                    else ArithmeticPredicate.GTE.value
                    if pred.gte
                    else ArithmeticPredicate.LT.value
                    if pred.lt
                    else ArithmeticPredicate.LTE.value
                )
                return MZNConstraintDecl(arithmetic_pred, None, atom.terms)
                # model.add_constraint_decl(cons_decl)
        # case where we have a type declaration
        elif isinstance(pred, clif.TypePred):
            if len(atom.terms) != 1:
                raise SemanticException(
                    "Type declarations must have a single term"
                )
            elif isinstance(atom.terms[0], (int, clif.ArithmeticExpr)):
                raise SemanticException(
                    "Type declarations must be of a valid type"
                )
            else:
                var_type = "bool" if pred.boolean else "int"
                return MZNVarDecl(atom.terms[0], var_type)
                # var_decl = MZNVarDecl(atom.terms[0], var_type)
                # model.add_var_decl(var_decl)
        else:
            raise RuntimeError(f"Wrong class type for pred:{pred}")
    # Equation case
    elif sentence.eq is not None:
        return MZNConstraintDecl(
            arithmetic_predicate=ArithmeticPredicate.EQ.value,
            terms=[sentence.eq.lhs, sentence.eq.rhs],
        )
    else:
        raise RuntimeError("Something went wrong parsing the atom sentece")
        # cons_decl = MZNConstraintDecl(
        #     arithmetic_predicate=ArithmeticPredicate.EQ.value,
        #     terms=[sentence.eq.lhs, sentence.eq.rhs],
        # )
        # model.add_constraint_decl(cons_decl)


def handle_constraint(
    sentence: clif.Sentence, top_level: bool
) -> list[MZNExpression]:
    if isinstance(sentence, clif.AtomSentence):
        return [handle_atom_sentence(sentence, top_level)]
    elif isinstance(sentence, clif.BoolSentence):
        return handle_bool_sentence(sentence, top_level)
    else:
        raise TypeError("Only boolean and atomic senteces are handled")


def clif_to_MZN(clif_model: clif.Text) -> MZNModel:
    # the sentences in the text construction are the toplevel objects,
    # i.e. they correspond to the high-level constraints
    # that are given by the model itself
    high_level_constraints = clif_model.constructions.sentences
    if not high_level_constraints:
        raise RuntimeError("Empty set of sentences")
    mzn_model = MZNModel()
    for sentence in high_level_constraints:
        for c in handle_constraint(sentence=sentence, top_level=True):
            if isinstance(c, MZNVarDecl):
                mzn_model.add_var_decl(c)
            elif isinstance(c, MZNConstraintDecl):
                mzn_model.add_constraint_decl(c)
    return mzn_model
