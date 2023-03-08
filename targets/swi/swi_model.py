from __future__ import annotations
from typing import Optional
from grammars import clif
from enum import Enum, unique
from abc import ABC, abstractmethod
from utils.exceptions import SemanticException
from targets.solver_model import SolverModel


class StrEnum(str, Enum):
    pass


@unique
class ArithmeticPredicate(StrEnum):
    LT = " #< "
    LTE = " #=< "
    GT = " #> "
    GTE = " #>= "
    EQ = " #= "
    NEQ = " #\\= "


@unique
class ReificationPredicate(StrEnum):
    IMP = " #==> "
    BIMP = " #<==> "


class SWIModel(SolverModel):
    __delimiter = ","

    def __init__(self) -> None:
        self.var_decls: dict[str, SWIFDVarDomainDec | SWIEnumVarDec] = dict()
        self.constraint_decls: list[SWIFDConstraint] = []

    def add_cons(self, cons: SWIConstraint) -> None:
        match cons:
            case SWIFDVarDomainDec() | SWIEnumVarDec():
                assert cons.terms is not None
                assert isinstance((t := cons.terms[0]), str), "Must be a string"
                self.var_decls[t] = cons
            case SWIFDConstraint():
                self.constraint_decls.append(cons)

    def fix_variable(self, variable: str, value: int):
        try:
            var_decl = self.var_decls[variable]
            var_decl.fix_variable(value)
        except KeyError:
            raise RuntimeError(
                "You are trying to set a variable that does not exist"
            )

    def reset_fix(self, variable: str):
        try:
            var_decl = self.var_decls[variable]
            var_decl.reset_fix()
        except KeyError:
            raise RuntimeError(
                "You are trying to reset a variable that does not exist"
            )

    def generate_program(self) -> list[str]:
        strs: list[str] = []
        constraints: list[SWIConstraint] = [
            *self.var_decls.values(),
            *self.constraint_decls,
        ]
        for cons in constraints[:-1]:
            strs.append(cons.to_string() + self.__delimiter)
        strs.append(constraints[-1].to_string())
        return strs


class SWIConstraint(ABC):
    def __init__(
        self,
        terms: Optional[list[str | int | clif.ArithmeticExpr]] = None,
    ) -> None:
        self.terms = terms

    @abstractmethod
    def to_string(self) -> str:
        pass


class SWIFDConstraint(SWIConstraint):
    def __init__(
        self,
        arithmetic_predicate: Optional[str] = None,
        reification_predicate: Optional[str] = None,
        terms: Optional[list[str | int | clif.ArithmeticExpr]] = None,
        sub_constraints: Optional[list[list[SWIConstraint]]] = None,
    ) -> None:
        super().__init__(terms)
        self.arithmetic_predicate = arithmetic_predicate
        self.reification_predicate = reification_predicate
        self.sub_constraints = sub_constraints

    def render_expr(self, term) -> str:
        if isinstance(term, (int, str)):
            return str(term)
        elif isinstance(term, clif.ArithmeticExpr):
            return term.model_str()
        else:
            raise SemanticException(
                "Something went wrong parsing constraint terms"
            )

    # FIXME: improve and generalize
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
                    return f"{rhs}{self.arithmetic_predicate}{lhs}"

            else:
                raise NotImplementedError("No handling of reification yet...")
        elif self.reification_predicate is not None:
            if self.sub_constraints is None:
                raise SemanticException("Incorrectly instantiated reification")
            if len(self.sub_constraints) != 2:
                raise SemanticException("Wrong number of subexpresions")
            sub_ant = " #/\\ ".join(
                (c.to_string() for c in self.sub_constraints[0])
            )
            sub_con = " #/\\ ".join(
                (c.to_string() for c in self.sub_constraints[1])
            )
            return f"{sub_ant} {self.reification_predicate} {sub_con}"

        else:
            raise NotImplementedError(
                "No handling for more complex stuff yet..."
            )


class SWIFDVarDomainDec(SWIConstraint):
    def __init__(
        self,
        bounds: tuple[(int | str), ...] = ("inf", "sup"),
        # We have a guarantee that the term is a single element
        terms: Optional[list[str | int | clif.ArithmeticExpr]] = None,
    ) -> None:
        super().__init__(terms)
        self.bounds = bounds
        self._default_bounds = bounds

    def fix_variable(self, value):
        if self.bounds[0] <= value and value <= self.bounds[1]:
            self.bounds = value, value
        else:
            raise SemanticException("This value violates the variable's bounds")

    def reset_fix(self):
        self.bounds = self._default_bounds

    def to_string(self) -> str:
        if self.terms is not None and not isinstance(
            self.terms[0], (int, clif.ArithmeticExpr)
        ):
            return f"{self.terms[0]} in {self.bounds[0]}..{self.bounds[1]}"
        else:
            # FIXME: Add a better error/ figure out how to avoid
            # the check in the first place
            raise SemanticException("error in domain def")


class SWIEnumVarDec(SWIConstraint):
    def __init__(
        self,
        terms: Optional[list[str | int | clif.ArithmeticExpr]] = None,
        values: list[str] = [],
    ) -> None:
        super().__init__(terms)
        self.values = values

    def to_string(self) -> str:
        if len(self.values) == 0:
            raise SemanticException("Enum variables must have values")
        if self.terms is None or len(self.terms) != 1:
            raise SemanticException("Enum variables must have one term")
        values_list = "[" + ", ".join(self.values) + "]"
        ints = list(map(str, range(len(self.values))))
        enum_values_list = "[" + ", ".join(ints) + "]"
        var_bounds = (ints[0], ints[-1])
        enum_str = (
            f"{values_list} = {enum_values_list}, "
            f"{self.terms[0]} in {var_bounds[0]}..{var_bounds[1]}"
        )
        print(enum_str)
        return enum_str


def handle_bool_sentence(sentence: clif.BoolSentence) -> list[SWIConstraint]:
    if len(sentence.sentences) > 0:
        if sentence.operator == "and":
            # We can handle conjunction natively
            # by just expressing our constraints
            exprs: list[SWIConstraint] = []
            for s in sentence.sentences:
                if isinstance(s, clif.AtomSentence):
                    exprs.append(handle_atom_sentence(s))
                elif isinstance(s, clif.BoolSentence):
                    exprs.extend(handle_bool_sentence(s))
                else:
                    raise NotImplementedError(
                        "No handling for quantification as inner constraint yet"
                    )
            return exprs
        else:
            raise NotImplementedError(
                "Native disjunction is not yet handled..."
            )
    # implication and biconditional case
    elif sentence.antecedent is not None and sentence.consequent is not None:
        exprs: list[SWIConstraint] = []
        sub_expressions: list[list[SWIConstraint]] = []
        sub_expressions.append(handle_sentence(sentence=sentence.antecedent))
        sub_expressions.append(handle_sentence(sentence=sentence.consequent))
        reif_predicate = (
            ReificationPredicate.IMP
            if sentence.operator == "if"
            else ReificationPredicate.BIMP
        )
        return [
            SWIFDConstraint(
                reification_predicate=reif_predicate,
                sub_constraints=sub_expressions,
            )
        ]
    # Negation case
    elif sentence.sentence is not None:
        raise NotImplementedError("Negation currently unsupported")
    else:
        raise SemanticException("invalid boolean expression")


def handle_atom_sentence(sentence: clif.AtomSentence) -> SWIConstraint:
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
                return SWIFDConstraint(
                    arithmetic_predicate=arithmetic_pred, terms=atom.terms
                )
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
                # We know that this is a type decl
                if pred.boolean:
                    bounds = (0, 1)
                elif pred.bounded_integer:
                    if pred.lower is None or pred.upper is None:
                        raise SemanticException("integer bounds undefined")
                    bounds = (pred.lower, pred.upper)
                elif pred.integer:
                    # bounds = ("inf", "sup")
                    bounds = 0, 100000
                elif pred.enum:
                    if pred.values is None or len(pred.values) == 0:
                        raise SemanticException("Enum values undefined")
                    # special return case for enum
                    return SWIEnumVarDec(terms=atom.terms, values=pred.values)
                else:
                    raise SemanticException("Type declaration not supported")
                return SWIFDVarDomainDec(bounds=bounds, terms=atom.terms)
                # var_decl = MZNVarDecl(atom.terms[0], var_type)
                # model.add_var_decl(var_decl)
        else:
            raise RuntimeError(f"Wrong class type for pred:{pred}")
    # Equation case
    elif sentence.eq is not None:
        return SWIFDConstraint(
            arithmetic_predicate=ArithmeticPredicate.EQ.value,
            terms=[sentence.eq.lhs, sentence.eq.rhs],
        )
    else:
        raise RuntimeError("Something went wrong parsing the atom sentece")


def handle_sentence(sentence: clif.Sentence) -> list[SWIConstraint]:
    if isinstance(sentence, clif.AtomSentence):
        return [handle_atom_sentence(sentence)]
    elif isinstance(sentence, clif.BoolSentence):
        return handle_bool_sentence(sentence)
    # TODO: Handle other sentence types...
    else:
        raise NotImplementedError("No other type of sentence handled yet")


def clif_to_SWI(clif_model: clif.Text):
    # the sentences in the text construction are the toplevel objects,
    # i.e. they correspond to the high-level constraints
    # that are given by the model itself
    high_level_constraints = clif_model.constructions.sentences
    if not high_level_constraints:
        raise RuntimeError("Empty set of sentences")
    swi = SWIModel()
    for sentence in high_level_constraints:
        for constraint in handle_sentence(sentence):
            swi.add_cons(constraint)
    return swi
