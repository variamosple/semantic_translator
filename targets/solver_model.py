from abc import ABC, abstractmethod
from grammars import clif
from dataclasses import dataclass, field
from typing import Optional, cast
from enum import Enum, auto
from utils.exceptions import SemanticException


class ArithmeticPredicate(Enum):
    EQ = auto()
    NEQ = auto()
    GT = auto()
    GTE = auto()
    LT = auto()
    LTE = auto()


class ReificationPredicate(Enum):
    IMP = auto()
    BIMP = auto()


class TypePredType(Enum):
    BOOL = auto()
    BOUNDED_INT = auto()
    INT = auto()
    ENUM = auto()


class CSPExpression(ABC):
    # @abstractmethod
    def to_string(self) -> str:
        pass


@dataclass
class CSPVariable(CSPExpression):
    name: str
    type: TypePredType
    upper: int | None = 1
    lower: int | None = 0
    _default_bounds: tuple[int, int] | None = field(init=False)

    def __post_init__(self):
        if self.upper is not None and self.lower is not None:
            self._default_bounds = self.upper, self.lower
        else:
            self._default_bounds = None

    def fix_variable(self, value: int):
        if self.lower is None or self.lower <= value:
            self.lower = value
        else:
            raise SemanticException("This value violates the variable's bounds")
        
        if self.upper is None or self.upper >= value:
            self.upper = value
        else:
            raise SemanticException("This value violates the variable's bounds")

    # TODO: This is a hack, we should be able to fix variables for enums too
    def reset_fix(self):
        if self._default_bounds is not None:
            self.upper, self.lower = self._default_bounds
        else:
            self.upper, self.lower = None, None


@dataclass
class CSPEnumVariable(CSPVariable):
    values: list[str] = field(default_factory=list)


@dataclass
class CSPConstraint(CSPExpression):
    arithmetic_predicate: Optional[ArithmeticPredicate] = None
    reification_predicate: Optional[ReificationPredicate] = None
    terms: Optional[list[str | int | clif.ArithmeticExpr]] = None
    sub_constraints: Optional[list[list[CSPExpression]]] = None
    top_level: bool = True
    # handle negation case
    negation: bool = False
    # handle disjunction case
    disjunction: bool = False


@dataclass
class SolverModel(ABC):
    var_decls: dict[str, CSPVariable]
    constraints: list[CSPConstraint]

    # @abstractmethod
    def generate_program(self):
        pass

    # @abstractmethod
    def fix_variable(self, variable: str, value: int):
        pass

    # @abstractmethod
    def reset_fix(self, variable: str):
        pass

    @classmethod
    # @abstractmethod
    def from_clif_text(cls, clif_model: clif.Text):
        high_level_constraints = clif_model.constructions.sentences
        if not high_level_constraints:
            raise RuntimeError("No constraints found in CLIF model")
        var_decls: dict[str, CSPVariable] = dict()
        constraints: list[CSPConstraint] = []
        for sentence in high_level_constraints:
            for c in handle_constraint(sentence, top_level=True):
                if isinstance(c, (CSPVariable, CSPEnumVariable)):
                    var_decls[c.name] = c
                else:
                    constraints.append(cast(CSPConstraint, c))
        return cls(var_decls, constraints)


def handle_constraint(
    sentence: clif.Sentence, top_level: bool
) -> list[CSPExpression]:
    if isinstance(sentence, clif.AtomSentence):
        return [handle_atom_sentence(sentence, top_level)]
    elif isinstance(sentence, clif.BoolSentence):
        return handle_bool_sentence(sentence, top_level)
    else:
        raise TypeError("Only boolean and atomic senteces are handled")


def handle_bool_sentence(
    sentence: clif.BoolSentence, top_level: bool
) -> list[CSPExpression]:
    # first check if "sentences" has been bound
    # to determine we are in conjunction/disjunction case
    if len(sentence.sentences) > 0:
        if sentence.operator == "and":
            # conjunction is handled natively by mzn, they are simply additional
            # constraints and/or var decls
            exprs: list[CSPExpression] = []
            for s in sentence.sentences:
                if isinstance(s, clif.AtomSentence):
                    exprs.append(handle_atom_sentence(s, top_level))
                elif isinstance(s, clif.BoolSentence):
                    exprs.extend(handle_bool_sentence(s, top_level))
                elif isinstance(s, clif.QuantSentence):
                    raise NotImplementedError(
                        "No handling for quantification as inner constraint yet"
                    )
            return exprs
        # We should, in theory have the guarantee that op is then "or"
        elif sentence.operator == "or":
            # disjunction is handled natively by mzn, they are simply additional
            # constraints and/or var decls
            exprs: list[CSPExpression] = []
            for s in sentence.sentences:
                if isinstance(s, clif.AtomSentence):
                    exprs.append(handle_atom_sentence(s, top_level))
                elif isinstance(s, clif.BoolSentence):
                    exprs.extend(handle_bool_sentence(s, top_level))
                elif isinstance(s, clif.QuantSentence):
                    raise NotImplementedError(
                        "No handling for quantification as inner constraint yet"
                    )
            # There needs to be some sort of marker that this is a disjunction
            # and not a conjunction
            return [CSPConstraint(disjunction=True, sub_constraints=[exprs])]
        else:
            raise NotImplementedError(
                "Native disjunction is not yet handled..."
            )
    # implication and biconditional case
    elif sentence.antecedent is not None and sentence.consequent is not None:
        # This means we will be constructing subexpressions for a constraint
        exprs: list[CSPExpression] = []
        sub_expressions: list[list[CSPExpression]] = []
        sub_expressions.append(
            handle_constraint(sentence=sentence.antecedent, top_level=False)
        )
        sub_expressions.append(
            handle_constraint(sentence=sentence.consequent, top_level=False)
        )
        reif_predicate = (
            ReificationPredicate.IMP
            if sentence.operator == "if"
            else ReificationPredicate.BIMP
        )
        return [
            CSPConstraint(
                reification_predicate=reif_predicate,
                sub_constraints=sub_expressions,
                top_level=top_level,
            )
        ]
    # Negation case
    elif sentence.sentence is not None:
        inner_constraint = handle_constraint(
            sentence=sentence.sentence, top_level=False
        )
        return [
            CSPConstraint(
                negation=True,
                top_level=top_level,
                sub_constraints=[inner_constraint],
            )
        ]
    else:
        raise SemanticException("invalid boolean expression")


def handle_atom_sentence(
    sentence: clif.AtomSentence, top_level: bool
) -> CSPExpression:
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
                    ArithmeticPredicate.GT
                    if pred.gt
                    else ArithmeticPredicate.GTE
                    if pred.gte
                    else ArithmeticPredicate.LT
                    if pred.lt
                    else ArithmeticPredicate.LTE
                )
                return CSPConstraint(
                    arithmetic_pred, None, atom.terms, top_level=top_level
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
                if pred.boolean:
                    return CSPVariable(atom.terms[0], TypePredType.BOOL)
                elif pred.bounded_integer:
                    if pred.upper is None or pred.lower is None:
                        raise SemanticException(
                            "Bounded integer type must have upper and lower bounds"
                        )
                    return CSPVariable(
                        atom.terms[0],
                        TypePredType.BOUNDED_INT,
                        upper=pred.upper,
                        lower=pred.lower,
                    )
                elif pred.integer:
                    return CSPVariable(atom.terms[0], TypePredType.INT, None, None)
                # Handle enum case
                elif pred.enum:
                    if pred.values is None or len(pred.values) == 0:
                        raise SemanticException(
                            "Enum type must have at least one value"
                        )
                    return CSPEnumVariable(
                        atom.terms[0], TypePredType.ENUM, values=pred.values
                    )
                else:
                    raise RuntimeError("Unknown type predicate")
                # var_decl = MZNVarDecl(atom.terms[0], var_type)
                # model.add_var_decl(var_decl)
        else:
            raise RuntimeError(f"Wrong class type for pred:{pred}")
    # Equation case
    elif sentence.eq is not None:
        return CSPConstraint(
            arithmetic_predicate=ArithmeticPredicate.EQ,
            terms=[sentence.eq.lhs, sentence.eq.rhs],
            top_level=top_level,
        )
    else:
        raise RuntimeError("Something went wrong parsing the atom sentece")
        # cons_decl = MZNConstraintDecl(
        #     arithmetic_predicate=ArithmeticPredicate.EQ.value,
        #     terms=[sentence.eq.lhs, sentence.eq.rhs],
        # )
        # model.add_constraint_decl(cons_decl)
