# pyright: strict
from abc import ABC
from grammars import clif
from dataclasses import dataclass, field
from typing import cast
from enum import Enum, auto
from utils.exceptions import SemanticException


class ArithmeticPredicate(Enum):
    """
    Enum for arithmetic predicates. This is used to represent the
    different types of arithmetic relations that can be used in a
    constraint satisfaction problem.
    """
    EQ = auto()
    NEQ = auto()
    GT = auto()
    GTE = auto()
    LT = auto()
    LTE = auto()


class ReificationPredicate(Enum):
    """
    Enum for reification predicates. This is used to represent the
    different types of reification relations that can be used in a
    constraint satisfaction problem.
    """
    IMP = auto()
    BIMP = auto()


class TypePredType(Enum):
    """"
    Enum for type predicates. This is used to represent the different
    type declarations that can occur in a CSP.
    """
    BOOL = auto()
    BOUNDED_INT = auto()
    INT = auto()
    ENUM = auto()


class CSPExpression(ABC):
    """
    The CSPExpression is the base class from which all the CSP constraints
    are derived. It is an abstract class and should not be instantiated.

    It represents both predicates (constraints) and the variable declaration
    constraints that are used in a constraint satisfaction problem.
    """
    # @abstractmethod
    def to_string(self) -> str:
        raise NotImplementedError("This method must be implemented by the subclass")


@dataclass
class CSPVariable(CSPExpression):
    """
    The CSPVariable class is used to represent the variable declarations
    that are used in a constraint satisfaction problem. It is a dataclass
    and should be instantiated with the name of the variable and the type
    of the variable at a minimum. One can also specify the upper and lower
    bounds of the variable if it is a bounded integer type.
    
    TODO: Add for support for string constraints and possibly for object
    constraints as is the case for UVL.
    """
    name: str
    type: TypePredType
    upper: int | None = 1
    lower: int | None = 0
    _default_bounds: tuple[int, int] | None = field(init=False)

    def __post_init__(self):
        """
        This method is called after the dataclass has been initialized. It
        is used to set the default bounds of the variable if the bounds have been
        declared. It is not to be called directly.
        """
        if self.upper is not None and self.lower is not None:
            self._default_bounds = self.upper, self.lower
        else:
            self._default_bounds = None

    def fix_variable(self, value: int):
        """
        This method is used to "fix" the variable to a specific value. It restricts the
        variable's bounds to a single value. It is used in conjunction with the reset_fix
        function to reset the variable's bounds to their original values as defined in the
        _default_bounds attribute.
        """
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
        """"
        This method is used to reset the variable's bounds to their original values as
        defined in the _default_bounds attribute. It is the counterpart to the
        fix_variable method.
        """
        if self._default_bounds is not None:
            self.upper, self.lower = self._default_bounds
        else:
            self.upper, self.lower = None, None


@dataclass
class CSPEnumVariable(CSPVariable):
    values: list[str] = field(default_factory=list)


@dataclass
class CSPConstraint(CSPExpression):
    #arithmetic_predicate: Optional[ArithmeticPredicate] = None
    #reification_predicate: Optional[ReificationPredicate] = None
    #terms: Optional[list[str | int | clif.ArithmeticExpr]] = None
    #sub_constraints: Optional[list[list[CSPExpression]]] = None
    top_level: bool = True
    # handle negation case
    #negation: bool = False
    # handle disjunction case
    #disjunction: bool = False

@dataclass(kw_only=True)
class CSPArithmeticConstraint(CSPConstraint):
    arithmetic_predicate: ArithmeticPredicate
    terms: list[str | int | clif.ArithmeticExpr]
@dataclass(kw_only=True)
class CSPReificationConstraint(CSPConstraint):
    reification_predicate: ReificationPredicate
    lhs: CSPExpression
    rhs: CSPExpression

@dataclass(kw_only=True)
class CSPConjunctionConstraint(CSPConstraint):
    sub_constraints: list[CSPExpression]

@dataclass(kw_only=True)
class CSPDisjunctionConstraint(CSPConstraint):
    sub_constraints: list[CSPExpression]

@dataclass(kw_only=True)
class CSPNegationConstraint(CSPConstraint):
    sub_constraint: CSPExpression

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
            c = handle_constraint(sentence, top_level=True)
            if isinstance(c, (CSPVariable, CSPEnumVariable)):
                var_decls[c.name] = c
            else:
                # We must recursively unroll ands at the top level
                if isinstance(c, CSPConjunctionConstraint):
                    for un_c in unfold_ands(c):
                        if isinstance(un_c, (CSPVariable, CSPEnumVariable)):
                            var_decls[un_c.name] = un_c
                        else:  
                            constraints.append(cast(CSPConstraint, un_c))
                else:
                    constraints.append(cast(CSPConstraint, c))
            # for c in handle_constraint(sentence, top_level=True):
            #     if isinstance(c, (CSPVariable, CSPEnumVariable)):
            #         var_decls[c.name] = c
            #     else:
            #         constraints.append(cast(CSPConstraint, c))
        return cls(var_decls, constraints)

def unfold_ands(top_level_and: CSPConjunctionConstraint) -> list[CSPExpression]:
    """
    This function is used to unfold a top level conjunction into a list of constraints
    that will all be at the top level. This is necessary because we want to handle
    variables separately from the constraints in the generic csp.
    """
    constraints: list[CSPExpression] = []
    for c in top_level_and.sub_constraints:
        if isinstance(c, CSPConjunctionConstraint):
            constraints.extend(unfold_ands(c))
        else:
            constraints.append(c)
    return constraints

def handle_constraint(
    sentence: clif.Sentence, top_level: bool
) -> CSPExpression:
    if isinstance(sentence, clif.AtomSentence):
        return handle_atom_sentence(sentence, top_level)
    elif isinstance(sentence, clif.BoolSentence):
        return handle_bool_sentence(sentence, top_level)
    else:
        raise TypeError("Only boolean and atomic senteces are handled")


def handle_bool_sentence(
    sentence: clif.BoolSentence, top_level: bool
) -> CSPExpression:
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
                    exprs.append(handle_bool_sentence(s, top_level))
                elif isinstance(s, clif.QuantSentence):
                    raise NotImplementedError(
                        "No handling for quantification as inner constraint yet"
                    )
            return CSPConjunctionConstraint(sub_constraints=exprs, top_level=top_level)
        # We should, in theory have the guarantee that op is then "or"
        elif sentence.operator == "or":
            # disjunction is handled natively by mzn, they are simply additional
            # constraints and/or var decls
            exprs: list[CSPExpression] = []
            for s in sentence.sentences:
                if isinstance(s, clif.AtomSentence):
                    exprs.append(handle_atom_sentence(s, top_level=False))
                elif isinstance(s, clif.BoolSentence):
                    exprs.append(handle_bool_sentence(s, top_level=False))
                elif isinstance(s, clif.QuantSentence):
                    raise NotImplementedError(
                        "No handling for quantification as inner constraint yet"
                    )
            # There needs to be some sort of marker that this is a disjunction
            # and not a conjunction
            return CSPDisjunctionConstraint(sub_constraints=exprs, top_level=top_level)
        else:
            raise NotImplementedError(
                "Native disjunction is not yet handled..."
            )
    # implication and biconditional case
    elif sentence.antecedent is not None and sentence.consequent is not None:
        # This means we will be constructing subexpressions for a constraint
        # exprs: list[CSPExpression] = []
        # sub_expressions: list[] = []
        lhs = handle_constraint(sentence=sentence.antecedent, top_level=False)
        rhs = handle_constraint(sentence=sentence.consequent, top_level=False)
        
        reif_predicate = (
            ReificationPredicate.IMP
            if sentence.operator == "if"
            else ReificationPredicate.BIMP
        )
        return CSPReificationConstraint(
            reification_predicate=reif_predicate,
            # TODO: Check if this is the right way to handle this
            # in particular, check if the lists are doubly nested or not
            lhs=lhs,
            rhs=rhs,
            top_level=top_level,
        )
        # return [
        #     CSPConstraint(
        #         reification_predicate=reif_predicate,
        #         sub_constraints=sub_expressions,
        #         top_level=top_level,
        #     )
        # ]
    # Negation case
    elif sentence.sentence is not None:
        inner_constraint = handle_constraint(
            sentence=sentence.sentence, top_level=False
        )
        return CSPNegationConstraint(
            sub_constraint=inner_constraint, top_level=top_level
        )
        # return [
        #     CSPConstraint(
        #         negation=True,
        #         top_level=top_level,
        #         sub_constraints=[inner_constraint],
        #     )
        # ]
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
                # return CSPConstraint(
                #     arithmetic_pred, None, atom.terms, top_level=top_level
                # )
                return CSPArithmeticConstraint(
                    arithmetic_predicate=arithmetic_pred,
                    terms=atom.terms,
                    top_level=top_level,
                )
                # model.add_constraint_decl(cons_decl)
        # case where we have a type declaration
        # thus the type of "pred" is clif.TypePred
        else:
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

        # NOTE: This case in principle cannot happen, so we have removed it
        # to make the type checker happy
        # else:
        #     raise RuntimeError(f"Wrong class type for pred:{pred}")
    # Equation case
    elif sentence.eq is not None:
        # TODO: I think we are still missing the not equal case
        return CSPArithmeticConstraint(
            arithmetic_predicate=ArithmeticPredicate.EQ,
            terms=[sentence.eq.lhs, sentence.eq.rhs],
            top_level=top_level,
        )
        # return CSPConstraint(
        #     arithmetic_predicate=ArithmeticPredicate.EQ,
        #     terms=[sentence.eq.lhs, sentence.eq.rhs],
        #     top_level=top_level,
        # )
    else:
        raise RuntimeError("Something went wrong parsing the atom sentece")
        # cons_decl = MZNConstraintDecl(
        #     arithmetic_predicate=ArithmeticPredicate.EQ.value,
        #     terms=[sentence.eq.lhs, sentence.eq.rhs],
        # )
        # model.add_constraint_decl(cons_decl)
