from __future__ import annotations
from grammars import clif
from enum import Enum, unique

from utils.exceptions import SemanticException


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


class SWIModel:
    def __init__(self) -> None:
        pass


class SWIConstraint:
    def __init__(self) -> None:
        pass


def handle_atom(sentence: clif.AtomSentence):
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


def handle_sentence(sentence: clif.Sentence):
    if isinstance(sentence, clif.AtomSentence):
        handle_atom(sentence)
    elif isinstance(sentence, clif.BoolSentence):
        pass
    else:
        raise NotImplementedError("No other type of sentence handled yet")


def clif_to_SWI_objects(clif_model: clif.Text):
    # the sentences in the text construction are the toplevel objects,
    # i.e. they correspond to the high-level constraints
    # that are given by the model itself
    high_level_constraints = clif_model.constructions.sentences
    if not high_level_constraints:
        raise RuntimeError("Empty set of sentences")
    swi = SWIModel()
    for sentence in high_level_constraints:
        handle_sentence(sentence)
