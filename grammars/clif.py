from __future__ import annotations
from textx.metamodel import metamodel_from_file, TextXMetaModel
from textx import TextXSemanticError
from typing import Any, Optional
from abc import ABC, abstractmethod
from utils.enums import TargetLang
from dataclasses import dataclass


@dataclass
class Text:
    constructions: TextConstruction
    # def __init__(self, constructions: TextConstruction) -> None:
    #     self.constructions = constructions

    # def model_str(self, target: str) -> list[str]:
    #     """Construct a string representation of the clif model in
    #     a given target language"""
    #     return self.constructions.model_str(target)


@dataclass
class TextConstruction:
    parent: Text
    model: bool
    verif: bool
    sentences: list[Sentence]
    # def __init__(
    #     self, parent: Text, model: bool, verif: bool, sentences: list[Sentence]
    # ) -> None:
    #     self.parent = parent
    #     self.model = model
    #     self.verif = verif
    #     self.sentences = sentences

    # def model_str(self, target: str) -> list[str]:
    #     return [s.model_str(target) for s in self.sentences]


@dataclass
class Sentence(ABC):
    parent: TextConstruction | Sentence
    # def __init__(self, parent: TextConstruction | Sentence) -> None:
    #     self.parent = parent

    # @abstractmethod
    # def model_str(self, target: str) -> str:
    #     pass


@dataclass
class AtomSentence(Sentence):
    eq: Optional[Equation]
    atom: Optional[Atom]
    # def __init__(
    #     self,
    #     parent: TextConstruction | Sentence,
    #     eq: Optional[Equation],
    #     atom: Optional[Atom],
    # ) -> None:
    #     super().__init__(parent)
    #     self.eq = eq
    #     self.atom = atom

    # def type_decl(self) -> bool:
    #     return self.atom is not None and isinstance(self.atom.pred, TypePred)

    # def type_decl_sibling(self) -> bool:
    #     return (
    #         self.eq is not None
    #         and isinstance(self.parent, BoolSentence)
    #         and len(self.parent.sentences) == 2
    #         and isinstance((sib := self.parent.sentences[0]), AtomSentence)
    #         and sib.atom is not None
    #         and isinstance(sib.atom.pred, TypePred)
    #     )

    # def model_str(self, target):
    #     top_level_mzn_constraint_header = (
    #         "constraint "
    #         if (
    #             isinstance(self.parent, TextConstruction)
    #             and not self.type_decl()
    #         )
    #         or self.type_decl_sibling()
    #         else ""
    #     )
    #     if self.eq is not None:
    #         return top_level_mzn_constraint_header + self.eq.model_str(target)
    #     elif self.atom is not None:
    #         return top_level_mzn_constraint_header + self.atom.model_str(target)
    #     else:
    #         raise RuntimeError("An atomic sentence cannot be empty")


@dataclass
class Equation:
    parent: AtomSentence
    lhs: str | int | ArithmeticExpr
    rhs: str | int | ArithmeticExpr
    # def __init__(
    #     self,
    #     parent,
    #     lhs: str | int | ArithmeticExpr,
    #     rhs: str | int | ArithmeticExpr,
    # ) -> None:
    #     self.parent = parent
    #     self.lhs = lhs
    #     self.rhs = rhs

    # def render_operand(self, target, left: bool = True) -> str:
    #     prop = self.lhs if left else self.rhs
    #     if isinstance(prop, str):
    #         return prop if target == TargetLang.swi else "'" + prop + "'"
    #     if isinstance(prop, int):
    #         return str(prop)
    #     elif isinstance(prop, ArithmeticExpr):
    #         return prop.model_str()
    #     else:
    #         raise RuntimeError(f"Wrong class type for prop:{prop}")

    # def model_str(self, target):
    #     equality_sign = " #= " if target == TargetLang.swi else " == "
    #     return (
    #         self.render_operand(target, True)
    #         + equality_sign
    #         + self.render_operand(target, False)
    #     )


@dataclass
class Atom:
    parent: AtomSentence
    pred: str | ArithmeticPred | TypePred
    terms: list[str | int | ArithmeticExpr]
    # def __init__(
    #     self,
    #     parent,
    #     pred: str | ArithmeticPred | TypePred,
    #     terms: list[str | int | ArithmeticExpr],
    # ) -> None:
    #     self.parent = parent
    #     self.pred = pred
    #     self.terms = terms

    # def render_term(self, idx: int, target):
    #     t = self.terms[idx]
    #     if isinstance(t, str):
    #         return t if target == TargetLang.swi else "'" + t + "'"
    #     if isinstance(t, int):
    #         return str(t)
    #     elif isinstance(t, ArithmeticExpr):
    #         return t.model_str()
    #     else:
    #         raise RuntimeError(f"Wrong class type for term:{self.terms[0]}")

    # def model_str(self, target) -> str:
    #     if isinstance(self.pred, str):
    #         raise NotImplementedError("No handling yet for complex predicates")
    #     elif isinstance(self.pred, ArithmeticPred):
    #         if len(self.terms) != 2:
    #             raise TextXSemanticError(
    #                 "Arithmetic predicates can only have two terms"
    #             )
    #         else:
    #             t0 = self.render_term(0, target)
    #             t1 = self.render_term(1, target)
    #             return t0 + self.pred.model_str(target) + t1
    #     elif isinstance(self.pred, TypePred):
    #         if len(self.terms) != 1:
    #             raise TextXSemanticError("Type predicates are unary")
    #         else:
    #             return self.pred.model_str(target)
    #     else:
    #         raise RuntimeError(f"Wrong class type for pred:{self.pred}")


@dataclass
class ArithmeticPred:
    parent: Any
    lt: bool
    lte: bool
    gt: bool
    gte: bool
    # def __init__(
    #     self, parent, lt: bool, lte: bool, gt: bool, gte: bool
    # ) -> None:
    #     self.parent = parent
    #     self.lt = lt
    #     self.lte = lte
    #     self.gt = gt
    #     self.gte = gte

    # def model_str(self, target) -> str:
    #     if self.lt:
    #         return " #< " if target == TargetLang.swi else " < "
    #     elif self.lte:
    #         return " #=< " if target == TargetLang.swi else " <= "
    #     elif self.gt:
    #         return " #> " if target == TargetLang.swi else " > "
    #     elif self.gte:
    #         return " #>= " if target == TargetLang.swi else " >= "
    #     else:
    #         raise RuntimeError(
    #             "Something went wrong parsing the arithmetic pred"
    #         )


@dataclass
class ArithmeticExpr:
    parent: Any
    expr: E
    # def __init__(self, parent, expr: E) -> None:
    #     self.parent = parent
    #     self.expr = expr

    def model_str(self, target: str = "swi") -> str:
        return self.expr.model_str(target)


@dataclass
class E:
    parent: Any
    t: T
    ep: Optional[EP]
    # def __init__(self, parent, t: T, ep: Optional[EP]) -> None:
    #     self.parent = parent
    #     self.t = t
    #     self.ep = ep

    def model_str(self, target) -> str:
        return self.t.model_str(target) + (
            self.ep.model_str(target) if self.ep is not None else ""
        )


@dataclass
class EP:
    parent: Any
    op: str
    t: T
    ep: Optional[EP]
    # def __init__(self, parent, op: str, t: T, ep: Optional[EP]) -> None:
    #     self.parent = parent
    #     self.op = op
    #     self.t = t
    #     self.ep = ep

    def model_str(self, target) -> str:
        return (
            " "
            + self.op
            + " "
            + self.t.model_str(target)
            + (self.ep.model_str(target) if self.ep is not None else "")
        )


@dataclass
class T:
    parent: Any
    f: F
    tp: Optional[TP]
    # def __init__(self, parent, f: F, tp: Optional[TP]) -> None:
    #     self.parent = parent
    #     self.f = f
    #     self.tp = tp

    def model_str(self, target) -> str:
        return self.f.model_str(target) + (
            self.tp.model_str(target) if self.tp is not None else ""
        )


@dataclass
class TP:
    parent: Any
    op: str
    f: F
    tp: Optional[TP]
    # def __init__(self, parent, op, f: F, tp: Optional[TP]) -> None:
    #     self.parent = parent
    #     self.op = op
    #     self.f = f
    #     self.tp = tp

    def model_str(self, target) -> str:
        return (
            " "
            + self.op
            + " "
            + self.f.model_str(target)
            + (self.tp.model_str(target) if self.tp is not None else "")
        )


@dataclass
class F:
    parent: Any
    expr: Optional[E]
    val: Optional[str | int | ArithmeticExpr]
    # def __init__(
    #     self,
    #     parent,
    #     expr: Optional[E],
    #     val: Optional[str | int | ArithmeticExpr],
    # ) -> None:
    #     self.parent = parent
    #     self.expr = expr
    #     self.val = val

    def model_str(self, target) -> str:
        if self.expr is not None:
            return self.expr.model_str(target)
        elif self.val is not None:
            if isinstance(self.val, int):
                return str(self.val)
            elif isinstance(self.val, str):
                if target == TargetLang.swi:
                    return self.val
                else:
                    return "'" + self.val + "'"
            elif isinstance(self.val, ArithmeticExpr):
                return self.val.model_str(target)
            else:
                raise RuntimeError(f"Wrong class type for prop:{self.val}")
        else:
            raise RuntimeError(
                "either expr or val must be defined in an F-expression"
            )


@dataclass
class TypePred:
    parent: Atom
    boolean: bool
    integer: bool
    bounded_integer: bool
    lower: int | None
    upper: int | None
    enum: bool
    values: Optional[list[str]]
    # def __init__(
    #     self,
    #     parent: Atom,
    #     boolean: bool,
    #     integer: bool,
    #     enum: bool,
    #     values: Optional[list[str]],
    # ) -> None:
    #     self.parent = parent
    #     self.boolean = boolean
    #     self.integer = integer
    #     self.enum = enum
    #     self.values = values

    # def render_bool_type(self, target) -> str:
    #     if target == TargetLang.swi:
    #         return str(self.parent.terms[0]) + " in 0..1"
    #     else:
    #         return f"var 0..1:{self.parent.terms[0]}"

    # def model_str(self, target) -> str:
    #     if self.boolean:
    #         return self.render_bool_type(target)
    #     else:
    #         raise NotImplementedError("Haven't done this part yet")


# Something special happens with this object,
# since we use the += operator for the sentences
# it cannot be None...
@dataclass
class BoolSentence(Sentence):
    parent: TextConstruction | Sentence
    operator: str
    sentences: list[Sentence]
    antecedent: Optional[Sentence]
    consequent: Optional[Sentence]
    sentence: Optional[Sentence]
    # def __init__(
    #     self,
    #     parent: TextConstruction | Sentence,
    #     operator: str,
    #     sentences: list[Sentence],
    #     antecedent: Optional[Sentence],
    #     consequent: Optional[Sentence],
    #     sentence: Optional[Sentence],
    # ) -> None:
    #     super().__init__(parent)
    #     self.operator = operator
    #     self.sentences = sentences
    #     self.antecedent = antecedent
    #     self.consequent = consequent
    #     self.sentence = sentence

    # def render_conditional(self, target) -> str:
    #     if self.operator == "if":
    #         return " #==> " if target == TargetLang.swi else " -> "
    #     else:
    #         return " #<==> " if target == TargetLang.swi else " <-> "

    # def model_str(self, target: str):
    #     # i.e there are matches for the sentences...
    #     # it means we are in the first case
    #     if len(self.sentences) > 0:
    #         if self.operator == "and":
    #             # conjunction is handled natively by the different
    #             # solvers...
    #             if target == TargetLang.swi:
    #                 return ",\n".join(
    #                     [s.model_str(target) for s in self.sentences]
    #                 )
    #             else:
    #                 return ";\n".join(
    #                     [s.model_str(target) for s in self.sentences]
    #                 )
    #         # We should, in theory have the guarantee that op is then "or"
    #         else:
    #             raise NotImplementedError(
    #                 "Native disjunction is not yet handled..."
    #             )
    #     elif self.antecedent is not None and self.consequent is not None:
    #         # We can only deal with cond/bicond for FD Constraints,
    #         # we'll deal with full FOL later since it is much harder...
    #         # For now we'll also restrict the depth to 1 of the conditions...
    #         if isinstance(self.antecedent, AtomSentence) and isinstance(
    #             self.consequent, AtomSentence
    #         ):
    #             return (
    #                 self.antecedent.model_str(target)
    #                 + self.render_conditional(target)
    #                 + self.consequent.model_str(target)
    #             )
    #         else:
    #             raise NotImplementedError(
    #                 "We only support depth 1 conditionals"
    #             )
    #     elif self.sentence is not None:
    #         raise NotImplementedError("Negation currently unsupported")


@dataclass
class QuantSentence(Sentence):
    operator: str
    boundlist: BoundList
    sentence: Sentence
    # def __init__(
    #     self, parent, operator: str, boundlist: BoundList, sentence: Sentence
    # ) -> None:
    #     super().__init__(parent)
    #     self.operator = operator
    #     self.boundlist = boundlist
    #     self.sentence = sentence

    # def model_str(self):
    #     raise NotImplementedError("Not Done Yet")


@dataclass
class BoundList:
    parent: QuantSentence
    vars: list[str]
    # def __init__(self, parent, vars: list[str]) -> None:
    #     self.parent = parent
    #     self.vars = vars


def clif_meta_model(debug: Optional[bool] = None) -> TextXMetaModel:
    return metamodel_from_file(
        "./grammars/clif.tx",
        classes=[
            Text,
            TextConstruction,
            AtomSentence,
            BoolSentence,
            QuantSentence,
            BoundList,
            Atom,
            Equation,
            ArithmeticPred,
            ArithmeticExpr,
            E,
            EP,
            T,
            TP,
            F,
            TypePred,
        ],
        debug=debug,
    )


# def clif_model(
#     meta_model: TextXMetaModel, constraints_list: list[str], target
# ) -> list[str]:
#     c_str = "\n".join(constraints_list)
#     mod: Text = meta_model.model_from_str(c_str)
#     return (
#         mod.model_str(target)
#         if target == TargetLang.swi
#         else list(map(lambda c: c + ";", mod.model_str(target)))
#     )
