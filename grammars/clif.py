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


@dataclass
class TextConstruction:
    parent: Text
    model: bool
    verif: bool
    sentences: list[Sentence]


@dataclass
class Sentence(ABC):
    parent: TextConstruction | Sentence


@dataclass
class AtomSentence(Sentence):
    eq: Optional[Equation]
    atom: Optional[Atom]


@dataclass
class Equation:
    parent: AtomSentence
    lhs: str | int | ArithmeticExpr
    rhs: str | int | ArithmeticExpr


@dataclass
class Atom:
    parent: AtomSentence
    pred: str | ArithmeticPred | TypePred
    terms: list[str | int | ArithmeticExpr]


@dataclass
class ArithmeticPred:
    parent: Any
    lt: bool
    lte: bool
    gt: bool
    gte: bool


@dataclass
class ArithmeticExpr:
    parent: Any
    expr: E

    def model_str(self, target: str = "swi") -> str:
        return self.expr.model_str(target)


@dataclass
class E:
    parent: Any
    t: T
    ep: Optional[EP]

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


@dataclass
class BoolSentence(Sentence):
    parent: TextConstruction | Sentence
    operator: str
    sentences: list[Sentence]
    antecedent: Optional[Sentence]
    consequent: Optional[Sentence]
    sentence: Optional[Sentence]


@dataclass
class QuantSentence(Sentence):
    operator: str
    boundlist: BoundList
    sentence: Sentence


@dataclass
class BoundList:
    parent: QuantSentence
    vars: list[Binding]


@dataclass
class Binding:
    parent: BoundList
    var: str
    set_from: str


@dataclass
class AttributeLookup:
    parent: Any  #
    element: ElemSelector
    attribute: str


@dataclass
class ElemSelector:
    parent: AttributeLookup
    element: str
    fun: str | None


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
            Binding,
            AttributeLookup,
            ElemSelector,
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
