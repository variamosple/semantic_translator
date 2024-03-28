# pyright: strict
from __future__ import annotations
# from utils.exceptions import SemanticException
from grammars import clif
from dataclasses import dataclass, field
from functools import singledispatch, singledispatchmethod
from targets.solver_model import (
    ArithmeticPredicate,
    CSPArithmeticConstraint,
    CSPConjunctionConstraint,
    CSPDisjunctionConstraint,
    CSPExpression,
    CSPNegationConstraint,
    CSPReificationConstraint,
    ReificationPredicate,
    CSPEnumVariable,
    CSPConstraint,
    CSPVariable,
    SolverModel,
    TypePredType,
)
import random
import string
import typing


@dataclass
class MZNModel:
    var_decls: dict[str, CSPVariable] = field(default_factory=dict)
    constraint_decls: list[CSPConstraint] = field(default_factory=list)
    __delimiter = ";"

    # TODO: This is a hack, we should be able to fix variables for enums too
    def fix_variable(self, variable: str, value: int):
        try:
            var_decl = self.var_decls[variable]
            var_decl.fix_variable(value)
        except KeyError:
            raise RuntimeError("You are trying to set a variable that does not exist")

    # TODO: This is a hack, we should be able to reset variables for enums too
    def reset_fix(self, variable: str):
        try:
            var_decl = self.var_decls[variable]
            var_decl.reset_fix()
        except KeyError:
            raise RuntimeError("You are trying to reset a variable that does not exist")

    def to_arith_pred_str(self, predicate: ArithmeticPredicate) -> str:
        match predicate:
            case ArithmeticPredicate.LT:
                return " < "
            case ArithmeticPredicate.LTE:
                return " <= "
            case ArithmeticPredicate.GT:
                return " > "
            case ArithmeticPredicate.GTE:
                return " >= "
            case ArithmeticPredicate.EQ:
                return " == "
            case ArithmeticPredicate.NEQ:
                return " != "

    def to_reif_pred_str(self, predicate: ReificationPredicate) -> str:
        match predicate:
            case ReificationPredicate.IMP:
                return " -> "
            case ReificationPredicate.BIMP:
                return " <-> "

    @staticmethod
    def quote_var(var_name: str) -> str:
        return "'" + var_name + "'"

    @singledispatchmethod
    def render_expression(self, expr: CSPExpression) -> str:
        raise NotImplementedError(
            "Cannot render an expression of type " + str(type(expr))
        )

    @render_expression.register(CSPEnumVariable)
    def _(self, expr: CSPEnumVariable) -> str:
        # generate a random name for the enum
        enum_name = f"enum_{''.join(random.choices(string.ascii_letters, k=10))}"
        # create the enum declaration
        # PS why the triple brackets?
        enum_decl_str = f"enum {enum_name} = {{{', '.join(expr.values)}}}"
        # render the final string
        qvar = self.quote_var(expr.name)
        # TODO: handle the case where the variable is fixed
        # TODO: check if it is even needed
        return f"{enum_decl_str}; var {enum_name}:{qvar}"

    @render_expression.register(CSPVariable)
    def _(self, expr: CSPVariable) -> str:
        qvar = self.quote_var(expr.name)
        if (
            expr.type == TypePredType.BOOL
            or expr.type == TypePredType.BOUNDED_INT
            or (expr.lower is not None and expr.upper is not None)
        ):
            return f"var {expr.lower}..{expr.upper}:{qvar}"
        else:
            return f"var int:{qvar}"

    @render_expression.register(CSPArithmeticConstraint)
    def _(self, expr: CSPArithmeticConstraint) -> str:
        if len(expr.terms) != 2:
            raise RuntimeError("Arithmetic constraints are binary only")
        rhs, lhs = (
            render_mzn_expression(expr.terms[0]),
            render_mzn_expression(expr.terms[1]),
        )
        top_level_str = "constraint " if expr.top_level else ""
        return (
            f"{top_level_str}{rhs}" 
            f"{self.to_arith_pred_str(expr.arithmetic_predicate)}" 
            f"{lhs}"
        )
    
    @render_expression.register(CSPReificationConstraint)
    def _(self, expr: CSPReificationConstraint) -> str:
        ant = self.render_expression(expr.lhs)
        con = self.render_expression(expr.rhs)
        top_level_str = "constraint " if expr.top_level else ""
        return (
            f"{top_level_str}({ant})" 
            f"{self.to_reif_pred_str(expr.reification_predicate)}" 
            f"({con})"
        )

    @render_expression.register(CSPConjunctionConstraint)
    def _(self, expr: CSPConjunctionConstraint) -> str:
        top_level_str = "constraint " if expr.top_level else ""
        return top_level_str + " /\\ ".join(
            (
                self.render_expression(c)
                for c in expr.sub_constraints
            )
        )

    @render_expression.register(CSPDisjunctionConstraint)
    def _(self, expr: CSPDisjunctionConstraint) -> str:
        top_level_str = "constraint " if expr.top_level else ""
        return top_level_str + "\\/ ".join(
            (
                self.render_expression(c)
                for c in expr.sub_constraints
            )
        )

    @render_expression.register(CSPNegationConstraint)
    def _(self, expr: CSPNegationConstraint) -> str:
        top_level_str = "constraint " if expr.top_level else ""
        return f"{top_level_str}not ({self.render_expression(expr.sub_constraint)})"

    def generate_program(self) -> list[str]:
        strs: list[str] = []
        constraints: list[CSPConstraint | CSPVariable] = [
            *self.var_decls.values(),
            *self.constraint_decls,
        ]
        for cons in constraints:
            strs.append(self.render_expression(cons) + self.__delimiter)
        return strs

    @classmethod
    def from_gen_csp(cls, generic_csp: SolverModel):
        return cls(generic_csp.var_decls, generic_csp.constraints)

@singledispatch
def render_mzn_expression(term: typing.Any) -> str:
    raise NotImplementedError(f"Cannot render an expression of type {type(term)}")

@render_mzn_expression.register(int)
def _(term: int) -> str:
    return str(term)

@render_mzn_expression.register(str)
def _(term: str) -> str:
    return f"'{term}'"

@render_mzn_expression.register(clif.ArithmeticExpr)
def _(term: clif.ArithmeticExpr) -> str:
    return term.model_str("minizinc")