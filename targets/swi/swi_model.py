# pyright: strict
from __future__ import annotations
import typing
from grammars import clif
from dataclasses import dataclass, field
from utils.exceptions import SemanticException
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
    # TypePredType,
)


@dataclass
class SWIModel:
    var_decls: dict[str, CSPVariable] = field(default_factory=dict)
    constraint_decls: list[CSPConstraint] = field(default_factory=list)
    __delimiter = ","

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

    @staticmethod
    def to_arith_pred_str(predicate: ArithmeticPredicate) -> str:
        match predicate:
            case ArithmeticPredicate.LT:
                return " #< "
            case ArithmeticPredicate.LTE:
                return " #=< "
            case ArithmeticPredicate.GT:
                return " #> "
            case ArithmeticPredicate.GTE:
                return " #>= "
            case ArithmeticPredicate.EQ:
                return " #= "
            case ArithmeticPredicate.NEQ:
                return " #\\= "

    @staticmethod
    def to_reif_pred_str(predicate: ReificationPredicate) -> str:
        match predicate:
            case ReificationPredicate.IMP:
                return " #==> "
            case ReificationPredicate.BIMP:
                return " #<==> "
            
    @singledispatchmethod
    def render_expression(self, expr: CSPExpression) -> str:
        raise  NotImplementedError(
            "Cannot render an expression of type " + str(type(expr))
        )
    
    @render_expression.register(CSPEnumVariable)
    def _(self, expr: CSPEnumVariable) -> str:
        if len(expr.values) == 0:
            raise SemanticException("Enum variables must have values")
        values_list = "[" + ", ".join(expr.values) + "]"
        ints = list(map(str, range(len(expr.values))))
        enum_values_list = "[" + ", ".join(ints) + "]"
        var_bounds = (ints[0], ints[-1])
        return (
            f"{values_list} = {enum_values_list}, "
            f"{expr.name} in {var_bounds[0]}..{var_bounds[1]}"
        )
    
    @render_expression.register(CSPVariable)
    def _(self, expr: CSPVariable) -> str:
        if expr.lower is None or expr.upper is None:
            return f"{expr.name} in inf..sup"
        return f"{expr.name} in {expr.lower}..{expr.upper}"

    @render_expression.register(CSPArithmeticConstraint)
    def _(self, expr: CSPArithmeticConstraint) -> str:
        if len(expr.terms) != 2:
            raise RuntimeError("Arithmetic constraints are binary only")
        rhs, lhs = (
            render_swi_expression(expr.terms[0]),
            render_swi_expression(expr.terms[1]),
        )
        return (
            f"{rhs}"
            f"{self.to_arith_pred_str(expr.arithmetic_predicate)}"
            f"{lhs}"
        )
    
    @render_expression.register(CSPReificationConstraint)
    def _(self, expr: CSPReificationConstraint) -> str:
        ant = self.render_expression(expr.lhs)
        con = self.render_expression(expr.rhs)
        return (
            f"({ant})"
            f"{self.to_reif_pred_str(expr.reification_predicate)}"
            f"({con})"
        )
    
    @render_expression.register(CSPConjunctionConstraint)
    def _(self, expr: CSPConjunctionConstraint) -> str:
        return " #/\\ ".join(
            (
                self.render_expression(c)
                for c in expr.sub_constraints
            )
        )
    
    @render_expression.register(CSPDisjunctionConstraint)
    def _(self, expr: CSPDisjunctionConstraint) -> str:
        return " #\\/ ".join(
            (
                self.render_expression(c)
                for c in expr.sub_constraints
            )
        )
    
    @render_expression.register(CSPNegationConstraint)
    def _(self, expr: CSPNegationConstraint) -> str:
        return f"#\\ ({self.render_expression(expr.sub_constraint)})"

    def generate_program(self) -> list[str]:
        strs: list[str] = []
        constraints: list[CSPConstraint | CSPVariable] = [
            *self.var_decls.values(),
            *self.constraint_decls,
        ]
        for cons in constraints[:-1]:
            strs.append(
                self.render_expression(cons) + self.__delimiter
            )
        strs.append(self.render_expression(constraints[-1]))
        return strs

    @classmethod
    def from_gen_csp(cls, generic_csp: SolverModel):
        return cls(generic_csp.var_decls, generic_csp.constraints)
    
@singledispatch
def render_swi_expression(term: typing.Any) -> str:
    raise NotImplementedError(f"Cannot render an expression of type {type(term)}")

@render_swi_expression.register(int)
def _(term: int) -> str:
    return str(term)

@render_swi_expression.register(str)
def _(term: str) -> str:
    return str(term)

@render_swi_expression.register(clif.ArithmeticExpr)
def _(term: clif.ArithmeticExpr) -> str:
    return term.model_str("swi")
