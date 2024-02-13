# pyright: basic
from __future__ import annotations
import z3
import typing
from grammars import clif
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
from utils.exceptions import SemanticException
from dataclasses import dataclass, field


@dataclass
class Z3Model:
    """
    This class describes a Z3 model. It stores the variable declarations,
    the enum constants, and the constraints. It also provides methods for
    fixing variables and resetting them. Since Z3 has an object based API,
    the variables and constraints are stored as Z3 objects, and hence no
    string representations necessary to generate the program.
    """

    var_decls: dict[str, typing.Any] = field(default_factory=dict)
    enum_consts: dict[str, typing.Any] = field(default_factory=dict)
    constraints: list = field(default_factory=list)

    # Temporarily this might make sense to handle it here
    # for uniformity, but it might make more sense to handle
    # it in the solver bridge
    def fix_variable(self, variable: str, value: int):
        if variable in self.var_decls:
            var = self.var_decls[variable]
            self.constraints.append(var == z3.IntVal(value))
        elif variable in self.enum_consts:
            raise NotImplementedError("Enum variable fixing not implemented")
        else:
            raise SemanticException(f"Variable {variable} not found in model {self}")

    def reset_fix(self, variable: str):
        # For now we can get away with just removing the last constraint
        # since we are only fixing one variable at a time
        if variable in self.var_decls:
            self.constraints.pop()
        elif variable in self.enum_consts:
            raise NotImplementedError("Enum variable fixing not implemented")
        else:
            raise SemanticException(f"Variable {variable} not found in model {self}")

    @classmethod
    def from_gen_csp(cls, generic_csp: SolverModel):
        z3model = cls()
        for var in generic_csp.var_decls.values():
            if isinstance(var, (CSPVariable)):
                if isinstance(var, CSPEnumVariable):
                    (
                        enum_consts,
                        z3model.var_decls[var.name],
                        constraints,
                    ) = Z3Model._construct_z3_enum_var(var)
                    for value, enum_const in enum_consts:
                        z3model.enum_consts[value] = enum_const
                else:
                    (
                        z3model.var_decls[var.name],
                        constraints,
                    ) = Z3Model._construct_z3_var(var)
            # Type checker says this code path is unreachable
            else:
                raise SemanticException(
                    f"Unknown variable type {type(var)} for variable {var.name}"
                )
            if constraints is not None:
                z3model.constraints.append(constraints)
        for constraint in generic_csp.constraints:
            if isinstance(constraint, CSPConstraint):
                z3model.constraints.append(
                    Z3Model._construct_z3_constraint(constraint, z3model)
                )
            else:
                raise SemanticException(
                    f"Unknown constraint type {type(constraint)} for constraint {constraint}"
                )
        return z3model

    @staticmethod
    def _construct_z3_var(var: CSPVariable) -> tuple:
        if isinstance(var, CSPVariable):
            match var.type:
                case TypePredType.BOOL:
                    return (
                        z3_var := z3.Int(var.name),
                        z3.Or([z3_var == 1, z3_var == 0]),
                    )
                case TypePredType.INT:
                    return (z3.Int(var.name), None)
                case TypePredType.BOUNDED_INT:
                    return (
                        z3_var := z3.Int(var.name),
                        z3.And(z3_var >= var.lower, z3_var <= var.upper),
                    )
                case _:
                    raise SemanticException("Unknown variable type")
        else:
            raise SemanticException(
                f"Unknown variable type {type(var)} for variable {var.name}"
            )

    @staticmethod
    def _construct_z3_enum_var(var: CSPEnumVariable):
        if isinstance(var, CSPEnumVariable):
            # Construct an enum for the context variable
            EnumConstructor, EnumConstants = z3.EnumSort(var.name, var.values)
            return (
                zip(var.values, EnumConstants),
                z3_var := z3.Const(var.name + "_const", EnumConstructor),
                z3.Or([z3_var == enum_const for enum_const in EnumConstants]),
            )
        else:
            raise SemanticException(
                f"Unknown variable type {type(var)} for variable {var.name}"
            )

    @singledispatchmethod
    @staticmethod
    def _construct_z3_constraint(constraint: CSPExpression, _: Z3Model):
        raise NotImplementedError(
            f"Cannot construct a Z3 constraint from expression {constraint}"
        )

    @staticmethod
    def _construct_z3_arithmetic_expr(expr: clif.ArithmeticExpr, z3model: Z3Model):
        def handle_e(e: clif.E):
            if e.ep is not None:
                if e.ep.op == "+":
                    return handle_t(e.t) + handle_ep(e.ep)
                else:
                    return handle_t(e.t) - handle_ep(e.ep)
            else:
                return handle_t(e.t)

        def handle_ep(ep: clif.EP):
            if ep.ep is not None:
                if ep.ep.op == "+":
                    return handle_t(ep.t) + handle_ep(ep.ep)
                else:
                    return handle_t(ep.t) - handle_ep(ep.ep)
            else:
                return handle_t(ep.t)

        def handle_t(t: clif.T):
            if t.tp is not None:
                if t.tp.op == "*":
                    return handle_f(t.f) * handle_tp(t.tp)
                else:
                    return handle_f(t.f) / handle_tp(t.tp)
            else:
                return handle_f(t.f)

        def handle_tp(tp: clif.TP):
            if tp.tp is not None:
                if tp.tp.op == "*":
                    return handle_f(tp.f) * handle_tp(tp.tp)
                else:
                    return handle_f(tp.f) / handle_tp(tp.tp)
            else:
                return handle_f(tp.f)

        def handle_f(f: clif.F):
            if f.expr is not None:
                return handle_e(f.expr)
            elif f.val is not None:
                if isinstance(f.val, int):
                    return z3.IntVal(f.val)
                elif isinstance(f.val, str):
                    if f.val in z3model.var_decls:
                        return z3model.var_decls[f.val]
                    elif f.val in z3model.enum_consts:
                        return z3model.enum_consts[f.val]
                    else:
                        raise SemanticException(
                            f"Arithmetic expression {expr} has val {f.val}"
                            "which is not a variable or constant"
                        )
                elif isinstance(f.val, clif.ArithmeticExpr):
                    return Z3Model._construct_z3_arithmetic_expr(f.val, z3model)
            else:
                raise SemanticException(
                    f"Arithmetic expression {expr} has neither expr nor val"
                )

        inner_expr = expr.expr
        return handle_e(inner_expr)

    @_construct_z3_constraint.register(CSPArithmeticConstraint)
    @staticmethod
    def _(constraint: CSPArithmeticConstraint, z3model: Z3Model):
        if constraint.terms is None or len(constraint.terms) != 2:
            raise SemanticException(
                f"Arithmetic constraint {constraint} has more than two terms"
            )
        else:
            # Before we do the check to grab the terms, they may be inner arithmetic
            # expressions so we do need to check for that
            if isinstance(constraint.terms[0], clif.ArithmeticExpr):
                term1 = Z3Model._construct_z3_arithmetic_expr(
                    constraint.terms[0], z3model
                )
            # First extract the terms and check if they are variables or constants
            elif constraint.terms[0] in z3model.var_decls:
                term1 = z3model.var_decls[constraint.terms[0]]
            elif constraint.terms[0] in z3model.enum_consts:
                term1 = z3model.enum_consts[constraint.terms[0]]
            else:
                # FIXME: No error handling!!!!
                term1 = 1 if constraint.terms[0] == 1 else 0
            if isinstance(constraint.terms[1], clif.ArithmeticExpr):
                term2 = Z3Model._construct_z3_arithmetic_expr(
                    constraint.terms[1], z3model
                )
            elif constraint.terms[1] in z3model.var_decls:
                term2 = z3model.var_decls[constraint.terms[1]]
            elif constraint.terms[1] in z3model.enum_consts:
                term2 = z3model.enum_consts[constraint.terms[1]]
            else:
                # FIXME: No error handling!!!!
                term2 = 1 if constraint.terms[1] == 1 else 0
            if term1 is None or term2 is None:
                raise SemanticException(
                    f"Arithmetic constraint {constraint} has None term"
                )
            match constraint.arithmetic_predicate:
                case ArithmeticPredicate.GT:
                    return term1 > term2
                case ArithmeticPredicate.GTE:
                    return term1 >= term2
                case ArithmeticPredicate.LT:
                    return term1 < term2
                case ArithmeticPredicate.LTE:
                    return term1 <= term2
                case ArithmeticPredicate.EQ:
                    return term1 == term2
                case ArithmeticPredicate.NEQ:
                    return term1 != term2

    @_construct_z3_constraint.register(CSPReificationConstraint)
    @staticmethod
    def _(constraint: CSPReificationConstraint, z3model: Z3Model):
        lhs = Z3Model._construct_z3_constraint(constraint.lhs, z3model)
        rhs = Z3Model._construct_z3_constraint(constraint.rhs, z3model)
        match constraint.reification_predicate:
            case ReificationPredicate.IMP:
                return z3.Implies(lhs, rhs)
            case ReificationPredicate.BIMP:
                return z3.And(z3.Implies(lhs, rhs), z3.Implies(rhs, lhs))

    @_construct_z3_constraint.register(CSPConjunctionConstraint)
    @staticmethod
    def _(constraint: CSPConjunctionConstraint, z3model: Z3Model):
        return z3.And(
            *(
                Z3Model._construct_z3_constraint(c, z3model)
                for c in constraint.sub_constraints
            )
        )

    @_construct_z3_constraint.register(CSPDisjunctionConstraint)
    @staticmethod
    def _(constraint: CSPDisjunctionConstraint, z3model: Z3Model):
        return z3.Or(
            *(
                Z3Model._construct_z3_constraint(c, z3model)
                for c in constraint.sub_constraints
            )
        )

    @_construct_z3_constraint.register(CSPNegationConstraint)
    @staticmethod
    def _(constraint: CSPNegationConstraint, z3model: Z3Model):
        return z3.Not(
            Z3Model._construct_z3_constraint(constraint.sub_constraint, z3model)
        )
