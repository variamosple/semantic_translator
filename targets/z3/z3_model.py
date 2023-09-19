import z3
import typing
from grammars import clif
from targets.solver_model import (
    SolverModel,
    CSPVariable,
    CSPEnumVariable,
    CSPConstraint,
    TypePredType,
    ArithmeticPredicate,
    ReificationPredicate,
)
from utils.exceptions import SemanticException
from dataclasses import dataclass, field


@dataclass
class Z3Model:
    var_decls: dict[str, typing.Any] = field(default_factory=dict)
    enum_consts: dict[str, typing.Any] = field(default_factory=dict)
    constraints: list = field(default_factory=list)

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
                    ) = _construct_z3_enum_var(var)
                    for value, enum_const in enum_consts:
                        z3model.enum_consts[value] = enum_const
                else:
                    (
                        z3model.var_decls[var.name],
                        constraints,
                    ) = _construct_z3_var(var)
            else:
                raise SemanticException(
                    f"Unknown variable type {type(var)} for variable {var.name}"
                )
            if constraints is not None:
                z3model.constraints.append(constraints)
        for constraint in generic_csp.constraints:
            if isinstance(constraint, CSPConstraint):
                z3model.constraints.append(
                    _construct_z3_constraint(constraint, z3model)
                )
            else:
                raise SemanticException(
                    f"Unknown constraint type {type(constraint)} for constraint {constraint}"
                )
        return z3model


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


def _construct_z3_constraint(constraint: CSPConstraint, z3model: Z3Model):
    if isinstance(constraint, CSPConstraint):
        if constraint.arithmetic_predicate is not None:
            if constraint.terms is not None:
                return _construct_z3_arithmetic_constraint(constraint, z3model)
            else:
                raise SemanticException(
                    f"Constraint {constraint}"
                    "has arithmetic predicate but no terms"
                )
        elif constraint.reification_predicate is not None:
            if constraint.sub_constraints is not None:
                return _construct_z3_reification_constraint(constraint, z3model)
            else:
                raise SemanticException(
                    f"Constraint {constraint}"
                    "has reification predicate but no subconstraints"
                )
        else:
            raise SemanticException(
                f"Constraint {constraint}"
                "has neither arithmetic nor reification predicate"
            )
    else:
        raise SemanticException(
            f"Unknown constraint type {type(constraint)}"
            f"for constraint {constraint}"
        )


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
                return _construct_z3_arithmetic_expr(f.val, z3model)
        else:
            raise SemanticException(
                f"Arithmetic expression {expr} has neither expr nor val"
            )

    inner_expr = expr.expr
    return handle_e(inner_expr)


def _construct_z3_arithmetic_constraint(
    constraint: CSPConstraint, z3model: Z3Model
):
    if constraint.terms is None or len(constraint.terms) != 2:
        raise SemanticException(
            f"Arithmetic constraint {constraint} has more than two terms"
        )
    else:
        # Before we do the check to grab the terms, they may be inner arithmetic
        # expressions so we do need to check for that
        if isinstance(constraint.terms[0], clif.ArithmeticExpr):
            term1 = _construct_z3_arithmetic_expr(constraint.terms[0], z3model)
        # First extract the terms and check if they are variables or constants
        elif constraint.terms[0] in z3model.var_decls:
            term1 = z3model.var_decls[constraint.terms[0]]
        elif constraint.terms[0] in z3model.enum_consts:
            term1 = z3model.enum_consts[constraint.terms[0]]
        else:
            # FIXME: No error handling!!!!
            term1 = 1 if constraint.terms[0] == 1 else 0
        if isinstance(constraint.terms[1], clif.ArithmeticExpr):
            term2 = _construct_z3_arithmetic_expr(constraint.terms[1], z3model)
        elif constraint.terms[1] in z3model.var_decls:
            term2 = z3model.var_decls[constraint.terms[1]]
        elif constraint.terms[1] in z3model.enum_consts:
            term2 = z3model.enum_consts[constraint.terms[1]]
        else:
            # FIXME: No error handling!!!!
            term2 = 1 if constraint.terms[1] == 1 else 0
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


def _construct_z3_reification_constraint(
    constraint: CSPConstraint, z3model: Z3Model
):
    if (
        constraint.sub_constraints is None
        or len(constraint.sub_constraints) != 2
    ):
        raise SemanticException(
            f"Reification constraint {constraint}"
            "has more than two subconstraints"
        )
    else:
        # We have to assume that the constraints are CSPConstraints
        subc1 = z3.And(
            [
                _construct_z3_constraint(subc, z3model)
                for subc in constraint.sub_constraints[0]
            ]
        )  # noqa
        subc2 = z3.And(
            [
                _construct_z3_constraint(subc, z3model)
                for subc in constraint.sub_constraints[1]
            ]
        )  # noqa
        match constraint.reification_predicate:
            case ReificationPredicate.IMP:
                return z3.Implies(subc1, subc2)
            case ReificationPredicate.BIMP:
                return z3.And(
                    z3.Implies(subc1, subc2), z3.Implies(subc2, subc1)
                )
