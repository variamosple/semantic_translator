from __future__ import annotations
from utils.exceptions import SemanticException
from grammars import clif
from dataclasses import dataclass, field
from targets.solver_model import (
    ArithmeticPredicate,
    ReificationPredicate,
    CSPEnumVariable,
    CSPConstraint,
    CSPVariable,
    SolverModel,
)
import random
import string


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
            raise RuntimeError(
                "You are trying to set a variable that does not exist"
            )

    # TODO: This is a hack, we should be able to reset variables for enums too
    def reset_fix(self, variable: str):
        try:
            var_decl = self.var_decls[variable]
            var_decl.reset_fix()
        except KeyError:
            raise RuntimeError(
                "You are trying to reset a variable that does not exist"
            )

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
    def quote_var(var_name) -> str:
        return "'" + var_name + "'"

    def mzn_render_csp_var_decl(self, var_decl: CSPVariable) -> str:
        qvar = self.quote_var(var_decl.name)
        if (
            var_decl.type == "bool"
            or var_decl.type == "bounded_int"
            or (var_decl.lower is not None and var_decl.upper is not None)
        ):
            return f"var {var_decl.lower}..{var_decl.upper}:{qvar}"
        else:
            return f"var int:{qvar}"

    def mzn_render_enum_var_decl(self, var_decl: CSPEnumVariable) -> str:
        # generate a random name for the enum
        enum_name = (
            f"enum_{''.join(random.choices(string.ascii_letters, k=10))}"
        )
        # create the enum declaration
        # PS why the triple brackets?
        enum_decl_str = f"enum {enum_name} = {{{', '.join(var_decl.values)}}}"
        # render the final string
        qvar = self.quote_var(var_decl.name)
        # TODO: handle the case where the variable is fixed
        # TODO: check if it is even needed
        return f"{enum_decl_str}; var {enum_name}:{qvar}"

    def mzn_render_constraint(self, constraint: CSPConstraint) -> str:
        def render_expr(self, term) -> str:
            if isinstance(term, int):
                return str(term)
            elif isinstance(term, str):
                return "'" + term + "'"
            elif isinstance(term, clif.ArithmeticExpr):
                return term.model_str("minizinc")
            else:
                raise SemanticException(
                    "Something went wrong parsing constraint terms"
                )

        top_level_str = "constraint " if constraint.top_level else ""
        if constraint.arithmetic_predicate is not None:
            if constraint.terms is None or len(constraint.terms) != 2:
                raise SemanticException("Arithmetic predicates are binary only")
            else:
                rhs, lhs = (
                    render_expr(self, constraint.terms[0]),
                    render_expr(self, constraint.terms[1]),
                )
                return (
                    f"{top_level_str}{rhs}"
                    f"{self.to_arith_pred_str(constraint.arithmetic_predicate)}"
                    f"{lhs}"
                )
        elif constraint.reification_predicate is not None:
            if (
                constraint.sub_constraints is None
                or len(constraint.sub_constraints) != 2
            ):
                raise SemanticException("Wrong number of subexpresions")
            sub_ant = " /\\ ".join(
                (
                    self.render_csp_var_decl_or_constraint(c)
                    for c in constraint.sub_constraints[0]
                )
            )
            sub_con = " /\\ ".join(
                (
                    self.render_csp_var_decl_or_constraint(c)
                    for c in constraint.sub_constraints[1]
                )
            )
            return (
                f"{top_level_str}({sub_ant})"
                f"{self.to_reif_pred_str(constraint.reification_predicate)}"
                f"({sub_con})"
            )

    def render_csp_var_decl_or_constraint(
        self, cons: CSPVariable | CSPConstraint
    ) -> str:
        if isinstance(cons, CSPVariable):
            if isinstance(cons, CSPEnumVariable):
                return self.mzn_render_enum_var_decl(cons)
            else:
                return self.mzn_render_csp_var_decl(cons)
        elif isinstance(cons, CSPConstraint):
            return self.mzn_render_constraint(cons)
        else:
            raise RuntimeError("Something went wrong rendering the constraint")

    def generate_program(self) -> list[str]:
        strs: list[str] = []
        constraints: list[CSPConstraint | CSPVariable] = [
            *self.var_decls.values(),
            *self.constraint_decls,
        ]
        for cons in constraints:
            strs.append(
                self.render_csp_var_decl_or_constraint(cons) + self.__delimiter
            )
        return strs

    @classmethod
    def from_gen_csp(cls, generic_csp: SolverModel):
        return cls(generic_csp.var_decls, generic_csp.constraints)
