from __future__ import annotations
from grammars import clif
from dataclasses import dataclass, field
from utils.exceptions import SemanticException
from targets.solver_model import (
    ArithmeticPredicate,
    ReificationPredicate,
    CSPEnumVariable,
    CSPConstraint,
    CSPVariable,
    SolverModel,
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

    def swi_render_var_decl(self, var_decl: CSPVariable) -> str:
        if var_decl.lower is None or var_decl.upper is None:
            return f"{var_decl.name} in inf..sup"
        return f"{var_decl.name} in {var_decl.lower}..{var_decl.upper}"

    def swi_render_enum_var_decl(self, var_decl: CSPEnumVariable) -> str:
        if len(var_decl.values) == 0:
            raise SemanticException("Enum variables must have values")
        values_list = "[" + ", ".join(var_decl.values) + "]"
        ints = list(map(str, range(len(var_decl.values))))
        enum_values_list = "[" + ", ".join(ints) + "]"
        var_bounds = (ints[0], ints[-1])
        return (
            f"{values_list} = {enum_values_list}, "
            f"{var_decl.name} in {var_bounds[0]}..{var_bounds[1]}"
        )

    def swi_render_constraint(self, constraint: CSPConstraint) -> str:
        def render_expr(term) -> str:
            if isinstance(term, (int, str)):
                return str(term)
            elif isinstance(term, clif.ArithmeticExpr):
                return term.model_str()
            else:
                raise SemanticException(
                    "Something went wrong parsing constraint terms"
                )

        if constraint.arithmetic_predicate is not None:
            if constraint.terms is None or len(constraint.terms) != 2:
                raise SemanticException("Arithmetic predicates are binary only")
            else:
                rhs, lhs = (
                    render_expr(constraint.terms[0]),
                    render_expr(constraint.terms[1]),
                )
                return (
                    f"{rhs}"
                    f"{self.to_arith_pred_str(constraint.arithmetic_predicate)}"
                    f"{lhs}"
                )
        elif constraint.reification_predicate is not None:
            if (
                constraint.sub_constraints is None
                or len(constraint.sub_constraints) != 2
            ):
                raise SemanticException("Wrong number of subexpresions")
            sub_ant = " #/\\ ".join(
                (
                    self.render_csp_var_decl_or_constraint(c)
                    for c in constraint.sub_constraints[0]
                )
            )
            sub_con = " #/\\ ".join(
                (
                    self.render_csp_var_decl_or_constraint(c)
                    for c in constraint.sub_constraints[1]
                )
            )
            return (
                f"{sub_ant}"
                f"{self.to_reif_pred_str(constraint.reification_predicate)}"
                f"{sub_con}"
            )
        else:
            raise NotImplementedError(
                "No handling for more complex stuff yet..."
            )

    def render_csp_var_decl_or_constraint(
        self, item: CSPVariable | CSPConstraint
    ) -> str:
        if isinstance(item, CSPVariable):
            if isinstance(item, CSPEnumVariable):
                return self.swi_render_enum_var_decl(item)
            else:
                return self.swi_render_var_decl(item)
        elif isinstance(item, CSPConstraint):
            return self.swi_render_constraint(item)
        else:
            raise RuntimeError("Something went wrong rendering the csp item")

    def generate_program(self) -> list[str]:
        strs: list[str] = []
        constraints: list[CSPConstraint | CSPVariable] = [
            *self.var_decls.values(),
            *self.constraint_decls,
        ]
        for cons in constraints[:-1]:
            strs.append(
                self.render_csp_var_decl_or_constraint(cons) + self.__delimiter
            )
        strs.append(self.render_csp_var_decl_or_constraint(constraints[-1]))
        return strs

    @classmethod
    def from_gen_csp(cls, generic_csp: SolverModel):
        return cls(generic_csp.var_decls, generic_csp.constraints)
