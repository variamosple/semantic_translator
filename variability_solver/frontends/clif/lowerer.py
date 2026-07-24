from __future__ import annotations

from dataclasses import dataclass, field

from variability_solver.ir.constraints import (
    Addition,
    Biconditional,
    BooleanConstant,
    Conjunction,
    Disjunction,
    Division,
    Equality,
    Formula,
    Implication,
    IntConstant,
    LessEqual,
    LessThan,
    Multiplication,
    Negation,
    Subtraction,
    Term,
    VariableRef,
)
from variability_solver.ir.model import Model
from variability_solver.ir.variables import (
    BoolSort,
    Domain,
    FullDomain,
    IntSort,
    Sort,
    Variable,
)

from . import model as clif


class LoweringError(TypeError):
    pass


@dataclass
class _VariableInfo:
    var_id: int
    name: str
    sort: Sort | None
    domain: Domain | None


@dataclass
class _Context:
    variables: list[_VariableInfo] = field(default_factory=list)
    next_id: int = 0

    def new_variable(self, name: str) -> int:
        var_id = self.next_id
        self.next_id += 1
        self.variables.append(_VariableInfo(var_id, name, None, None))
        return var_id

    def get_variable(self, name: str) -> _VariableInfo | None:
        for variable in self.variables:
            if variable.name == name:
                return variable
        return None

    def change_sort(self, var_id: int, sort: Sort) -> None:
        for var in self.variables:
            if var.var_id == var_id:
                if var.sort is not None and var.sort != sort:
                    raise LoweringError(
                        f"Variable {var.name} with ID {var_id} already has a different sort"
                    )
                var.sort = sort
                return
        raise LoweringError(f"Variable with ID {var_id} not found")


def lower_clif(theory: clif.Theory) -> Model:
    ctx = _Context()
    constraints = [_lower_formula(f, ctx) for f in theory.formulas]
    variables = []
    for var in ctx.variables:
        if var.sort is None:
            raise LoweringError(f"Variable {var.name} has no sort")
        if var.domain is None:
            var.domain = FullDomain()
        variables.append(
            Variable(var_id=var.var_id, name=var.name, sort=var.sort, domain=var.domain)
        )
    return Model(
        variables=variables,
        constraints=constraints,
    )


def _lower_formula(node: clif.Formula, ctx: _Context) -> Formula:
    match node:
        case clif.Atom():
            return _lower_atom(node, ctx)

        case clif.Equality(left=left, right=right):
            return Equality(
                left=_lower_term(left, ctx),
                right=_lower_term(right, ctx),
            )

        case clif.Not(term=term):
            return Negation(operand=_lower_formula(term, ctx))

        case clif.And(terms=terms):
            return Conjunction(operands=tuple(_lower_formula(t, ctx) for t in terms))

        case clif.Or(terms=terms):
            return Disjunction(operands=tuple(_lower_formula(t, ctx) for t in terms))

        case clif.Implies(antecedent=antecedent, consequent=consequent):
            return Implication(
                left=_lower_formula(antecedent, ctx),
                right=_lower_formula(consequent, ctx),
            )

        case clif.Iff(left=left, right=right):
            return Biconditional(
                left=_lower_formula(left, ctx),
                right=_lower_formula(right, ctx),
            )

        case _:
            raise LoweringError(f"Unsupported formula: {node!r}")


BINARY_PREDICATES = {
    # predicate: (constructor, swap_operands)
    "<": (LessThan, False),
    "<=": (LessEqual, False),
    ">": (LessThan, True),
    ">=": (LessEqual, True),
    "!=": (
        lambda left, right: Negation(operand=Equality(left=left, right=right)),
        False,
    ),
}


def _lower_atom(node: clif.Atom, ctx: _Context) -> Formula:
    if not isinstance(node.predicate, clif.Name):
        raise LoweringError("Higher-order predicates are not directly translatable.")

    args = _lower_term_sequence(node.arguments, ctx)

    match node.predicate.name:
        case predicate if predicate in BINARY_PREDICATES:
            constructor, swap_operands = BINARY_PREDICATES[predicate]
            if len(args) != 2:
                raise LoweringError(f"{predicate} requires exactly two arguments, got {len(args)}.")
            if swap_operands:
                return constructor(left=args[1], right=args[0])
            return constructor(left=args[0], right=args[1])
        case "bool" | "boolean":
            if len(args) != 1:
                raise LoweringError("Bool requires exactly one argument, got {len(args)}.")
            if isinstance(args[0], VariableRef):
                ctx.change_sort(args[0].var_id, BoolSort())
                return BooleanConstant(value=True)
            raise LoweringError("Bool requires a variable reference")
        case "int" | "integer":
            if len(args) != 1:
                raise LoweringError("Int requires exactly one argument, got {len(args)}.")
            if isinstance(args[0], VariableRef):
                ctx.change_sort(args[0].var_id, IntSort())
                return BooleanConstant(value=True)
            raise LoweringError("Int requires a variable reference")
        case _:
            raise LoweringError(f"Unsupported predicate: {node.predicate.name}")


def _lower_term(node: clif.Term, ctx: _Context) -> Term:
    match node:
        case clif.Name(name=name):
            if (info := ctx.get_variable(name)) is not None:
                return VariableRef(var_id=info.var_id)

            if name == "true":
                return BooleanConstant(value=True)

            if name == "false":
                return BooleanConstant(value=False)

            try:
                return IntConstant(value=int(name))
            except ValueError:
                pass

            # Unknown symbol - treat as variable
            var_id = ctx.new_variable(name)
            return VariableRef(var_id=var_id)

        case clif.Function():
            return _lower_function(node, ctx)

        case _:
            raise LoweringError(f"Unsupported term: {type(node).__name__}")


def _lower_function(node: clif.Function, ctx: _Context) -> Term:
    if not isinstance(node.operator, clif.Name):
        raise LoweringError("Higher-order function operators are not directly translatable.")

    args = _lower_term_sequence(node.arguments, ctx)

    match node.operator.name:
        case "+":
            return Addition(operands=args)

        case "-":
            if len(args) != 2:
                raise LoweringError("Subtraction requires exactly two operands.")
            return Subtraction(left=args[0], right=args[1])

        case "*":
            return Multiplication(operands=args)

        case "/":
            if len(args) != 2:
                raise LoweringError("Division requires exactly two operands.")
            return Division(left=args[0], right=args[1])

        case _:
            raise LoweringError(f"Function '{node.operator.name}' is not directly translatable.")


def _lower_term_sequence(seq: clif.TermSequence, ctx: _Context) -> tuple[Term, ...]:
    result = []
    for element in seq.elements:
        if isinstance(element, clif.SequenceMarker):
            raise LoweringError("Sequence markers are not directly translatable.")
        result.append(_lower_term(element, ctx))
    return tuple(result)
