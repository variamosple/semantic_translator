import z3

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
from variability_solver.ir.variables import BoolSort, IntSort, Sort, Variable


class Z3Compiler:
    def __init__(self) -> None:
        self.variables: dict[int, z3.ExprRef] = {}
        
    def get_variable_names(self, model: Model) -> dict[str, str]:
        return {f"var_{variable.var_id}": variable.name for variable in model.variables}

    def compile_model(self, model: Model) -> z3.Solver:
        """
        Compile the given model into a Z3 representation.

        Args:
            model: The model to compile

        Returns:
            The compiled model
        """

        self.variables.clear()

        for variable in model.variables:
            self.variables[variable.var_id] = self._compile_variable(variable)

        solver = z3.Solver()
        for constraint in model.constraints:
            solver.add(self._compile_formula(constraint))
        return solver

    def _resolve_sort(self, sort: Sort) -> z3.SortRef:
        if isinstance(sort, BoolSort):
            return z3.BoolSort()
        if isinstance(sort, IntSort):
            return z3.IntSort()
        raise ValueError(f"Unsupported sort: {sort}")

    def _compile_variable(self, variable: Variable) -> z3.ExprRef:
        sort = self._resolve_sort(variable.sort)
        return z3.Const(f"var_{variable.var_id}", sort)

    def _compile_term(self, term: Term) -> z3.ExprRef:
        match term:
            case VariableRef(var_id=var_id):
                result = self.variables[var_id]
            case BooleanConstant(value=value):
                result = z3.BoolVal(value)
            case IntConstant(value=value):
                result = z3.IntVal(value)
            case Addition(operands=operands):
                result = z3.Sum(*[self._compile_term(operand) for operand in operands])
            case Subtraction(left=left, right=right):
                compiled_left = self._compile_term(left)
                compiled_right = self._compile_term(right)
                assert isinstance(compiled_left, z3.ArithRef), (
                    f"Expected ArithRef, got {type(compiled_left)}"
                )
                assert isinstance(compiled_right, z3.ArithRef), (
                    f"Expected ArithRef, got {type(compiled_right)}"
                )
                result = compiled_left - compiled_right
            case Multiplication(operands=operands):
                result = z3.Product(
                    *[self._compile_term(operand) for operand in operands]
                )
            case Division(left=left, right=right):
                compiled_left = self._compile_term(left)
                compiled_right = self._compile_term(right)
                assert isinstance(compiled_left, z3.ArithRef), (
                    f"Expected ArithRef, got {type(compiled_left)}"
                )
                assert isinstance(compiled_right, z3.ArithRef), (
                    f"Expected ArithRef, got {type(compiled_right)}"
                )
                result = compiled_left / compiled_right
            case _:
                raise ValueError(f"Unsupported term: {term}")
        assert isinstance(result, z3.ExprRef), f"Expected ExprRef, got {type(result)}"
        return result

    def _compile_formula(self, formula: Formula) -> z3.BoolRef:
        match formula:
            case BooleanConstant(value=value):
                result = z3.BoolVal(value)
            case Negation(operand=operand):
                result = z3.Not(self._compile_formula(operand))
            case Disjunction(operands=operands):
                result = z3.Or(
                    *[self._compile_formula(operand) for operand in operands]
                )
            case Conjunction(operands=operands):
                result = z3.And(
                    *[self._compile_formula(operand) for operand in operands]
                )
            case Implication(left=left, right=right):
                result = z3.Implies(
                    self._compile_formula(left), self._compile_formula(right)
                )
            case Biconditional(left=left, right=right):
                result = self._compile_formula(left) == self._compile_formula(right)
            case Equality(left=left, right=right):
                result = self._compile_term(left) == self._compile_term(right)
            case LessThan(left=left, right=right):
                compiled_left = self._compile_term(left)
                compiled_right = self._compile_term(right)
                assert isinstance(compiled_left, z3.ArithRef), (
                    f"Expected ArithRef, got {type(compiled_left)}"
                )
                assert isinstance(compiled_right, z3.ArithRef), (
                    f"Expected ArithRef, got {type(compiled_right)}"
                )
                result = compiled_left < compiled_right
            case LessEqual(left=left, right=right):
                compiled_left = self._compile_term(left)
                compiled_right = self._compile_term(right)
                assert isinstance(compiled_left, z3.ArithRef), (
                    f"Expected ArithRef, got {type(compiled_left)}"
                )
                assert isinstance(compiled_right, z3.ArithRef), (
                    f"Expected ArithRef, got {type(compiled_right)}"
                )
                result = compiled_left <= compiled_right
            case _:
                raise ValueError(f"Unsupported formula: {formula}")
        assert isinstance(result, z3.BoolRef), f"Expected BoolRef, got {type(result)}"
        return result
