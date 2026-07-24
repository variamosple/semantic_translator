from ...ir.constraints import (
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
from ...ir.model import Model
from ...ir.variables import BoolSort, IntSort, Sort, Variable


class MinizincCompiler:
    def compile_model(self, model: Model) -> str:
        """
        Compile the given model into a Minizinc representation.

        Args:
            model: The model to compile

        Returns:
            The compiled model
        """
        mzn_model = ""
        for variable in model.variables:
            mzn_model += self._compile_variable(variable) + "\n"
        for formula in model.constraints:
            mzn_model += "constraint " + self._compile_formula(formula) + ";\n"
        return mzn_model

    def get_variable_names(self, model: Model) -> dict[str, str]:
        return {
            self._resolve_variable_name(variable.var_id): variable.name
            for variable in model.variables
        }

    def _resolve_variable_sort(self, sort: Sort) -> str:
        match sort:
            case IntSort():
                return "int"
            case BoolSort():
                return "bool"
            case _:
                raise NotImplementedError(f"Unsupported sort: {sort}")

    def _resolve_variable_name(self, var_id: int) -> str:
        return f"var_{var_id}"

    def _compile_variable(self, variable: Variable) -> str:
        sort = self._resolve_variable_sort(variable.sort)
        name = self._resolve_variable_name(variable.var_id)
        return f"var {sort} : {name};"

    def _compile_formula(self, formula: Formula) -> str:
        match formula:
            case BooleanConstant(value=value):
                return "true" if value else "false"
            case Negation(operand=operand):
                return f"(not {self._compile_formula(operand)})"
            case Disjunction(operands=operands):
                return (
                    "(" + " \\/ ".join(self._compile_formula(operand) for operand in operands) + ")"
                )
            case Conjunction(operands=operands):
                return (
                    "(" + " /\\ ".join(self._compile_formula(operand) for operand in operands) + ")"
                )
            case Implication(left=left, right=right):
                return f"({self._compile_formula(left)} -> {self._compile_formula(right)})"
            case Biconditional(left=left, right=right):
                return f"({self._compile_formula(left)} <-> {self._compile_formula(right)})"
            case Equality(left=left, right=right):
                return f"({self._compile_term(left)} == {self._compile_term(right)})"
            case LessThan(left=left, right=right):
                return f"({self._compile_term(left)} < {self._compile_term(right)})"
            case LessEqual(left=left, right=right):
                return f"({self._compile_term(left)} <= {self._compile_term(right)})"
            case _:
                raise NotImplementedError(f"Unsupported formula: {formula}")

    def _compile_term(self, term: Term) -> str:
        match term:
            case VariableRef(var_id=var_id):
                return self._resolve_variable_name(var_id)
            case BooleanConstant(value=value):
                return "true" if value else "false"
            case IntConstant(value=value):
                return str(value)
            case Addition(operands=operands):
                if len(operands) == 0:
                    return "0"
                return "(" + " + ".join(self._compile_term(operand) for operand in operands) + ")"
            case Subtraction(left=left, right=right):
                return f"({self._compile_term(left)} - {self._compile_term(right)})"
            case Multiplication(operands=operands):
                if len(operands) == 0:
                    return "1"
                return "(" + " * ".join(self._compile_term(operand) for operand in operands) + ")"
            case Division(left=left, right=right):
                return f"({self._compile_term(left)} / {self._compile_term(right)})"
            case _:
                raise NotImplementedError(f"Unsupported term: {term}")
