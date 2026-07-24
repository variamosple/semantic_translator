from variability_solver.ir.model import Model
from variability_solver.options import Options
from variability_solver.query import Operation, Query
from variability_solver.result import Result

from ..backend import Backend
from .compiler import Z3Compiler
from .executor import Z3Executor


class Z3Backend(Backend):
    @property
    def name(self) -> str:
        return "z3"

    @property
    def description(self) -> str:
        return "Z3 solver (currently only provide an only solution)"

    def execute(self, model: Model, query: Query, options: Options) -> Result:
        solver = Z3Compiler().compile_model(model)
        variable_names = Z3Compiler().get_variable_names(model)

        if query.iteration_rules:
            raise NotImplementedError("Iteration rules are not supported yet")

        match query.operation:
            case Operation.CHECK_SATISFIABILITY:
                return Z3Executor().check_satisfiability(solver)
            case Operation.SOLVE:
                limit = query.limit or 1
                return Z3Executor().solve(solver, limit, variable_names)
            case _:
                raise NotImplementedError(f"The operation {query.operation} is not supported")
