from abc import ABC, abstractmethod
from enum import StrEnum

from variability_solver.backends.minizinc.executor import MinizincExecutor

from ...ir.model import Model
from ...options import Options
from ...query import Operation, Query
from ...result import Result
from ..backend import Backend
from .compiler import MinizincCompiler


class _Solver(StrEnum):
    GECODE = "gecode"


class MinizincBackend(Backend, ABC):
    @property
    @abstractmethod
    def _solver(self) -> _Solver:
        pass

    @property
    def name(self) -> str:
        return self._solver

    @property
    def description(self) -> str:
        return f"{self._solver.capitalize()} solver using MiniZinc API"

    def execute(self, model: Model, query: Query, options: Options) -> Result:
        solver_model = MinizincCompiler().compile_model(model)
        variable_names = MinizincCompiler().get_variable_names(model)
        match query.operation:
            case Operation.CHECK_SATISFIABILITY:
                return MinizincExecutor().check_satisfiability(self._solver, solver_model)
            case Operation.SOLVE:
                return MinizincExecutor().solve(
                    self._solver, solver_model, query.limit or 1, variable_names
                )
            case _:
                raise NotImplementedError(
                    f"Operation {query.operation} is not supported by MiniZinc backend"
                )


class GecodeBackend(MinizincBackend):
    @property
    def _solver(self) -> _Solver:
        return _Solver.GECODE
