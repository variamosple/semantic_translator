import z3

from ...result import Result, SATResult, SolveResult


class Z3Executor:
    def check_satisfiability(self, solver_model: z3.Solver) -> Result:
        satisfiable = solver_model.check()
        if satisfiable == z3.unknown:
            raise RuntimeError("Solver returned unknown")
        return SATResult(satisfiable=satisfiable == z3.sat)

    def solve(self, solver_model: z3.Solver, limit: int, variable_names: dict[str, str]) -> Result:
        if limit > 1:
            raise NotImplementedError("Multiple solutions are not supported yet")
        satisfiable = solver_model.check()
        if satisfiable == z3.unknown:
            raise RuntimeError("Solver returned unknown")
        if satisfiable == z3.sat:
            solver_solution = solver_model.model()
            solution = self._solution_from_model(solver_solution, variable_names)
            return SolveResult(solutions=[solution])
        return SolveResult(solutions=[])

    def _solution_from_model(self, model: z3.ModelRef, variable_names: dict[str, str]) -> dict[str, object]:
        solution = {}
        for d in model.decls():
            value = model[d]
            match value:
                case z3.BoolRef():
                    solution[variable_names[d.name()]] = z3.is_true(value)
                case z3.IntNumRef():
                    solution[variable_names[d.name()]] = value.as_long()
                case _:
                    raise ValueError(f"Unsupported Z3 type in solution: {type(value)}")
        return solution
