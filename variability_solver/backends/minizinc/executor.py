import minizinc

from ...result import Result, SATResult, SolveResult


class MinizincExecutor:
    def check_satisfiability(self, solver_name: str, model_str: str) -> Result:
        solver = minizinc.Solver.lookup(solver_name)
        model = minizinc.Model()
        model.add_string(model_str)
        model.add_string("solve satisfy;")
        instance = minizinc.Instance(solver, model)
        result = instance.solve()
        match result.status:
            case minizinc.Status.SATISFIED:
                return SATResult(satisfiable=True)
            case minizinc.Status.UNSATISFIABLE:
                return SATResult(satisfiable=False)
            case _:
                raise RuntimeError(f"Unexpected status: {result.status}")

    def solve(
        self,
        solver_name: str,
        model_str: str,
        limit: int,
        variable_names: dict[str, str],
    ) -> Result:
        solver = minizinc.Solver.lookup(solver_name)
        model = minizinc.Model()
        model.add_string(model_str)
        model.add_string("solve satisfy;")
        instance = minizinc.Instance(solver, model)
        result = instance.solve(nr_solutions=limit)
        match result.status:
            case minizinc.Status.SATISFIED | minizinc.Status.ALL_SOLUTIONS:
                return SolveResult(
                    solutions=self._convert_solutions(result, variable_names),
                )
            case minizinc.Status.UNSATISFIABLE:
                return SolveResult(solutions=[])
            case _:
                raise RuntimeError(f"Unexpected status: {result.status}")

    def _convert_solutions(
        self, result: minizinc.Result, variable_names: dict[str, str]
    ) -> list[dict[str, object]]:
        return [
            {
                variable_names[var_name]: result[solution_idx, var_name]
                for var_name in variable_names
            }
            for solution_idx in range(len(result))
        ]
