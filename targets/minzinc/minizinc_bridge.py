from dataclasses import dataclass
from minizinc import Instance, Model, Solver
from targets.minzinc.minizinc_model import MZNModel
from targets.solver_model import SolverModel
from utils.exceptions import SolverException
from variamos import rules as rls
from variamos import model as mdl
from variamos import query


@dataclass
class MiniZincBridge:
    """
    This class is a bridge between the MiniZinc application and the rest of the
    application. It is used for running queries and solving models. It allows 
    for both optimization and satisfaction problems, though is limited to single
    solutions for optimization problems.
    """
    def solve(self, model: SolverModel, n_sols: int = 1):
        if not isinstance(model, MZNModel):
            raise TypeError("Must be a mzn model")
        constraints = model.generate_program()
        # print(constraints)
        gecode = Solver.lookup("gecode")
        mzn_model = Model()
        mzn_model.add_string("\n".join(constraints) + "\n" + "solve satisfy;")
        print("\n".join(constraints) + "\n" + "solve satisfy;")
        instance = Instance(gecode, mzn_model)
        result = instance.solve(nr_solutions=n_sols)
        # if not result.status.has_solution():
        #     raise SolverException("CLIF/MZN - Model is UNSAT")
        return result

    def optimize(
        self,
        model: SolverModel,
        objective: str,
        direction: query.OptimizationDirectionEnum,
        n_sols: int = 1,
    ):
        if not isinstance(model, MZNModel):
            raise TypeError("Must be a mzn model")
        constraints = model.generate_program()
        gecode = Solver.lookup("gecode")
        mzn_model = Model()
        direction_str = (
            "minimize"
            if direction == query.OptimizationDirectionEnum.min
            else "maximize"
        )
        mzn_model.add_string(
            "\n".join(constraints)
            + "\n"
            + f"solve {direction_str} {objective};"
        )
        print(
            "\n".join(constraints)
            + "\n"
            + f"solve {direction_str} {objective};"
        )
        instance = Instance(gecode, mzn_model)
        # We don't yet have support for multiple optimal solutions
        # We need to pass None if we have a single solution
        # and the number of solutions if we have multiple solutions
        n_sol_arg = None if n_sols == 1 else n_sols
        result = instance.solve(nr_solutions=n_sol_arg)
        # We won't except out if there solution is not found
        # if not result.status.has_solution():
        #     raise SolverException("CLIF/MZN - Model is UNSAT")
        return result

    def update_model(self, model: mdl.Model, rules: rls.Rules, result):
        for e in model.elements:
            if e.type in rules.element_types and e.properties[1][
                "value"
            ] not in [
                "Selected",
                "Unselected",
            ]:  # noqa
                e.properties[1]["value"] = (
                    "SelectedForced"
                    if result[0, "UUID_" + str(e.id).replace("-", "_")] == 1
                    else "UnselectedForced"
                )
