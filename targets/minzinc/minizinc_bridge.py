from dataclasses import dataclass
from minizinc import Instance, Model, Solver
from targets.minzinc.minizinc_model import MZNModel
from targets.solver_model import SolverModel
from utils.exceptions import SolverException
from variamos import rules as rls


@dataclass
class MiniZincBridge:
    def solve(self, model: SolverModel, n_sols: int = 1):
        if not isinstance(model, MZNModel):
            raise TypeError("Must be a mzn model")
        constraints = model.generate_program()
        # print(constraints)
        gecode = Solver.lookup("gecode")
        mzn_model = Model()
        mzn_model.add_string("\n".join(constraints) + "\n" + "solve satisfy;")
        # print("\n".join(constraints) + "\n" + "solve satisfy;")
        instance = Instance(gecode, mzn_model)
        result = instance.solve(nr_solutions=n_sols)
        if not result.status.has_solution():
            raise SolverException("CLIF/MZN - Model is UNSAT")
        return result

    def update_model(self, model, rules: rls.Rules, result):
        for e in model["elements"]:
            if e["type"] in rules.element_types and e["properties"][1][
                "value"
            ] not in [
                "Selected",
                "Unselected",
            ]:  # noqa
                e["properties"][1]["value"] = (
                    "SelectedForced"
                    if result[0, "UUID_" + str(e["id"]).replace("-", "_")] == 1
                    else "UnselectedForced"
                )
