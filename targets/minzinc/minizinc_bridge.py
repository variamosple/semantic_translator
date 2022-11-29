from minizinc import Instance, Model, Solver
from grammars import clif


class MiniZincBridge:
    def minizinc_solve(self, constraints, n_sols: int = 1):
        # print(constraints)
        gecode = Solver.lookup("gecode")
        mzn_model = Model()
        mzn_model.add_string("\n".join(constraints) + "\n" + "solve satisfy;")
        print("\n".join(constraints) + "\n" + "solve satisfy;")
        instance = Instance(gecode, mzn_model)
        return instance.solve(nr_solutions=n_sols)

    def minizinc_update_model(self, model, rules, result):
        for e in model["elements"]:
            if e["type"] in rules["elementTypes"] and e["properties"][1][
                "value"
            ] not in [
                "Selected",
                "Unselected",
            ]:  # noqa
                e["properties"][1]["value"] = (
                    "SelectedForced"
                    if result["UUID_" + str(e["id"]).replace("-", "_")] == 1
                    else "UnselectedForced"
                )

    def handle_verifications(
        self, verif_spec: clif.Text, verif_model: clif.Text
    ):
        pass
