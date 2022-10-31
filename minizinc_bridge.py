from minizinc import Instance, Model, Solver

def minizinc_solve(constraints):
    # print(constraints)
    gecode = Solver.lookup("gecode")
    mzn_model = Model()
    mzn_model.add_string("\n".join(constraints) + "\n" + "solve satisfy;")
    print("\n".join(constraints) + "\n" + "solve satisfy;")
    instance = Instance(gecode, mzn_model)
    return instance.solve()

def minizinc_update_model(model, rules, result):
    for e in model["elements"]:
        if e["type"] in rules["elementTypes"] and e["properties"][1]["value"] not in ['Selected', 'Unselected']:
            e["properties"][1]["value"] = "SelectedForced" if result["UUID_" +  str(e["id"]).replace("-","_")] == 1 else  "UnselectedForced"
