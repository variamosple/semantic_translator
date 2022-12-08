from generator import clif_generator
from variamos import model, rules
from grammars.clif import Text, clif_meta_model
from solvers.solver_control import SolverController


def run(project_json, rules_json, solver, dry, selectedModelId):
    """Main application logic entrypoint"""
    # Get the feature model @ /productLines[0]/domainEngineering/models[0]
    idx, fm = next(
        filter(
            lambda mod: mod[1]["id"] == selectedModelId,
            enumerate(
                project_json["productLines"][0]["domainEngineering"]["models"]
            ),
        )
    )
    vmos_model = model.Model(**fm)
    vmos_model_rules = rules.Rules(**rules_json)
    clif_gen = clif_generator.CLIFGenerator(
        rule_set=vmos_model_rules, variamos_model=vmos_model
    )
    clif_str = clif_gen.generate_logic_model()
    clif_mm = clif_meta_model()
    clif_model: Text = clif_mm.model_from_str(clif_str)
    sc = SolverController(solver, clif_model)
    result = sc.solve_n(n_sols=1)
    if not dry:
        sc.update_model(fm, rules_json, result)
    else:
        return f"CLIF/{solver} - SAT OK"
    print(result)  # pyright: ignore
    project_json["productLines"][0]["domainEngineering"]["models"][idx] = fm
    return project_json
