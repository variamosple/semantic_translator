from generator import clif_generator
from variamos import model, rules, query
from grammars.clif import Text, clif_meta_model
from solvers import query_handler


def run(project_json, rules_json, query_json, selectedModelId):
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
    vmos_nx_graph = vmos_model.construct_graph()
    vmos_model_rules = rules.Rules(**rules_json)
    vmos_query = query.Query(**query_json)
    clif_gen = clif_generator.CLIFGenerator(
        rule_set=vmos_model_rules,
        variamos_model=vmos_model,
        variamos_graph=vmos_nx_graph,
    )
    clif_str = clif_gen.generate_logic_model()
    clif_mm = clif_meta_model()
    clif_model: Text = clif_mm.model_from_str(clif_str)
    qh = query_handler.QueryHandler(
        model_obj=vmos_model,
        query_obj=vmos_query,
        clif_model=clif_model,
        nx_graph=vmos_nx_graph,
        translation_rules=vmos_model_rules,
    )
    # print("result", qh.run_query(project_json, fm))
    # project_json["productLines"][0]["domainEngineering"]["models"][idx] = fm
    return qh.run_query(project_json=project_json, idx=idx, feature_model=fm)
