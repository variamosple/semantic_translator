from variamos import model, rules, query
import networkx as nx


def transform_request_to_python(
    project_json, rules_json, query_json, selectedModelId
) -> tuple[model.Model, nx.DiGraph, rules.Rules, query.Query, int]:
    # Get the feature model @ /productLines[0]/domainEngineering/models[0]
    dom_models = project_json["productLines"][0]["domainEngineering"]["models"]
    app_models = project_json["productLines"][0]["applicationEngineering"]["models"]
    all_models = [*dom_models, *app_models]
    idx, fm = next(
        filter(
            lambda mod: mod[1]["id"] == selectedModelId,
            enumerate(
                all_models
            ),
        )
    )
    vmos_model = model.Model(**fm)
    vmos_nx_graph = vmos_model.construct_graph()
    vmos_model_rules = rules.Rules(**rules_json)
    vmos_query = query.Query(**query_json)
    return vmos_model, vmos_nx_graph, vmos_model_rules, vmos_query, idx
