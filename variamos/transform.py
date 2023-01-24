from variamos import model, rules, query
import networkx as nx


def transform_request_to_python(
    project_json, rules_json, query_json, selectedModelId
) -> tuple[model.Model, nx.DiGraph, rules.Rules, query.Query, int]:
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
    return vmos_model, vmos_nx_graph, vmos_model_rules, vmos_query, idx
