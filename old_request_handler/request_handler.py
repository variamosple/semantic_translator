import json
import copy
import time
from flask import request, jsonify, make_response, Response
from old_request_handler.solvers.results import StatusEnum
from old_request_handler.variamos import model, transform
from old_request_handler.solvers import query_handler
from old_request_handler.utils.exceptions import SolverException
from old_request_handler.utils import enums

def request_handler():
    if request.method == "OPTIONS":
        return _build_cors_preflight_response()
    elif request.method == "POST":
        """Handle a translation request for a given <language>."""
        t0 = time.thread_time_ns()
        content = request.json
        ## Now we need to handle queries that may come from other clients than just vmos
        if "input" in content["data"]: # pyright: ignore
            input = content["data"]["input"] # pyright: ignore
        else:
            input = enums.InputEnum.vmos
        match (input):
            # Case where the request comes from VariaMos
            case enums.InputEnum.vmos:
                selectedModel = content["data"]["modelSelectedId"]  # pyright: ignore
                (
                    model,
                    graph,
                    rules,
                    query,
                    model_idx,
                ) = transform.transform_vmos_request_to_python(
                    project_json=content["data"]["project"],  # pyright: ignore
                    rules_json=content["data"]["rules"],  # pyright: ignore
                    query_json=content["data"]["query"],  # pyright: ignore
                    selectedModelId=selectedModel,
                )
                qh = query_handler.QueryHandler(
                    nx_graph=graph,
                    query_obj=query,
                    translation_rules=rules,
                    input=input,
                )
            case enums.InputEnum.uvl:
                (query, model_str) = transform.transform_uvl_request_to_python(content) # pyright: ignore
                # Case where the request comes from UVL
                # We need to make a large refactor on the i/o side
                # to handle the different types of input languages
                qh = query_handler.QueryHandler(
                    query_obj=query,
                    model_str=model_str,
                    input=input,
                )
                model_idx = None
                model = None
            case _:
                return _corsify_actual_response(
                    jsonify({"data": {"error": "Unknown input type"}})
                )
        try:
            return construct_response(qh, content, model_idx, model, t0)  # type: ignore
        except SolverException as err:
            print(err)
            t1 = time.thread_time_ns()
            total_time = t1 - t0
            return _corsify_actual_response(
                jsonify(
                    {
                        "data": {"error": str(err)},
                        "statistics": {"total_time": total_time},
                    }
                )
            )
    else:
        err_response = make_response(jsonify({"error": "Method not supported"}), 500)
        return _corsify_actual_response(err_response)


def construct_response(
    qh: query_handler.QueryHandler, content, model_idx: int, model: model.Model, t0: int
):
    # TODO: handle the different types of queries in the responses to avoid
    # always updating the model and updating the project JSON
    query_result = qh.run_query()
    if qh.is_dry() or query_result is False:
        # In this case we know the response is a boolean
        t1 = time.thread_time_ns()
        total_time = t1 - t0
        return _corsify_actual_response(
            jsonify(
                {
                    "data": {"content": query_result},
                    "statistics": {**qh.get_statistics(), "total_time": total_time},
                }
            )
        )
    elif (
        not isinstance(query_result, list)
        and query_result.status == StatusEnum.UNSATISFIED  # type: ignore
    ):
        # In this case we know that no solution was foundx
        return _corsify_actual_response(
            jsonify({"data": {"content": False}, "statistics": qh.get_statistics()})
        )
    elif not query_result.solution.single_solution:  # type: ignore
        # In this case we know the response is a list of configurations
        # HACK: FIXME: We will hack together the set of responses by creating a list
        # of project with the different configurations
        # This needs to be better handled in the future
        dom_length = len(
            content["data"]["project"]["productLines"][0]["domainEngineering"]["models"]
        )
        model_copies = []
        project_copies = []
        for sln in query_result.solution.solutions:  # type: ignore
            m_c = model.copy(deep=True)
            m_c.update_selections(sln)
            model_copies.append(m_c)
            p_c = copy.deepcopy(content["data"]["project"])
            p_c["productLines"][0][
                (
                    "domainEngineering"
                    if model_idx < dom_length
                    else "applicationEngineering"
                )
            ]["models"][model_idx % dom_length] = json.loads(m_c.json(by_alias=True))
            project_copies.append(p_c)
        return _corsify_actual_response(
            jsonify(
                {"data": {"content": project_copies}, "statistics": qh.get_statistics()}
            )
        )
    elif query_result.solution.single_solution:  # type: ignore
        # update the model with the new values
        model.update_selections(query_result.solution.solutions[0])  # type: ignore
        # fix the project JSON content
        # get the lenght of the models
        dom_length = len(
            content["data"]["project"]["productLines"][0]["domainEngineering"]["models"]
        )
        model = json.loads(model.json(by_alias=True))
        if model_idx < dom_length:
            content["data"]["project"]["productLines"][0]["domainEngineering"][
                "models"
            ][model_idx] = model
        else:
            content["data"]["project"]["productLines"][0]["applicationEngineering"][
                "applications"
            ][0]["models"][model_idx - dom_length] = model
        return _corsify_actual_response(
            jsonify(
                {
                    "data": {"content": content["data"]["project"]},
                    "statistics": qh.get_statistics(),
                }
            )
        )
    else:
        raise RuntimeError("Unknown query result")


def _build_cors_preflight_response():
    response = make_response()
    response.headers.add("Access-Control-Allow-Origin", "*")
    response.headers.add("Access-Control-Allow-Headers", "*")
    response.headers.add("Access-Control-Allow-Methods", "*")
    return response


def _corsify_actual_response(response) -> Response:
    response.headers.add("Access-Control-Allow-Origin", "*")
    return response
