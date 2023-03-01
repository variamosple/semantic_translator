import json
import pytest
import os
from variamos import query


# create fixtures returning Query objects from the json files in the
# json/queries directory
@pytest.fixture(scope="module")
def query_obj(request) -> query.Query:
    with open(os.path.join("json", "queries", request.param)) as f:
        return query.Query(**json.load(f))


# test function for query_sat.json
@pytest.mark.parametrize("query_obj", ["query_sat.json"], indirect=True)
def test_query_sat(query_obj: query.Query):
    assert query_obj.operation == query.OperationEnum.sat
    assert query_obj.solver == query.enums.TargetLang.minizinc
    assert query_obj.iterate_over is None
    assert query_obj.operation_n is None


# test function for query_config.json
@pytest.mark.parametrize("query_obj", ["query_config.json"], indirect=True)
def test_query_solve(query_obj: query.Query):
    assert query_obj.operation == query.OperationEnum.solve
    assert query_obj.solver == query.enums.TargetLang.minizinc
    assert query_obj.iterate_over is None
    assert query_obj.operation_n is None


# test function for query_config_n.json
@pytest.mark.parametrize("query_obj", ["query_config_n.json"], indirect=True)
def test_query_nsolve(query_obj: query.Query):
    assert query_obj.operation == query.OperationEnum.nsolve
    assert query_obj.solver == query.enums.TargetLang.minizinc
    assert query_obj.iterate_over is None
    assert query_obj.operation_n == 10


# test function for query_dead_feat.json
@pytest.mark.parametrize("query_obj", ["query_dead_feat.json"], indirect=True)
def test_query_dead_feat(query_obj: query.Query):
    assert query_obj.operation == query.OperationEnum.sat
    assert query_obj.solver == query.enums.TargetLang.minizinc
    assert query_obj.iterate_over is not None
    assert len(query_obj.iterate_over) == 1
    assert query_obj.iterate_over[0].model_object == "element"
    assert query_obj.iterate_over[0].relationship_element == "target"
    assert query_obj.iterate_over[0].object_type == [
        "AbstractFeature",
        "ConcreteFeature",
    ]
    assert query_obj.iterate_over[0].with_value == 1
    assert query_obj.operation_n is None


# create test function for query_dead_optional.json
@pytest.mark.parametrize(
    "query_obj", ["query_dead_optional.json"], indirect=True
)
def test_query_dead_optional(query_obj: query.Query):
    assert query_obj.operation == query.OperationEnum.sat
    assert query_obj.solver == query.enums.TargetLang.minizinc
    assert query_obj.iterate_over is not None
    assert len(query_obj.iterate_over) == 2
    assert query_obj.iterate_over[0].model_object == "relationship_element"
    assert query_obj.iterate_over[0].relationship_element == "target"
    assert query_obj.iterate_over[0].object_type == ["Optional"]
    assert query_obj.iterate_over[0].with_value == 0
    assert query_obj.iterate_over[1].model_object == "reified"
    assert query_obj.iterate_over[1].relationship_element == "target"
    assert query_obj.iterate_over[1].object_type == ["Range", "Xor", "Or"]
    assert query_obj.operation_n is None
