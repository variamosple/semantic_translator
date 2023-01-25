import json
import pytest

from variamos import query


@pytest.fixture
def query_data():
    with open("json/query_dead_optional.json") as file:
        return json.loads(file.read())


def test_query_parse(query_data):
    q = query.Query(**query_data)
    assert q.operation is query.OperationEnum.sat
    assert q.iterate_over is not None
    assert len(q.iterate_over) == 2
