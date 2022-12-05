import json
import pytest

from variamos import rules


@pytest.fixture
def rules_data():
    with open("json/fmrulesv4_simple.json") as file:
        rules_json = json.loads(file.read())
        return rules.Rules(**rules_json)


def test_rules_obj(rules_data: rules.Rules):
    assert len(rules_data.element_types) == 3
    assert len(rules_data.relation_reification_types) == 1
