import json
import pytest

from variamos import model, rules
from generator import clif_generator

@pytest.fixture
def variamos_data():
    with open("json/vmosfmv2.json") as file:
        v_model = json.loads(file.read())
        fm_obj = v_model["productLines"][0]["domainEngineering"]["models"][0]
        return fm_obj


@pytest.fixture
def rules_data():
    with open("json/fmrulesv4_simple.json") as file:
        rules_json = json.loads(file.read())
        return rules.Rules(**rules_json)


@pytest.fixture
def vmos_model_obj(variamos_data):
    return model.Model(**variamos_data)


def test_model_generation(vmos_model_obj: model.Model, rules_data: rules.Rules):
    clif_gen = clif_generator.CLIFGenerator(rules_data, vmos_model_obj)
    clif = clif_gen.generate_logic_model()
