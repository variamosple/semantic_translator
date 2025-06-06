import json
import pytest
pytest.skip(allow_module_level=True)
from variamos import feature_model


@pytest.fixture
def variamos_data():
    with open("json/requests/vmosfmv2.json") as file:
        v_model = json.loads(file.read())
        fm_obj = v_model["productLines"][0]["domainEngineering"]["models"][0]
        return fm_obj


@pytest.fixture
def variamos_bad_data():
    with open("json/requests/vmosfm_non_tree.json") as file:
        v_model = json.loads(file.read())
        fm_obj = v_model["productLines"][0]["domainEngineering"]["models"][0]
        return feature_model.FeatureModel(**fm_obj)


@pytest.fixture
def vmos_model_obj(variamos_data):
    return feature_model.FeatureModel(**variamos_data)


def test_model_name(vmos_model_obj: feature_model.FeatureModel):
    assert vmos_model_obj.name == "Features"


def test_opt_rels(vmos_model_obj: feature_model.FeatureModel):
    assert len(vmos_model_obj.get_optional_relationships()) == 2


def test_bundles(vmos_model_obj: feature_model.FeatureModel):
    assert len(vmos_model_obj.get_bundles()) == 1


def test_graph_construct(vmos_model_obj: feature_model.FeatureModel):
    G = vmos_model_obj.construct_graph()
    assert len(G.nodes.items()) == 10
    assert len(G.edges.items()) == 11


def test_graph_tree_check(vmos_model_obj: feature_model.FeatureModel):
    G = vmos_model_obj.construct_graph()
    assert vmos_model_obj.check_tree_structure(G) is True


def test_graph_tree_check_bad(variamos_bad_data: feature_model.FeatureModel):
    G = variamos_bad_data.construct_graph()
    assert variamos_bad_data.check_tree_structure(G) is False
