import json
import time
import pytest
pytest.skip(allow_module_level=True)
from variamos import model, rules
from generator import clif_generator
from grammars import clif
from targets.minzinc import minizinc_model
from solvers import solver_control


@pytest.fixture
def variamos_data():
    with open("json/requests/vmosfmv2.json") as file:
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
    return model.Model(**variamos_data).construct_graph()


@pytest.mark.skip(
    reason="This is a benchmark test and does not reflect the current API"
)
def test_model_generation(vmos_model_obj: model.Model, rules_data: rules.Rules):
    t0 = time.perf_counter()
    clif_gen = clif_generator.CLIFGenerator(rules_data, vmos_model_obj)
    clif_str = clif_gen.generate_logic_model()
    t1 = time.perf_counter()
    clif_ast: clif.Text = clif.clif_meta_model(False).model_from_str(clif_str)
    t2 = time.perf_counter()
    mzn_model = minizinc_model.clif_to_MZN(clif_model=clif_ast)
    t3 = time.perf_counter()
    _ = mzn_model.generate_program()
    t4 = time.perf_counter()
    assert len(clif_ast.constructions.sentences) == 18
    print("T Clif Generation", round((t1 - t0) * 1000, 3), "ms")
    print("T Clif Parsing into AST", round((t2 - t1) * 1000, 3), "ms")
    print(
        "T Generate Minizinc Model from AST", round((t3 - t2) * 1000, 3), "ms"
    )
    print("T Generate Minizinc strings", round((t4 - t3) * 1000, 3), "ms")
    print("Total roundtrip T", round((t4 - t0) * 1000, 3), "ms")


@pytest.mark.skip(
    reason="This is a benchmark test and does not reflect the current API"
)
def test_model_roundtrip(vmos_model_obj: model.Model, rules_data: rules.Rules):
    t0 = time.perf_counter()
    clif_gen = clif_generator.CLIFGenerator(rules_data, vmos_model_obj)
    clif_str = clif_gen.generate_logic_model()
    t1 = time.perf_counter()
    clif_ast: clif.Text = clif.clif_meta_model(False).model_from_str(clif_str)
    t2 = time.perf_counter()
    ctlr = solver_control.SolverController(
        target_lang="minizinc",  # Could also be SWI where T is aprox 2x
        clif_model=clif_ast,
    )
    t3 = time.perf_counter()
    sat = ctlr.sat()
    t4 = time.perf_counter()
    assert sat
    print("T Clif Generation", round((t1 - t0) * 1000, 3), "ms")
    print("T Clif Parsing into AST", round((t2 - t1) * 1000, 3), "ms")
    print("Create Controller (Gen MZN Model)", round((t3 - t2) * 1000, 3), "ms")
    print("T Run SAT Check", round((t4 - t3) * 1000, 3), "ms")
    print("Total roundtrip T", round((t4 - t0) * 1000, 3), "ms")
