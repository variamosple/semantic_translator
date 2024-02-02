import pytest
pytest.skip(allow_module_level=True)
from grammars import clif
from textx.metamodel import TextXMetaModel
from targets.swi.swi_model import clif_to_SWI
from targets.z3.z3_model import Z3Model
from targets.z3.z3_bridge import Z3Bridge
from targets.solver_model import ArithmeticPredicate, CSPConstraint, SolverModel


@pytest.fixture
def meta() -> TextXMetaModel:
    return clif.clif_meta_model(debug=True)


def test_complex_constraint(meta: TextXMetaModel):
    test_strs = [
        "(model",
        (
            "(and (=< (UUID_bf3ab018_6304_4e84_a11f_80f3f5d1d80f * 1) "
            "(UUID_43634fef_d816_4cc4_bbde_02cb7865afef + "
            "UUID_87b866ef_e358_4797_829c_d3fcac43a21f)) (=< "
            "(UUID_43634fef_d816_4cc4_bbde_02cb7865afef + "
            "UUID_87b866ef_e358_4797_829c_d3fcac43a21f) "
            "(UUID_bf3ab018_6304_4e84_a11f_80f3f5d1d80f * 2)) )"
        ),  # noqa
        ")",
    ]
    mod: clif.Text = meta.model_from_str("\n".join(test_strs))
    gen_csp = SolverModel.from_clif_text(mod)
    z3model = Z3Model.from_gen_csp(gen_csp)
    assert len(gen_csp.constraints) == 2
    assert all(
        map(
            lambda c: isinstance(c, CSPConstraint)
            and c.arithmetic_predicate == ArithmeticPredicate.LTE
            and c.terms is not None
            and all(map(lambda t: isinstance(t, clif.ArithmeticExpr), c.terms)),
            gen_csp.constraints,
        )
    )


def test_enum_decode(meta: TextXMetaModel):
    test_strs = [
        "(model",
        (
            "(enum (Low High) UUID_5fc5afd3_8754_4e53_b004_93c698587d7a)"
            "(= UUID_5fc5afd3_8754_4e53_b004_93c698587d7a Low)"
            "(int (0 4) UUID_2656a41e_ff9e_4c4c_a06a_d57aa82b6a9f)"
            "(and (bool UUID_452b652c_b763_4542_98c2_7791da3352ae) "
            "(iff (= UUID_452b652c_b763_4542_98c2_7791da3352ae 1) "
            "(if (= UUID_5fc5afd3_8754_4e53_b004_93c698587d7a Low) "
            "(and (= UUID_2656a41e_ff9e_4c4c_a06a_d57aa82b6a9f 4)) ) ) )"
        ),  # noqa
        ")",  # noqa
    ]
    mod: clif.Text = meta.model_from_str("\n".join(test_strs))
    gen_csp = SolverModel.from_clif_text(mod)
    z3model = Z3Model.from_gen_csp(gen_csp)
    z3bridge = Z3Bridge()
    z3sol = z3bridge.solve(z3model)
    print(z3sol)
