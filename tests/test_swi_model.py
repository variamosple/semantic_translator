import pytest
from grammars import clif
from textx.metamodel import TextXMetaModel
from targets.swi.swi_model import (
    clif_to_SWI_objects,
    ArithmeticPredicate,
    SWIFDVarDomainDec,
    SWIFDConstraint,
)


@pytest.fixture
def meta() -> TextXMetaModel:
    return clif.clif_meta_model(debug=True)


def test_simple_var_decl(meta: TextXMetaModel):
    test_strs = [
        "(model",
        "(bool UUID_69784178_c589_4447_bbe5_7b51b97f4918)",
        ")",
    ]
    mod: clif.Text = meta.model_from_str("\n".join(test_strs))
    swi = clif_to_SWI_objects(mod)
    assert len(swi.constraints) == 1
    assert isinstance(swi.constraints[0], SWIFDVarDomainDec)
    assert swi.constraints[0].bounds == (0, 1)
    assert swi.constraints[0].terms == [
        "UUID_69784178_c589_4447_bbe5_7b51b97f4918"
    ]


def test_simple_var_decl_mzn_gen(meta: TextXMetaModel):
    test_strs = [
        "(model",
        "(bool UUID_69784178_c589_4447_bbe5_7b51b97f4918)",
        ")",
    ]
    mod: clif.Text = meta.model_from_str("\n".join(test_strs))
    swi = clif_to_SWI_objects(mod)
    assert len(swi.constraints) == 1
    assert (
        swi.constraints[0].to_string()
        == "UUID_69784178_c589_4447_bbe5_7b51b97f4918 in 0..1"
    )


def test_simple_equation(meta: TextXMetaModel):
    test_strs = [
        "(model",
        "(= UUID_69784178_c589_4447_bbe5_7b51b97f4918 1)",
        ")",
    ]
    mod: clif.Text = meta.model_from_str("\n".join(test_strs))
    swi = clif_to_SWI_objects(mod)
    assert len(swi.constraints) == 1
    assert isinstance((cons := swi.constraints[0]), SWIFDConstraint)
    assert cons.arithmetic_predicate == ArithmeticPredicate.EQ
    assert cons.terms is not None
    assert len(cons.terms) == 2
    assert cons.terms[0] == "UUID_69784178_c589_4447_bbe5_7b51b97f4918"
    assert cons.terms[1] == 1


def test_conjunction_declaration(meta: TextXMetaModel):
    test_strs = [
        "(model",
        "(and (bool UUID_69784178_c589_4447_bbe5_7b51b97f4918) (= UUID_69784178_c589_4447_bbe5_7b51b97f4918 1))",  # noqa
        ")",
    ]
    mod: clif.Text = meta.model_from_str("\n".join(test_strs))
    swi = clif_to_SWI_objects(mod)
    assert len(swi.constraints) == 2
    assert isinstance((c0 := swi.constraints[0]), SWIFDVarDomainDec)
    # Decl Part
    assert c0.terms is not None
    assert len(c0.terms) == 1
    assert c0.bounds == (0, 1)
    assert c0.terms[0] == "UUID_69784178_c589_4447_bbe5_7b51b97f4918"
    # Constraint part
    assert isinstance((c1 := swi.constraints[1]), SWIFDConstraint)
    assert c1.arithmetic_predicate == ArithmeticPredicate.EQ
    assert c1.terms is not None
    assert len(c1.terms) == 2
    assert c1.terms[0] == "UUID_69784178_c589_4447_bbe5_7b51b97f4918"
    assert c1.terms[1] == 1


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
    swi = clif_to_SWI_objects(mod)
    assert len(swi.constraints) == 2
    assert all(
        map(
            lambda c: isinstance(c, SWIFDConstraint)
            and c.arithmetic_predicate == ArithmeticPredicate.LTE
            and c.terms is not None
            and all(map(lambda t: isinstance(t, clif.ArithmeticExpr), c.terms)),
            swi.constraints,
        )
    )


def test_complex_constraint_mzn_generation(meta: TextXMetaModel):
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
    swi = clif_to_SWI_objects(mod)
    assert (
        swi.constraints[0].to_string()
        == "UUID_bf3ab018_6304_4e84_a11f_80f3f5d1d80f * 1 #=< UUID_43634fef_d816_4cc4_bbde_02cb7865afef + UUID_87b866ef_e358_4797_829c_d3fcac43a21f"  # noqa
    )
    assert (
        swi.constraints[1].to_string()
        == "UUID_43634fef_d816_4cc4_bbde_02cb7865afef + UUID_87b866ef_e358_4797_829c_d3fcac43a21f #=< UUID_bf3ab018_6304_4e84_a11f_80f3f5d1d80f * 2"  # noqa
    )
