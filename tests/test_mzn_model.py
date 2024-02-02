import pytest
pytest.skip(allow_module_level=True)
from grammars import clif
from textx.metamodel import TextXMetaModel
from targets.minzinc.minizinc_model import (
    clif_to_MZN,
    ArithmeticPredicate,
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
    mzn = clif_to_MZN(mod)
    assert len(mzn.var_decls) == 1
    assert (
        mzn.var_decls["UUID_69784178_c589_4447_bbe5_7b51b97f4918"].type
        == "bool"
    )
    assert (
        mzn.var_decls["UUID_69784178_c589_4447_bbe5_7b51b97f4918"].var
        == "UUID_69784178_c589_4447_bbe5_7b51b97f4918"
    )


def test_simple_var_decl_mzn_gen(meta: TextXMetaModel):
    test_strs = [
        "(model",
        "(bool UUID_69784178_c589_4447_bbe5_7b51b97f4918)",
        ")",
    ]
    mod: clif.Text = meta.model_from_str("\n".join(test_strs))
    mzn = clif_to_MZN(mod)
    assert len(mzn.var_decls) == 1
    assert (
        mzn.var_decls["UUID_69784178_c589_4447_bbe5_7b51b97f4918"].to_string()
        == "var 0..1:'UUID_69784178_c589_4447_bbe5_7b51b97f4918'"
    )


def test_simple_equation(meta: TextXMetaModel):
    test_strs = [
        "(model",
        "(= UUID_69784178_c589_4447_bbe5_7b51b97f4918 1)",
        ")",
    ]
    mod: clif.Text = meta.model_from_str("\n".join(test_strs))
    mzn = clif_to_MZN(mod)
    assert len(mzn.constraint_decls) == 1
    assert (
        mzn.constraint_decls[0].arithmetic_predicate == ArithmeticPredicate.EQ
    )
    assert mzn.constraint_decls[0].terms is not None
    assert len(mzn.constraint_decls[0].terms) == 2
    assert (
        mzn.constraint_decls[0].terms[0]
        == "UUID_69784178_c589_4447_bbe5_7b51b97f4918"
    )
    assert mzn.constraint_decls[0].terms[1] == 1


def test_conjunction_declaration(meta: TextXMetaModel):
    test_strs = [
        "(model",
        "(and (bool UUID_69784178_c589_4447_bbe5_7b51b97f4918) (= UUID_69784178_c589_4447_bbe5_7b51b97f4918 1))",  # noqa
        ")",
    ]
    mod: clif.Text = meta.model_from_str("\n".join(test_strs))
    mzn = clif_to_MZN(mod)
    # Constraint part
    assert len(mzn.constraint_decls) == 1
    assert (
        mzn.constraint_decls[0].arithmetic_predicate == ArithmeticPredicate.EQ
    )
    assert mzn.constraint_decls[0].terms is not None
    assert len(mzn.constraint_decls[0].terms) == 2
    assert (
        mzn.constraint_decls[0].terms[0]
        == "UUID_69784178_c589_4447_bbe5_7b51b97f4918"
    )
    assert mzn.constraint_decls[0].terms[1] == 1
    # Decl Part
    assert len(mzn.var_decls) == 1
    assert (
        mzn.var_decls["UUID_69784178_c589_4447_bbe5_7b51b97f4918"].type
        == "bool"
    )
    assert (
        mzn.var_decls["UUID_69784178_c589_4447_bbe5_7b51b97f4918"].var
        == "UUID_69784178_c589_4447_bbe5_7b51b97f4918"
    )


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
    mzn = clif_to_MZN(mod)
    assert len(mzn.constraint_decls) == 2
    assert all(
        map(
            lambda c: c.arithmetic_predicate == ArithmeticPredicate.LTE
            and c.terms is not None
            and all(map(lambda t: isinstance(t, clif.ArithmeticExpr), c.terms)),
            mzn.constraint_decls,
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
    mzn = clif_to_MZN(mod)
    assert (
        mzn.constraint_decls[1].to_string()
        == "constraint 'UUID_43634fef_d816_4cc4_bbde_02cb7865afef' + 'UUID_87b866ef_e358_4797_829c_d3fcac43a21f' <= 'UUID_bf3ab018_6304_4e84_a11f_80f3f5d1d80f' * 2"  # noqa
    )
