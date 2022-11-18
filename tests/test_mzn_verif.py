import pytest
from textx.metamodel import TextXMetaModel
from grammars import clif


@pytest.fixture
def meta() -> TextXMetaModel:
    return clif.clif_meta_model(debug=True)


@pytest.fixture
def real_model(meta: TextXMetaModel):
    test_strs = [
        "(model",
        "(and (bool UUID_69784178_c589_4447_bbe5_7b51b97f4918) (= UUID_69784178_c589_4447_bbe5_7b51b97f4918 1))",  # noqa
        "(bool UUID_bf3ab018_6304_4e84_a11f_80f3f5d1d80f)",
        "(bool UUID_ac0d2916_749b_4146_ad32_37622e2aeef0)",
        "(bool UUID_9e5a250c_9ee7_4d7b_9486_40563a1e9ab8)",
        "(bool UUID_43634fef_d816_4cc4_bbde_02cb7865afef)",
        "(bool UUID_87b866ef_e358_4797_829c_d3fcac43a21f)",
        "(bool UUID_e51771f2_b0cc_433a_bfee_8e106bb8d17e)",
        "(bool UUID_1cb2b338_f05e_4ccb_9df2_2bc76894336a)",
        "(bool UUID_b2f0093c_60b1_40a0_98d6_ab392dcc74cc)",
        "(= UUID_69784178_c589_4447_bbe5_7b51b97f4918 UUID_bf3ab018_6304_4e84_a11f_80f3f5d1d80f)",
        "(= UUID_69784178_c589_4447_bbe5_7b51b97f4918 UUID_ac0d2916_749b_4146_ad32_37622e2aeef0)",
        "(>= (UUID_bf3ab018_6304_4e84_a11f_80f3f5d1d80f + UUID_9e5a250c_9ee7_4d7b_9486_40563a1e9ab8) 1)",
        "(= UUID_ac0d2916_749b_4146_ad32_37622e2aeef0 UUID_e51771f2_b0cc_433a_bfee_8e106bb8d17e)",
        "(= UUID_e51771f2_b0cc_433a_bfee_8e106bb8d17e UUID_1cb2b338_f05e_4ccb_9df2_2bc76894336a)",
        "(>= (UUID_e51771f2_b0cc_433a_bfee_8e106bb8d17e + UUID_b2f0093c_60b1_40a0_98d6_ab392dcc74cc) 1)",
        "(=< (UUID_b2f0093c_60b1_40a0_98d6_ab392dcc74cc + UUID_87b866ef_e358_4797_829c_d3fcac43a21f) 1)",
        "(>= (UUID_43634fef_d816_4cc4_bbde_02cb7865afef + UUID_9e5a250c_9ee7_4d7b_9486_40563a1e9ab8) 1)",
        "(= (UUID_43634fef_d816_4cc4_bbde_02cb7865afef + UUID_87b866ef_e358_4797_829c_d3fcac43a21f) UUID_bf3ab018_6304_4e84_a11f_80f3f5d1d80f )",
        ")",
    ]
    string = "\n".join(test_strs)
    mod: clif.Text = meta.model_from_str(string, debug=True)
    return mod


def test_non_void_spec(meta: TextXMetaModel, real_model: clif.Text):
    verif_spec = (
        "(verif"
        " (exists (list_of_sols)"
        " (and (sols 2 list_of_sols)"
        " (> list_of_sols 1)"
        " )"
        " )"
        ")"
    )
    print(verif_spec)
    ver_model: clif.Text = meta.model_from_str(verif_spec)
    assert ver_model.constructions.verif
    
