import pytest
from grammars import hlvl


@pytest.fixture
def model():
    strings = [
        'model test',
        'options:', 'choice UUID_69784178_c589_4447_bbe5_7b51b97f4918 [1,1]',
        'choice UUID_bf3ab018_6304_4e84_a11f_80f3f5d1d80f [0,1]',
        'choice UUID_ac0d2916_749b_4146_ad32_37622e2aeef0 [0,1]',
        'choice UUID_9e5a250c_9ee7_4d7b_9486_40563a1e9ab8 [0,1]',
        'choice UUID_43634fef_d816_4cc4_bbde_02cb7865afef [0,1]',
        'choice UUID_87b866ef_e358_4797_829c_d3fcac43a21f [0,1]',
        'choice UUID_e51771f2_b0cc_433a_bfee_8e106bb8d17e [0,1]',
        'choice UUID_1cb2b338_f05e_4ccb_9df2_2bc76894336a [0,1]',
        'choice UUID_b2f0093c_60b1_40a0_98d6_ab392dcc74cc [0,1]',
        'relations:',
        'decomposition(UUID_69784178_c589_4447_bbe5_7b51b97f4918, [UUID_bf3ab018_6304_4e84_a11f_80f3f5d1d80f], [1,1])',
        'decomposition(UUID_69784178_c589_4447_bbe5_7b51b97f4918, [UUID_ac0d2916_749b_4146_ad32_37622e2aeef0], [1,1])',
        'decomposition(UUID_bf3ab018_6304_4e84_a11f_80f3f5d1d80f, [UUID_9e5a250c_9ee7_4d7b_9486_40563a1e9ab8], [0,1])',
        'decomposition(UUID_ac0d2916_749b_4146_ad32_37622e2aeef0, [UUID_e51771f2_b0cc_433a_bfee_8e106bb8d17e], [1,1])',
        'decomposition(UUID_e51771f2_b0cc_433a_bfee_8e106bb8d17e, [UUID_1cb2b338_f05e_4ccb_9df2_2bc76894336a], [1,1])',
        'decomposition(UUID_e51771f2_b0cc_433a_bfee_8e106bb8d17e, [UUID_b2f0093c_60b1_40a0_98d6_ab392dcc74cc], [0,1])',
        'mutex(UUID_b2f0093c_60b1_40a0_98d6_ab392dcc74cc,UUID_87b866ef_e358_4797_829c_d3fcac43a21f)',
        'implies(UUID_9e5a250c_9ee7_4d7b_9486_40563a1e9ab8,UUID_43634fef_d816_4cc4_bbde_02cb7865afef)',
        'group (UUID_bf3ab018_6304_4e84_a11f_80f3f5d1d80f, [Xs], [1,1])'
    ]
    return hlvl.parse_hlvl(strings)


def test_model_name(model):
    assert model.name == 'test'
