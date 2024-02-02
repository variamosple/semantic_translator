import pytest
import typing
from grammars import clif
from textx.metamodel import TextXMetaModel
from textx import TextXSyntaxError
# Stuff for real model test with complex constraints
# TODO: Move to separate file
from targets.solver_model import SolverModel
from targets.swi.swi_model import SWIModel

@pytest.fixture
def meta() -> TextXMetaModel:
    return clif.clif_meta_model(debug=True)


def test_simple_model(meta: TextXMetaModel):
    test_str = """( model
    (and (= x y) (= y z))
)"""
    mod: clif.Text = meta.model_from_str(test_str)

    assert mod.constructions is not None
    assert isinstance(mod.constructions, clif.TextConstruction)
    for e in mod.constructions.sentences:
        assert isinstance(e, clif.Sentence)

    s = mod.constructions.sentences[0]
    assert isinstance(s, clif.BoolSentence)
    assert s.sentences is not None
    assert len(s.sentences) == 2
    assert isinstance(s.sentences[0], clif.AtomSentence)
    assert isinstance(s.sentences[0].eq, clif.Equation)
    assert s.sentences[0].eq.lhs == "x"
    assert s.sentences[0].eq.rhs == "y"
    assert isinstance(s.sentences[1], clif.AtomSentence)
    assert isinstance(s.sentences[1].eq, clif.Equation)
    assert s.sentences[1].eq.lhs == "y"
    assert s.sentences[1].eq.rhs == "z"


def test_multiple_conjunction(meta: TextXMetaModel):
    test_str = """( model
    (and (= x y) (= y z) (= z w))
)"""
    mod: clif.Text = meta.model_from_str(test_str)
    assert mod.constructions is not None
    assert isinstance(mod.constructions, clif.TextConstruction)
    for e in mod.constructions.sentences:
        assert isinstance(e, clif.Sentence)
    s = mod.constructions.sentences[0]
    assert isinstance(s, clif.BoolSentence)
    assert s.sentences is not None
    assert len(s.sentences) == 3
    assert isinstance(s.sentences[0], clif.AtomSentence)
    assert isinstance(s.sentences[0].eq, clif.Equation)
    assert s.sentences[0].eq.lhs == "x"
    assert s.sentences[0].eq.rhs == "y"
    assert isinstance(s.sentences[1], clif.AtomSentence)
    assert isinstance(s.sentences[1].eq, clif.Equation)
    assert s.sentences[1].eq.lhs == "y"
    assert s.sentences[1].eq.rhs == "z"
    assert isinstance(s.sentences[2], clif.AtomSentence)
    assert isinstance(s.sentences[2].eq, clif.Equation)
    assert s.sentences[2].eq.lhs == "z"
    assert s.sentences[2].eq.rhs == "w"

@pytest.mark.skip(reason="TODO: Investigate why this fails")
def test_attribute_lookup(meta: TextXMetaModel):
    test_str = """( model
    (= x y::z)
)"""
    mod: clif.Text = meta.model_from_str(test_str)
    assert mod.constructions is not None
    assert isinstance(mod.constructions, clif.TextConstruction)
    for e in mod.constructions.sentences:
        assert isinstance(e, clif.Sentence)
    s = mod.constructions.sentences[0]
    assert isinstance(s, clif.AtomSentence)
    assert s.eq is not None
    assert isinstance(s.eq.lhs, str)
    assert isinstance(s.eq.rhs, clif.AttributeLookup)
    assert s.eq.rhs.element == "y"
    assert s.eq.rhs.attribute == "z"


def test_arithm_predicate(meta: TextXMetaModel):
    test_str = """(model
    (< x y)
)"""
    mod: clif.Text = meta.model_from_str(test_str)
    assert mod.constructions is not None
    s = mod.constructions.sentences[0]
    assert isinstance(s, clif.AtomSentence)
    assert s.atom is not None
    assert isinstance(s.atom.pred, clif.ArithmeticPred)
    assert s.atom.pred.lt is True
    assert s.atom.pred.lte is False
    assert s.atom.pred.gt is False
    assert s.atom.pred.gte is False
    assert s.atom.terms == ["x", "y"]


def test_type_pred(meta: TextXMetaModel):
    test_str = """(model
    (int x)
)"""
    mod: clif.Text = meta.model_from_str(test_str)
    assert mod.constructions is not None
    assert isinstance((s := mod.constructions.sentences[0]), clif.AtomSentence)
    assert s.atom is not None
    assert (p := s.atom.pred) is not None
    assert isinstance(p, clif.TypePred)
    assert p.integer and not p.boolean and not p.enum and not p.bounded_integer
    assert len(s.atom.terms) == 1
    assert s.atom.terms[0] == "x"


def test_type_pred_bounded_int(meta: TextXMetaModel):
    test_str = """(model
    (int (1 2) y)
)"""
    mod: clif.Text = meta.model_from_str(test_str)
    assert mod.constructions is not None
    assert isinstance((s := mod.constructions.sentences[0]), clif.AtomSentence)
    assert s.atom is not None
    assert (p := s.atom.pred) is not None
    assert isinstance(p, clif.TypePred)
    assert p.bounded_integer and not p.boolean and not p.enum and not p.integer
    assert len(s.atom.terms) == 1
    assert s.atom.terms[0] == "y"
    assert p.lower == 1
    assert p.upper == 2


def test_multiple_terms(meta: TextXMetaModel):
    test_str = """(model
    (int x)
    (bool y)
    (enum (cat dog) z)
)"""
    mod: clif.Text = meta.model_from_str(test_str)
    assert mod.constructions is not None
    assert all(
        isinstance(s, clif.AtomSentence) for s in mod.constructions.sentences
    )
    assert len(mod.constructions.sentences) == 3
    s2 = typing.cast(clif.AtomSentence, mod.constructions.sentences[2])
    assert s2.atom is not None and (p2 := s2.atom.pred) is not None
    assert isinstance(p2, clif.TypePred)
    assert p2.enum and not p2.boolean and not p2.integer
    assert p2.values is not None and p2.values == ["cat", "dog"]


def test_quantified_terms(meta: TextXMetaModel):
    test_str = """(model
    (forall (x:Xs y:Ys) (and (int x) (int y) (= x y)))
)"""
    mod: clif.Text = meta.model_from_str(test_str)
    assert isinstance((f := mod.constructions.sentences[0]), clif.QuantSentence)
    assert len(f.boundlist.vars) == 2
    assert all(isinstance(s, clif.Binding) for s in f.boundlist.vars)
    assert f.boundlist.vars[0].var == "x"
    assert f.boundlist.vars[0].set_from == "Xs"
    assert f.boundlist.vars[1].var == "y"
    assert f.boundlist.vars[1].set_from == "Ys"
    assert isinstance((s := f.sentence), clif.BoolSentence)
    assert s.operator == "and"
    assert s.sentences is not None
    assert len(s.sentences) == 3
    assert isinstance(s.sentences[0], clif.AtomSentence)


def test_fm_optional(meta: TextXMetaModel):
    test_str = """(model
    (if (= F2 1) (= F1 1))
)"""
    mod: clif.Text = meta.model_from_str(test_str)
    assert isinstance((f := mod.constructions.sentences[0]), clif.BoolSentence)
    assert isinstance((a := f.antecedent), clif.AtomSentence)
    assert (ae := a.eq) is not None and a.atom is None
    assert ae.lhs == "F2" and ae.rhs == 1
    assert isinstance((c := f.consequent), clif.AtomSentence)
    assert (ce := c.eq) is not None and c.atom is None
    assert ce.lhs == "F1" and ce.rhs == 1


def test_disallow_int_as_pred(meta: TextXMetaModel):
    test_str = """(model
    (1 (= F2 1) (= F1 1))
)"""
    with pytest.raises(TextXSyntaxError):
        meta.model_from_str(test_str)


def test_complex_arithm_expr(meta: TextXMetaModel):
    test_str = """(model
    (=< (F1 + F2) 1)
)"""
    mod: clif.Text = meta.model_from_str(test_str, debug=True)
    assert isinstance((a := mod.constructions.sentences[0]), clif.AtomSentence)
    assert (ae := a.atom) is not None and a.eq is None
    assert isinstance(ae.pred, clif.ArithmeticPred)
    assert ae.pred.lte
    assert len(ae.terms) == 2
    assert isinstance((ex := ae.terms[0]), clif.ArithmeticExpr)
    assert isinstance((e := ex.expr), clif.E)
    assert e.t.tp is None
    assert e.t.f.expr is None and e.t.f.val is not None
    assert e.t.f.val == "F1"
    assert e.ep is not None and e.ep.ep is None
    assert e.ep.op == "+"
    assert e.ep.t.f.expr is None and e.ep.t.f.val is not None
    assert e.ep.t.f.val == "F2"


def test_arithm_expr_str(meta: TextXMetaModel):
    test_str = """(model
    (=< (F1 + F2) 1)
)"""
    mod: clif.Text = meta.model_from_str(test_str, debug=True)
    assert isinstance((a := mod.constructions.sentences[0]), clif.AtomSentence)
    assert (ae := a.atom) is not None and a.eq is None
    assert isinstance(ae.pred, clif.ArithmeticPred)
    assert ae.pred.lte
    assert len(ae.terms) == 2
    assert isinstance((ex := ae.terms[0]), clif.ArithmeticExpr)
    assert ex.model_str() == "F1 + F2"
    # assert ae.model_str("swi") == "F1 + F2 #=< 1"
    # assert ae.model_str("minizinc") == "F1 + F2 <= 1"


def test_real_model(meta: TextXMetaModel):
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


def test_real_model2(meta: TextXMetaModel):
    test_model = [
        "(model",
        "(and (bool F1) (= F1 1))",
        "(bool F2)",
        "(bool F3)",
        "(bool F4)",
        "(bool F5)",
        "(bool F6)",
        "(bool F7)",
        "(bool F8)",
        "(bool F9)",
        "(bool F10)",
        "(bool F11)",
        "(bool F12)",
        "(= F1 F2)",
        "(= F1 F3)",
        "(= F1 F4)",
        "(=< (F2 + F8) 1)",
        "(>= F3 F5)",
        "(>= F3 F6)",
        "(= F3 F7)",
        "(=< (F7 * 1) (F8 + F9 + F10))"
        "(=< (F8 + F9 + F10) (F7 * 2))",
        "(=< (F11 + F12) F4)",
        "(if (= F9 1) (not (or (and (= F5 1) (= F6 1) ) (= F11 1) ) ) )",
        ")",
    ]
    string = "\n".join(test_model)
    mod: clif.Text = meta.model_from_str(string, debug=True)
    assert mod.constructions is not None
    generic_csp = SolverModel.from_clif_text(mod)
    constraint_model = SWIModel.from_gen_csp(generic_csp)

