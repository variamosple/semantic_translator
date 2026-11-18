"""Possible values of attributes as people write them in the editor.

The template projects of the editor separate values with "," or ";", put
spaces and accents in them, and write ranges such as "[0..100]". The
generator used to split on "," only and copy each value as it was, so the
logic model either failed to parse (HTTP 500) or cut a value in two
("UPPER CASE" became two values, which made some models unsatisfiable).
Constraints written by hand that cannot be read now give a 400 with a message
instead of a 500.
"""

import contextlib
import copy
import io
import unittest

from flask import Flask

from old_request_handler.request_handler import request_handler
from old_request_handler.variamos import attributes
from tests.test_free_text_attributes import ATTRIBUTE_RULES, attribute, project_with
from tests.test_query_statistics import MODEL, RULES

AUTH = "77777777-7777-4777-8777-777777777777"
SPEED = "88888888-8888-4888-8888-888888888888"

# Integer and Float rules of the "Feature model with attributes" language.
NUMBER_RULE = {
    "param": "id",
    "value": "V",
    "parent": "F",
    "template": "A",
    "constraint": "(and (int UUID_A) (= UUID_A V))",
    "unset_constraint": (
        "(and (int UUID_A) (if (= UUID_F 0) (= UUID_A 0)) (if (>= UUID_F 1) (>= UUID_A 1)))"
    ),
}
LANGUAGE_RULES = {
    **ATTRIBUTE_RULES,
    "attributeTranslationRules": {
        **ATTRIBUTE_RULES["attributeTranslationRules"],
        "Integer": NUMBER_RULE,
        "Float": NUMBER_RULE,
    },
}


def typed(attribute_id, name, value, possible_values, attribute_type):
    return {
        **attribute(attribute_id, name, value, possible_values),
        "type": attribute_type,
    }


def with_constraints(project, constraints):
    project = copy.deepcopy(project)
    project["productLines"][0]["domainEngineering"]["models"][0]["constraints"] = constraints
    return project


class PossibleValuesTestCase(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        app = Flask(__name__)
        app.add_url_rule("/query", view_func=request_handler, methods=["POST"])
        cls.client = app.test_client()

    def post(self, project, solver, operation):
        body = {
            "data": {
                "project": project,
                "rules": {**RULES, **LANGUAGE_RULES},
                "query": {"solver": solver, "operation": operation},
                "modelSelectedId": MODEL,
                "input": "vmos",
            }
        }
        # The legacy handler prints the whole model on each request.
        with contextlib.redirect_stdout(io.StringIO()):
            return self.client.post("/query", json=body)

    def content(self, project, solver, operation):
        response = self.post(project, solver, operation)
        self.assertEqual(response.status_code, 200, response.get_data(as_text=True)[:300])
        return response.get_json()["data"]["content"]

    def test_semicolons_spaces_and_accents_give_one_name_per_value(self):
        project = project_with(
            attribute(AUTH, "Auth", None, "Pin; huella; registro facial; Contraseña")
        )

        clif = self.content(project, "swi", "get_model")

        self.assertIn("(enum (Pin huella registro_facial Contrasena) Leaf::Auth)", clif)

    def test_a_value_that_starts_with_a_digit_gets_a_prefix(self):
        project = project_with(attribute(AUTH, "Version", None, "4.1;5.0"))

        clif = self.content(project, "swi", "get_model")

        self.assertIn("(enum (v_4_1 v_5_0) Leaf::Version)", clif)

    def test_a_repeated_value_is_kept_once(self):
        project = project_with(attribute(AUTH, "Color", None, "Red, red,Blue,"))

        clif = self.content(project, "swi", "get_model")

        self.assertIn("(enum (Red Blue) Leaf::Color)", clif)

    def test_a_range_gives_a_bounded_integer_tied_to_the_selection(self):
        project = project_with(typed(SPEED, "Speed", "Undefined", "[0..100]", "Integer"))

        clif = self.content(project, "swi", "get_model")

        self.assertIn("(int (0 100) Leaf::Speed)", clif)
        self.assertIn("(and (>= Leaf::Speed 0) (=< Leaf::Speed 100))", clif)
        self.assertIs(self.content(project, "z3", "sat"), True)

    def test_a_range_keeps_a_value_set_in_the_model(self):
        project = project_with(typed(SPEED, "Speed", "30", "0..10000", "String"))

        clif = self.content(project, "swi", "get_model")

        self.assertIn("(int (0 10000) Leaf::Speed)", clif)
        self.assertIn("(= Leaf::Speed 30)", clif)

    def test_decimal_bounds_are_widened_to_integers(self):
        project = project_with(typed(SPEED, "Price", "Undefined", "[0.50..10.25]", "Float"))

        clif = self.content(project, "swi", "get_model")

        self.assertIn("(int (0 11) Leaf::Price)", clif)

    def test_unreadable_constraints_answer_400_with_a_message(self):
        project = with_constraints(project_with(), "(\n\n")

        response = self.post(project, "swi", "get_code")

        self.assertEqual(response.status_code, 400)
        error = response.get_json()["data"]["error"]
        self.assertIn("constraints written in this model are not valid", error)

    def test_blank_constraints_are_ignored(self):
        project = with_constraints(project_with(), "  \n ")

        self.assertIn(":- use_module", self.content(project, "swi", "get_code"))


class TranslateFromTestCase(unittest.TestCase):
    def test_a_solver_index_gives_back_the_value_of_the_same_list(self):
        prop = attribute(AUTH, "Auth", None, "Pin; huella; registro facial")

        value = attributes.translate_from(prop, {AUTH: 2})

        self.assertEqual(value, "registro facial")

    def test_a_solver_name_gives_back_the_written_value(self):
        prop = attribute(AUTH, "Auth", None, "Pin; Registro Facial")

        value = attributes.translate_from(prop, {AUTH: "a_registro_facial"})

        self.assertEqual(value, "registro facial")

    def test_a_range_gives_back_the_number(self):
        prop = typed(SPEED, "Speed", None, "[0..100]", "Integer")

        self.assertEqual(attributes.translate_from(prop, {SPEED: 42}), "42")


class ParseRangeTestCase(unittest.TestCase):
    def test_ranges(self):
        self.assertEqual(attributes.parse_range("[0..100]"), (0, 100))
        self.assertEqual(attributes.parse_range(" -5 .. 5 "), (-5, 5))
        self.assertEqual(attributes.parse_range("[0.00..1000.00]"), (0, 1000))

    def test_not_ranges(self):
        for text in (None, "", "Red,Blue", "1..a", "[10..1]", "[0.88, 0.95]"):
            self.assertIsNone(attributes.parse_range(text), text)


if __name__ == "__main__":
    unittest.main()
