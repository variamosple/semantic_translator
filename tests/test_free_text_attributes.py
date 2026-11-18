"""A String attribute without possible values must not break the translation.

The String rule of the attributed feature model language builds an enumeration
from the possible values ("Xs"). A free-text attribute has none, and the
generator used to call replace(None, ...), so every query on such a model,
"Get swi Model" included, failed with HTTP 500.
"""

import contextlib
import copy
import io
import unittest

from flask import Flask

from old_request_handler.request_handler import request_handler
from tests.test_query_statistics import LEAF, MODEL, PROJECT, RULES

FREE_TEXT = "55555555-5555-4555-8555-555555555555"
COLOR = "66666666-6666-4666-8666-666666666666"

# String rule of the "Feature model with attributes" language.
ATTRIBUTE_RULES = {
    "attributeTypes": ["RootFeature", "ConcreteFeature"],
    "attributeTranslationRules": {
        "String": {
            "param": "id",
            "parent": "F",
            "values": "Xs",
            "template": "A",
            "constraint": "(enum (Xs) UUID_A)",
        },
    },
}


def attribute(attribute_id, name, value, possible_values):
    return {
        "id": attribute_id,
        "name": name,
        "value": value,
        "type": "String",
        "custom": True,
        "possibleValues": possible_values,
    }


def project_with(*attributes):
    project = copy.deepcopy(PROJECT)
    model = project["productLines"][0]["domainEngineering"]["models"][0]
    leaf = next(e for e in model["elements"] if e["id"] == LEAF)
    leaf["properties"].extend(attributes)
    return project


class FreeTextAttributeTestCase(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        app = Flask(__name__)
        app.add_url_rule("/query", view_func=request_handler, methods=["POST"])
        cls.client = app.test_client()

    def query(self, project, solver, operation):
        body = {
            "data": {
                "project": project,
                "rules": {**RULES, **ATTRIBUTE_RULES},
                "query": {"solver": solver, "operation": operation},
                "modelSelectedId": MODEL,
                "input": "vmos",
            }
        }
        # The legacy handler prints the whole model on each request.
        with contextlib.redirect_stdout(io.StringIO()):
            response = self.client.post("/query", json=body)
        self.assertEqual(response.status_code, 200, response.get_data(as_text=True)[:300])
        return response.get_json()["data"]["content"]

    def test_get_code_answers_with_a_free_text_attribute(self):
        project = project_with(
            attribute(FREE_TEXT, "Description", "Any text, even with spaces", None)
        )

        program = self.query(project, "swi", "get_code")

        self.assertIn(":- use_module(library(clpfd)).", program)
        self.assertNotIn(FREE_TEXT.replace("-", "_"), program)

    def test_an_empty_list_of_possible_values_is_free_text_too(self):
        project = project_with(attribute(FREE_TEXT, "Description", "text", ""))

        # z3: the test machine may not have SWI-Prolog installed.
        self.assertIs(self.query(project, "z3", "sat"), True)

    def test_possible_values_still_give_an_enumeration(self):
        project = project_with(attribute(COLOR, "Color", "Red", "Red,Blue"))

        clif = self.query(project, "swi", "get_model")

        self.assertIn("(enum (Red Blue) Leaf::Color)", clif)


if __name__ == "__main__":
    unittest.main()
