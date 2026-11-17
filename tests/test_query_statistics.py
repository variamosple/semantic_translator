"""The legacy /query route must answer the operations that never run the solver.

get_code and get_model used to fail with HTTP 500 because the statistics read
solver timings that only exist after a solve. The editor calls both operations
from its "Solver Specific Semantics" and "CLIF Semantics" views.
"""

import contextlib
import io
import unittest

from flask import Flask

from old_request_handler.request_handler import request_handler

ROOT = "11111111-1111-4111-8111-111111111111"
LEAF = "22222222-2222-4222-8222-222222222222"
MANDATORY = "33333333-3333-4333-8333-333333333333"
MODEL = "44444444-4444-4444-8444-444444444444"


def feature(element_id, element_type, name):
    return {
        "id": element_id,
        "type": element_type,
        "name": name,
        "x": 0,
        "y": 0,
        "width": 100,
        "height": 33,
        "parentId": None,
        "properties": [
            {
                "id": element_id[:-1] + "a",
                "name": "Selected",
                "value": "Undefined",
                "type": "String",
                "custom": False,
                "possibleValues": None,
            }
        ],
    }


PROJECT = {
    "id": "project",
    "name": "project",
    "productLines": [
        {
            "id": "product-line",
            "name": "product-line",
            "domainEngineering": {
                "models": [
                    {
                        "id": MODEL,
                        "name": "Feature model",
                        "type": "Feature model",
                        "constraints": "",
                        "elements": [
                            feature(ROOT, "RootFeature", "Root"),
                            feature(LEAF, "ConcreteFeature", "Leaf"),
                        ],
                        "relationships": [
                            {
                                "id": MANDATORY,
                                "type": "Mandatory",
                                "name": "_",
                                "sourceId": LEAF,
                                "targetId": ROOT,
                                "points": [],
                                "min": 0,
                                "max": 9999999,
                                "properties": [
                                    {
                                        "id": MANDATORY[:-1] + "a",
                                        "name": "Type",
                                        "value": "Mandatory",
                                        "type": "String",
                                        "custom": False,
                                    }
                                ],
                            }
                        ],
                    }
                ]
            },
            "applicationEngineering": {"models": [], "applications": []},
        }
    ],
}

RULES = {
    "elementTypes": ["RootFeature", "ConcreteFeature"],
    "elementTranslationRules": {
        "RootFeature": {"param": "F", "constraint": "(and (bool UUID_F) (= UUID_F 1))"},
        "ConcreteFeature": {"param": "F", "constraint": "(bool UUID_F)"},
    },
    "attributeTypes": [],
    "attributeTranslationRules": {},
    "typingRelationTypes": [],
    "typingRelationTranslationRules": {},
    "hierarchyTypes": [],
    "hierarchyTranslationRules": {},
    "relationTypes": ["Mandatory"],
    "relationReificationTypes": [],
    "relationReificationTranslationRules": {},
    "relationReificationTypeDependentExpansions": {},
    "relationReificationPropertySchema": {},
    "relationReificationExpansions": {"params": [], "functions": []},
    "relationPropertySchema": {"type": {"index": 0, "key": "value"}},
    "relationTranslationRules": {
        "Mandatory": {"params": ["F1", "F2"], "constraint": "(= UUID_F1 UUID_F2)"}
    },
}

STATISTICS = {
    "clif_gen_time",
    "clif_ast_time",
    "bridge_solve_time",
    "code_gen_time",
    "generic_csp_model_time",
    "concrete_csp_model_time",
    "n_constraints",
    "n_features",
    "total_time",
}


class QueryStatisticsTestCase(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        app = Flask(__name__)
        app.add_url_rule("/query", view_func=request_handler, methods=["POST"])
        cls.client = app.test_client()

    def query(self, solver, operation):
        body = {
            "data": {
                "project": PROJECT,
                "rules": RULES,
                "query": {"solver": solver, "operation": operation},
                "modelSelectedId": MODEL,
                "input": "vmos",
            }
        }
        # The legacy handler prints the whole model on each request.
        with contextlib.redirect_stdout(io.StringIO()):
            response = self.client.post("/query", json=body)
        self.assertEqual(response.status_code, 200, response.get_data(as_text=True)[:300])
        return response.get_json()

    def test_get_code_returns_the_swi_program(self):
        payload = self.query("swi", "get_code")

        self.assertIn(":- use_module(library(clpfd)).", payload["data"]["content"])
        self.assertEqual(set(payload["statistics"]), STATISTICS)
        self.assertIsNone(payload["statistics"]["bridge_solve_time"])
        self.assertIsInstance(payload["statistics"]["code_gen_time"], int)

    def test_get_code_returns_the_minizinc_program(self):
        payload = self.query("minizinc", "get_code")

        self.assertIn(f"var 0..1:'UUID_{ROOT.replace('-', '_')}'", payload["data"]["content"])
        self.assertIsNone(payload["statistics"]["bridge_solve_time"])

    def test_get_model_returns_the_clif_text_with_names(self):
        payload = self.query("swi", "get_model")

        self.assertIn("(= Leaf Root)", payload["data"]["content"])
        self.assertEqual(set(payload["statistics"]), STATISTICS)
        self.assertIsNone(payload["statistics"]["n_features"])

    def test_sat_still_reports_every_measure(self):
        payload = self.query("z3", "sat")

        self.assertIs(payload["data"]["content"], True)
        for name in STATISTICS:
            self.assertIsInstance(payload["statistics"][name], int, name)


if __name__ == "__main__":
    unittest.main()
