"""
VariaMos semantic translator server.
"""

from flask import Flask, Response, jsonify, request
from flask_cors import CORS
from werkzeug.middleware.proxy_fix import ProxyFix

from old_request_handler.request_handler import request_handler as old_request_handler
from variability_solver.backends.minizinc.backend import GecodeBackend
from variability_solver.backends.z3.backend import Z3Backend
from variability_solver.frontends.clif.frontend import CLIFFrontend
from variability_solver.handler import Handler as VariabilitySolverHandler

app = Flask(__name__)
CORS(app)
app.wsgi_app = ProxyFix(app.wsgi_app, x_for=1, x_proto=1, x_host=1, x_prefix=1)


MODEL_LANGUAGES = {CLIFFrontend()}
SOLVERS = {Z3Backend(), GecodeBackend()}


# Legacy endpoint for backward compatibility
@app.route("/query", methods=["POST", "OPTIONS"])
def old_query() -> Response:
    response = old_request_handler()
    return response


@app.route("/status", methods=["GET"])
def status() -> Response:
    return Response("Server is up and running", status=200)


@app.route("/solvers", methods=["GET"])
def get_solvers() -> Response:
    return jsonify(
        {
            "solvers": [
                {
                    "name": solver.name,
                    "description": solver.description,
                }
                for solver in SOLVERS
            ]
        }
    )


@app.route("/solvers/<solver_name>", methods=["GET"])
def get_solver(solver_name: str) -> Response:
    solver = next((s for s in SOLVERS if s.name == solver_name), None)
    if solver is None:
        return Response("Solver not found", status=404)
    return jsonify(
        {
            "name": solver_name,
            "description": solver.description,
        }
    )


@app.route("/model-languages", methods=["GET"])
def get_model_languages() -> Response:
    return jsonify(
        {
            "model_languages": [
                {
                    "name": model_language.name,
                    "description": model_language.description,
                }
                for model_language in MODEL_LANGUAGES
            ]
        }
    )


@app.route("/model-languages/<model_language_name>", methods=["GET"])
def get_model_language(model_language_name: str) -> Response:
    model_language = next((m for m in MODEL_LANGUAGES if m.name == model_language_name), None)
    if model_language is None:
        return Response("Model language not found", status=404)
    return jsonify(
        {
            "name": model_language_name,
            "description": model_language.description,
        }
    )


@app.route("/solve", methods=["POST"])
def solve() -> Response:
    handler = VariabilitySolverHandler()
    return handler.handle(request)
