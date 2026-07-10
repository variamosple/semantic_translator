from flask import Request, Response, jsonify
from pydantic import ValidationError

from variability_solver.backends.minizinc.backend import GecodeBackend
from variability_solver.backends.z3.backend import Z3Backend
from variability_solver.frontends.clif.frontend import CLIFFrontend
from variability_solver.options import Options
from variability_solver.query import Query


class Handler:
    def handle(self, request: Request) -> Response:
        """
        Handle a request.

        Args:
            request: The request to handle.

        Returns:
            The result of the request.
        """
        assert request.method == "POST", "Request must be POST"
        if not request.is_json:
            return Response("Request must be JSON", status=400)

        # Extract content from request

        content = request.json
        if content is None:
            return Response("Request must be JSON", status=400)

        try:
            model = str(content["model"])
            model_language = str(content["model_language"])
            solver = str(content.get("solver", "z3"))
            query = Query.parse_obj(content["query"])
            options = Options.parse_obj(content["options"])
        except ValidationError as e:
            return Response(e.json(), status=400, mimetype="application/json")

        # Parse the input into the IR

        FRONTENDS = {
            "clif": CLIFFrontend,
        }

        if model_language not in FRONTENDS:
            return Response(
                f"Unsupported model language: {model_language}",
                status=400,
            )

        frontend = FRONTENDS[model_language]()
        ir = frontend.parse(model)

        # Execute the query

        BACKENDS = {
            "z3": Z3Backend,
            "gecode": GecodeBackend,
        }

        if solver not in BACKENDS:
            return Response(
                f"Unsupported solver: {solver}",
                status=400,
            )

        backend = BACKENDS[solver]()
        result = backend.execute(ir, query, options)

        return jsonify(
            {
                "status": "success",
                "result": result.dict(),
            }
        )
