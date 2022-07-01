#!/usr/bin/env python3

"""
Variamos semantic translator server.

By: Camilo Correa Restrepo camilo.correa-restrepo@univ-paris1.fr
By: Hiba Hnaini _@_.fr
"""

from flask import Flask, request, jsonify
from main import run

app = Flask(__name__)


@app.route("/translate/<language>", methods=['POST'])
def translate(language):
    """Handle a translation request for a given <language>."""
    content = request.json
    print(content["model"])
    print(content["rules"])
    return jsonify({"sln": str(run(
        model=content["model"],
        rules=content["rules"],
        language=language
    ))})
