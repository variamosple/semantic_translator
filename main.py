#!/usr/bin/env python3

"""
This is the VariaMos Semantic Translator
"""
import json

with open('/home/kaiser185/workspace/semantic_translator/json/vmos.json', 'r') as f:
    array = json.load(f)

print(array)
