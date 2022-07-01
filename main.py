#!/usr/bin/env python3

"""
This is the VariaMos Semantic Translator.
"""
import json
import re
from minizinc import Instance, Model, Solver


def replaceExpr(bundle, elems, rels, cons, p):
    print(cons)
    regex = r"\w?" + re.escape(p) + r"\w?"
    occs = re.findall(regex, cons)
    f = (r for (k, r) in rels.items() if (r["targetId"] == k))
    fs = [elem for ((iden, _), elem) in elems.items() if ([rel for (_,rel) in rels.items() if rel["targetId"] == iden])]


def bundleCons(bundle, elems, rels, rules):
    # get constraint rule
    rule = rules["elementTranslationRules"][0]["Bundle"]
    cons = rule["constraint"][bundle["properties"][1]["value"]]
    [cons := cons.replace(p, replaceExpr(bundle, elems, rels, cons, p)) for p in rule["param"]]


def mapBundles(elems, rels, rules):
    return [
        bundleCons(bs, elems, rels, rules)
        for bs in [
            e if e["type"] == "Bundle" else None for ((iden, typ), e) in elems.items()
        ]
        if bs is not None
    ]


def mapVar(element, rule):
    # print(element)
    # print(rule)
    # return rule
    if bool(rule):
        constraint = (
            rule["constraint"].replace(
                rule["param"], str(element["id"]).replace("-", "_")
            )
            + f'% {element["type"]} -> {element["id"]}'
        )
        # print(constraint)
        return constraint
    # If not bool(rule) then return None


def mapVars(elems, rules):
    return [
        cs
        for cs in [
            mapVar(element, rules["elementTranslationRules"][0][typ])
            if (typ in rules["elementTypes"])
            else None
            for ((ident, typ), element) in elems.items()
        ]
        if cs is not None
    ]


def mapCons(relation, rule):
    if bool(rule):
        # print(relation)
        acc = rule["constraint"]
        [
            acc := acc.replace(
                p,
                str(
                    relation[("source" if p == rule["params"][0] else "target") + "Id"]
                ).replace("-", "_"),
            )
            for p in rule["params"]
        ]
        # print(acc)
        return acc


def mapRels(relations, rules):
    return [
        rs
        for rs in [
            mapCons(
                v, rules["relationTranslationRules"][0][v["properties"][0]["value"]]
            )
            for (k, v) in [
                (k, rel) for (k, rel) in relations.items() if rel["properties"]
            ]
            if (v["properties"][0]["value"] in rules["relationTypes"])
        ]
        if rs is not None
    ]


FILE = "/home/kaiser185/workspace/semantic_translator/json/vmosfm.json"
RULES = "/home/kaiser185/workspace/semantic_translator/json/fmrules.json"

# Load file
with open(FILE, "r") as f:
    # Load json as obj
    array = json.load(f)
    # Get the feature model @ /productLines[0]/domainEngineering/models[0]
    fm = array["productLines"][0]["domainEngineering"]["models"][0]
    # Get the elements
    elementsMap = {(e["id"], e["type"]): e for e in fm["elements"]}
    # Get the relationships
    relationsMap = {r["id"]: r for r in fm["relationships"]}
    # print(relationsMap)
    # Create the rules
    with open(RULES, "r") as r:
        rules = json.load(r)
        # Map the constraints for the vars
        constraints = (
            mapVars(elementsMap, rules)
            + mapRels(relationsMap, rules)
            + mapBundles(elementsMap, relationsMap, rules)
            + ["solve satisfy;"]
        )
        # print(constraints)
        print("\n".join([c for c in constraints]))
        # Add model and solver
        gecode = Solver.lookup("gecode")
        model = Model()
        model.add_string("\n".join([c for c in constraints]))
        instance = Instance(gecode, model)
        result = instance.solve()
        print(result)
