import json
import re
from minizinc import Instance, Model, Solver

def replaceWithPattern(pattern, string, occ, v):
    if type(v) is not str and string is not None:
        print(v.items())
        print(string)
        [string := string.replace(occ, str(val)) for (k, val) in v.items()]
        print('OK')
        return string


def replaceExprs(bundle, elems, rels, cons, params, complexT):
    f = [iden for (k, r) in rels.items() for ((iden,_), _) in elems.items() if (str(r["sourceId"]) == str(iden) and str(r["targetId"]) == str(bundle["id"]))]
    # replace constraint for principal param
    fs = [
        iden
        for ((iden, _), elem)
        in elems.items()
        if ([rel for (_,rel) in rels.items() if rel["sourceId"] == bundle["id"] and rel["targetId"] == iden])
    ]
    fs = ["uuid_" + ef.replace("-","_") for ef in fs if (ef not in f)]
    print(fs)
    pattern = {
        "F": f[0],
        "Xs": {
            "sum":" + ".join(fs),
        }
    }
    cons = str(cons).replace(params[0], "uuid_" + pattern[params[0]].replace("-","_"))
    funs = r"(" + r"|".join(complexT["functions"]) + r")"
    regex_paren = funs + r"\(" + re.escape(params[1]) + r"\)"
    occs = set([oc.group(0) for oc in re.finditer(regex_paren, cons)])
    [cons := cons.replace(occ, pattern[params[1]][re.compile(regex_paren).search(occ).group(1)]) for occ in occs]
    return cons



def bundleCons(bundle, elems, rels, rules):
    # get constraint rule
    rule = rules["elementTranslationRules"][0]["Bundle"]
    cons = rule["constraint"][bundle["properties"][1]["value"]]
    complexTrans = rules["complexElemTranslations"]
    return replaceExprs(bundle, elems, rels, cons, rule["param"], complexTrans)


def mapBundles(elems, rels, rules):
    return [
        bundleCons(bs, elems, rels, rules)
        for bs in [
            e if e["type"] == "Bundle" else None for ((iden, typ), e) in elems.items()
        ]
        if bs is not None
    ]


def mapVar(element, rule):
    # return rule
    if bool(rule):
        constraint = (
            rule["constraint"].replace(
                rule["param"], str(element["id"]).replace("-", "_")
            )
            + f'% {element["type"]} -> {element["id"]}'
        )
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

def run(model, rules, language):
    # Get the feature model @ /productLines[0]/domainEngineering/models[0]
    fm = model["productLines"][0]["domainEngineering"]["models"][0]
    # Get the elements
    elementsMap = {(e["id"], e["type"]): e for e in fm["elements"]}
    # Get the relationships
    relationsMap = {r["id"]: r for r in fm["relationships"]}
    # Map the constraints for the vars
    constraints = (
        mapVars(elementsMap, rules)
        + mapRels(relationsMap, rules)
        + mapBundles(elementsMap, relationsMap, rules)
        + ["solve satisfy;"]
    )
    print(constraints)
    print("-------------------------------------------------------")
    print("\n".join([c for c in constraints]))
    # Add model and solver
    gecode = Solver.lookup("gecode")
    model = Model()
    model.add_string("\n".join([c for c in constraints]))
    instance = Instance(gecode, model)
    result = instance.solve()
    return result
