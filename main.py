import re

# from grammars.hlvl import parse_hlvl
from grammars.clif import clif_meta_model, clif_model
from minizinc_bridge import minizinc_solve, minizinc_update_model
from prolog_bridge import prolog_solve, prolog_update_model
from targets.minzinc.minizinc_model import clif_to_MZN_objects


def replaceWithPattern(pattern, string, occ, v):
    if type(v) is not str and string is not None:
        # print(v.items())
        # print(string)
        [string := string.replace(occ, str(val)) for (k, val) in v.items()]
        # print('OK')
        return string


def replaceExprs(bundle, elems, rels, cons, params, complexT):
    """
    This function replaces the first and second expressions
    for a bundle's constraint.
    """
    f = [
        iden
        for (k, r) in rels.items()
        for ((iden, _), _) in elems.items()
        if (
            str(r["sourceId"]) == str(iden)
            and str(r["targetId"]) == str(bundle["id"])
        )
    ]
    # replace constraint for principal param
    fs = [
        iden
        for ((iden, _), elem) in elems.items()
        if (
            [
                rel
                for (_, rel) in rels.items()
                if rel["sourceId"] == bundle["id"] and rel["targetId"] == iden
            ]
        )
    ]
    fs = ["UUID_" + ef.replace("-", "_") for ef in fs if (ef not in f)]
    # print(fs)
    # print(bundle)
    pattern = {
        "F": f[0],
        "Xs": {
            "sum(Xs)": " + ".join(fs),
            "len(Xs)": str(len(fs)),
            "[Xs]": f"[{','.join(fs)}]",
        },
    }
    cons = str(cons).replace(
        params[0], "UUID_" + pattern[params[0]].replace("-", "_")
    )
    funs = r"(" + r"|".join(complexT["functions"]) + r")?"
    regex_paren = funs + r"(\(|\[)" + re.escape(params[1]) + r"(\)|\])"
    occs = set([oc.group(0) for oc in re.finditer(regex_paren, cons)])
    [
        cons := cons.replace(
            occ,
            pattern[params[1]][
                re.compile(regex_paren).search(occ).group(0)  # pyright: ignore
            ],
        )
        for occ in occs
    ]
    # print(bundle["pruperties"][1]["type"])
    # handle special range case.
    if bundle["properties"][1]["value"] == "Range":
        ranges = {
            "min": bundle["properties"][2]["value"],
            "max": bundle["properties"][3]["value"],
        }
        [
            cons := cons.replace(params[i], ranges[params[i]])
            for i in range(2, len(params))
        ]
    return cons


def bundleCons(bundle, elems, rels, language, rules):
    """
    This is an auxiliary function that builds the request to replaceExprs
    """
    # get constraint rule
    rule = rules["elementTranslationRules"][language]["Bundle"]
    cons = rule["constraint"][bundle["properties"][1]["value"]]
    complexTrans = rules["complexElemTranslations"]
    return replaceExprs(bundle, elems, rels, cons, rule["param"], complexTrans)


def mapBundles(elems, rels, language, rules):
    """
    This function collects all the strings related to the bundles
    (it is the only portion of this module that is custom to feature models)
    """
    return [
        bundleCons(bs, elems, rels, language, rules)
        for bs in [
            e if e["type"] == "Bundle" else None
            for ((iden, typ), e) in elems.items()
        ]
        if bs is not None
    ]


def mapVar(element, rule):
    """Maps an element into a constraint according to the rules"""
    # return rule
    template = rule["constraint"]
    if bool(rule):
        if "selected_constraint" in rule and "deselected_constraint" in rule:
            if "Selected" in element["properties"][1]["value"]:
                template = rule["selected_constraint"]
            elif "Unselected" in element["properties"][1]["value"]:
                template = rule["deselected_constraint"]

        constraint = (
            template.replace(
                rule["param"], str(element["id"]).replace("-", "_")
            )
            # + f'% {element["type"]} → {element["id"]}'
        )
        return constraint
    # If not bool(rule) then return None


def mapVars(elems, language, rules):
    """This function collects all strings related to a set of elements and translation rules"""
    return [
        cs
        for cs in [
            mapVar(element, rules["elementTranslationRules"][language][typ])
            if (typ in rules["elementTypes"])
            else None
            for ((ident, typ), element) in elems.items()
        ]
        if cs is not None
    ]


def mapCons(relation, rule):
    """This function maps a relation into a constraint according to the rules"""
    if bool(rule):
        acc = rule["constraint"]
        [
            acc := acc.replace(
                p,
                str(
                    relation[
                        ("source" if p == rule["params"][0] else "target")
                        + "Id"
                    ]
                ).replace("-", "_"),
            )
            for p in rule["params"]
        ]
        return acc


def mapRels(relations, language, rules):
    """This function collects all strings related to a set of relations and translation rules"""
    return [
        rs
        for rs in [
            mapCons(
                v,
                rules["relationTranslationRules"][language][
                    v["properties"][0]["value"]
                ],
            )
            for (k, v) in [
                (k, rel) for (k, rel) in relations.items() if rel["properties"]
            ]
            if (v["properties"][0]["value"] in rules["relationTypes"])
        ]
        if rs is not None
    ]


class SolverException(Exception):
    pass


def run(model, rules, language, solver, dry, selectedModelId):
    """This function takes in a model, a set of rules and a language to translate to and runs the procedure"""
    # Get the feature model @ /productLines[0]/domainEngineering/models[0]
    idx, fm = next(
        filter(
            lambda mod: mod[1]["id"] == selectedModelId,
            enumerate(model["productLines"][0]["domainEngineering"]["models"]),
        )
    )
    # Get the elements
    elementsMap = {(e["id"], e["type"]): e for e in fm["elements"]}
    # Get the relationships
    relationsMap = {r["id"]: r for r in fm["relationships"]}
    #################################
    # hlvl_header = "model test"
    # hlvl_options = "options:"
    # hlvl_relations = "relations:"
    # # Map the constraints for the vars
    # constraints = (
    #     ([hlvl_header] if language == 'hlvl' else [])
    #     + ([hlvl_options] if language == 'hlvl' else [])
    #     + mapVars(elementsMap, language, rules)
    #     + ([hlvl_relations] if language == 'hlvl' else [])
    #     + mapRels(relationsMap, language, rules)
    #     + mapBundles(elementsMap, relationsMap, language, rules)
    #     # + ["solve satisfy;"]
    # )
    #################################
    hlvl_header = "(cl:text"
    hlvl_options = "options:"
    hlvl_relations = "relations:"
    # Map the constraints for the vars
    constraints = (
        ([hlvl_header] if language == "hlvl" else [])
        # + ([hlvl_options] if language == 'hlvl' else [])
        + mapVars(elementsMap, language, rules)
        # + ([hlvl_relations] if language == 'hlvl' else [])
        + mapRels(relationsMap, language, rules)
        + mapBundles(elementsMap, relationsMap, language, rules)
        + [")"]
    )
    ###############################
    print(constraints)
    # These conditions are left for now so that current
    # functionality doesn't break
    if language == "minizinc":
        result = minizinc_solve(constraints)
        # If no solution is found
        # the second element of the tuple is
        # None
        if not result.status.has_solution():
            raise SolverException("MZN - Model is UNSAT")
        elif not dry:
            minizinc_update_model(fm, rules, result)
        else:
            return "MZN - SAT check OK"
    elif language == "swi":
        result = prolog_solve(constraints)
        if result is False:
            raise SolverException("SWI - Model is UNSAT")
        elif not dry:
            prolog_update_model(fm, rules, result)
        else:
            return "SWI - SAT check OK"
    ###############################################################
    # elif language == 'hlvl':
    #     hlvl_solver_constraints = parse_hlvl(constraints, solver)
    #     if solver == 'minizinc':
    #         result = minizinc_solve(hlvl_solver_constraints)
    #         if not result.status.has_solution():
    #             raise SolverException('HLVL/MZN - Model is UNSAT')
    #         elif not dry:
    #             minizinc_update_model(fm, rules, result)
    #         else:
    #             return 'HLVL/MZN - SAT check OK'
    #     elif solver == 'swi':
    #         print(hlvl_solver_constraints)
    #         result = prolog_solve(hlvl_solver_constraints)
    #         if result is False:
    #             raise SolverException('SWI - Model is UNSAT')
    #         elif not dry:
    #             prolog_update_model(fm, rules, result)
    #         else:
    #             return 'SWI - SAT check OK'
    ###############################################################
    elif language == "hlvl":
        clif_mm = clif_meta_model()
        mzn = clif_to_MZN_objects(
            clif_mm.model_from_str("\n".join(constraints))
        )
        hlvl_solver_constraints = clif_model(
            clif_meta_model(), constraints, solver
        )
        if solver == "minizinc":
            result = minizinc_solve(
                [v.to_string() + ";" for v in mzn.var_decls]
                + [c.to_string() + ";" for c in mzn.constraint_decls]
            )
            if not result.status.has_solution():
                raise SolverException("CLIF/MZN - Model is UNSAT")
            elif not dry:
                minizinc_update_model(fm, rules, result)
            else:
                return "CLIF/MZN - SAT check OK"
        elif solver == "swi":
            print(hlvl_solver_constraints)
            result = prolog_solve(hlvl_solver_constraints)
            if result is False:
                raise SolverException("CLIF/SWI - Model is UNSAT")
            elif not dry:
                prolog_update_model(fm, rules, result)
            else:
                return "CLIF/SWI - SAT check OK"
    else:
        raise RuntimeError("Unrecognized Language")
    print(result)  # pyright: ignore
    # print(constraints)
    # print("-----------------------MODEL--------------------------------")
    # print("\n".join([c for c in constraints]))
    # # Add model and solver
    # gecode = Solver.lookup("gecode")
    # mzn_model = Model()
    # mzn_model.add_string("\n".join([c for c in constraints]))
    # instance = Instance(gecode, mzn_model)
    # result = instance.solve()
    # print("----------------------/MODEL--------------------------------")
    # return result
    # Now lets update the model based on the result

    # if not dry:
    model["productLines"][0]["domainEngineering"]["models"][idx] = fm
    return model


def update_model(model, rules, result):
    for e in model["elements"]:
        if e["type"] in rules["elementTypes"]:
            e["properties"][1]["value"] = (
                "Selected"
                if result["UUID_" + str(e["id"]).replace("-", "_")] == 1
                else "Unselected"
            )
