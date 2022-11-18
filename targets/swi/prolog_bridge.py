from swiplserver import PrologMQI, PrologThread
import tempfile
import re


def prolog_solve(constraints):
    print(constraints)
    regex = re.compile(r"(UUID(?:_[a-f0-9]+){5})")
    with tempfile.NamedTemporaryFile(dir="/tmp", delete=True) as tmp:
        program = """:- use_module(library(clpfd)).

program([!!!]) :-
"""
        program += ",\n".join(constraints) + ".\n"
        # print(program)
        occs = list(set(regex.findall(program)))
        print(occs)
        program = program.replace("!!!", ",".join(occs))
        print(program)
        tmp.write(program.encode())
        tmp.flush()
        # Seek file for reading
        tmp.seek(0)
        with PrologMQI() as mqi:
            with PrologThread(mqi) as prolog_thread:
                print(tmp.name)
                query_str = f"['{tmp.name}']"
                print(query_str)
                prolog_thread.query(query_str + ".")
                print("file loaded")
                result = prolog_thread.query(
                    "program([!!!]), once(label([!!!])).".replace(
                        "!!!", ",".join(occs)
                    )
                )
                print(result)
                return result


def prolog_update_model(model, rules, result):
    for e in model["elements"]:
        if e["type"] in rules["elementTypes"] and e["properties"][1][
            "value"
        ] not in ["Selected", "Unselected"]:
            e["properties"][1]["value"] = (
                "SelectedForced"
                if result[0]["UUID_" + str(e["id"]).replace("-", "_")] == 1
                else "UnselectedForced"
            )
