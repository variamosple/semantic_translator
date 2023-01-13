from dataclasses import dataclass
from swiplserver import PrologMQI, PrologThread
from targets.swi.swi_model import SWIModel
from targets.solver_model import SolverModel
from utils.exceptions import SolverException
import tempfile
import re


@dataclass
class SWIBridge:
    # TODO: Handle N solutions
    def solve(self, model: SolverModel, n_sols: int = 1):
        if not isinstance(model, SWIModel):
            raise TypeError("the model must be a swi model")
        constraints = model.generate_program()
        # print(constraints)
        regex = re.compile(r"(UUID(?:_[a-f0-9]+){5})")
        with tempfile.NamedTemporaryFile(dir="/tmp", delete=True) as tmp:
            program = """:- use_module(library(clpfd)).

    program([!!!]) :-
    """
            program += "\n".join(constraints) + "."
            # print(program)
            occs = set(regex.findall(program))
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
                    # handle solution before returning
                    if result is False:
                        raise SolverException("CLIF/SWI -> UNSAT")
                    return result

    def update_model(self, model, rules, result):
        for e in model["elements"]:
            if e["type"] in rules["elementTypes"] and e["properties"][1][
                "value"
            ] not in ["Selected", "Unselected"]:
                e["properties"][1]["value"] = (
                    "SelectedForced"
                    if result[0]["UUID_" + str(e["id"]).replace("-", "_")] == 1
                    else "UnselectedForced"
                )
