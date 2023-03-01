from dataclasses import dataclass
from swiplserver import PrologMQI, PrologThread
from targets.swi.swi_model import SWIModel
from targets.solver_model import SolverModel
from utils.exceptions import SolverException
import tempfile
import re
import typing
import time
from variamos import model as mdl


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
            # run the query
            query_str = "program([!!!]), label([!!!]).".replace(
                "!!!", ",".join(occs)
            )
            return self.query(
                temp_file_name=tmp.name, query_str=query_str, n_sols=n_sols
            )

    # function to handle prolog queries
    def query(
        self, temp_file_name: str, query_str: str, n_sols: int
    ) -> list[dict[str, typing.Any]]:
        time_limit = 60.0
        sols = []
        with PrologMQI() as mqi:
            with PrologThread(mqi) as prolog_thread:
                print(temp_file_name)
                query_str = f"['{temp_file_name}']"
                print(query_str)
                prolog_thread.query(query_str + ".")
                print("file loaded")
                prolog_thread.query_async(query_str, find_all=False)
                # start python thread timer
                # if timer expires, kill the prolog thread
                start_time = time.perf_counter()
                print("starting timer ", start_time)
                while not (
                    (loop_time := time.perf_counter()) - start_time > time_limit
                ) and len(sols) < n_sols:
                    result = prolog_thread.query_async_result()
                    print("loop time ", loop_time)
                    if result is None or result is False:
                        break
                    else:  # result is a solution
                        if result is True:
                            raise SolverException("SWI Missing Variables")
                        # check that the result is a list, that its length is 1,
                        # and that the first element is a dict
                        if (
                            not isinstance(result, list)
                            and len(result) != 1
                            and not isinstance(result[0], dict)
                        ):
                            raise SolverException("SWI Invalid Result")
                        sols.append(result[0])
                # after the timer expires or the prolog thread finishes
                prolog_thread.stop()
        return sols

    def update_model(self, model: mdl.Model, rules, result):
        for e in model.elements:
            if e.type in rules.element_types and e.properties[1][
                "value"
            ] not in ["Selected", "Unselected"]:
                e.properties[1]["value"] = (
                    "SelectedForced"
                    if result[0]["UUID_" + str(e.id).replace("-", "_")] == 1
                    else "UnselectedForced"
                )
