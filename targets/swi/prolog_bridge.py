from dataclasses import dataclass
from swiplserver import PrologMQI, PrologThread, PrologResultNotAvailableError
from targets.swi.swi_model import SWIModel
from targets.solver_model import SolverModel
from utils.exceptions import SolverException
import tempfile
import re
import typing
import time
from variamos import model as mdl
from variamos import query


@dataclass
class SWIBridge:
    """
    This class is a bridge between the SWI Prolog server and the rest of the
    application. It is used for running queries and solving models. To avoid
    having to manage a long-lived connection to the server, this class uses
    temporary files to store the program to be queried.

    It uses the async query interface of the server, which allows for setting
    a timeout and for looking for arbritrary numbers of solutions.
    """

    def translate(self, model: SWIModel):
        """
        This function takes a model and returns a string representing the
        program to be queried.

        @model: The model to be translated
        """
        constraints = model.generate_program()
        regex = re.compile(r"(UUID(?:_[a-f0-9]+){5})")
        program = """:- use_module(library(clpfd)).
program([!!!]) :-
"""
        program += "\n".join(constraints) + "."
        # print(program)
        occs = set(regex.findall(program))
        print(occs)
        program = program.replace("!!!", ",".join(occs))
        return program

    # TODO: Handle N solutions
    # TODO: Remove the regex and use the model to get the variables
    def solve(self, model: SolverModel, n_sols: int = 1):
        """
        This function takes a model and a number of solutions to find, and
        returns a list of solutions. Each solution is a dictionary mapping
        variable names to their values. Though it may also return an empty list
        if the model is unsatisfiable or nothing is found within the timeout.

        This function uses a temporary file to store the program to be queried,
        and a helper function to handle the actual query.

        @model: The model to be solved
        @n_sols: The number of solutions to find
        """
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
            query_str = "program([!!!]), labeling([ff],[!!!]).".replace(
                "!!!", ",".join(occs)
            )
            return self.query(
                temp_file_name=tmp.name, query_str=query_str, n_sols=n_sols
            )

    # TODO: Merge with solve to avoid code duplication
    def optimize(
        self,
        model: SolverModel,
        objective: str,
        direction: query.OptimizationDirectionEnum,
        n_sols: int = 1,
    ):
        """
        This function takes a model, an objective, a direction, and a number of
        solutions to find, and returns a list of solutions. Each solution is a
        dictionary mapping variable names to their values. Though it may also
        return an empty list if the model is unsatisfiable or nothing is found
        within the timeout.

        This function uses a temporary file to store the program to be queried,
        and a helper function to handle the actual query.

        @model: The model to be solved
        @objective: The objective to optimize
        @direction: The direction to optimize the objective
        @n_sols: The number of solutions to find
        """
        if not isinstance(model, SWIModel):
            raise TypeError("the model must be a swi model")
        # get all the variables in the model and join them with commas
        # as a string
        model_vars = ",".join(model.var_decls.keys())
        constraints = model.generate_program()
        with tempfile.NamedTemporaryFile(dir="/tmp", delete=True) as tmp:
            program = f""":- use_module(library(clpfd)).

    program([{model_vars}]) :-
    """
            program += "\n".join(constraints) + "."
            print(program)
            tmp.write(program.encode())
            tmp.flush()
            # Seek file for reading
            tmp.seek(0)
            # run the query
            # Check the optimization direction
            if direction == query.OptimizationDirectionEnum.min:
                query_str = (
                    f"program([{model_vars}]), "
                    f"labeling([min({objective})],[{model_vars}])."
                )
            elif direction == query.OptimizationDirectionEnum.max:
                query_str = (
                    f"program([{model_vars}]), "
                    f"labeling([max({objective})],[{model_vars}])."
                )
            else:
                raise ValueError("invalid optimization direction")
            return self.query(
                temp_file_name=tmp.name, query_str=query_str, n_sols=n_sols
            )

    # function to handle prolog queries
    def query(
        self, temp_file_name: str, query_str: str, n_sols: int
    ) -> list[dict[str, typing.Any]]:
        """
        This function takes a file name, a query string, and a number of
        solutions to find, and returns a list of solutions. Each solution is a
        dictionary mapping variable names to their values. Though it may also
        return an empty list if the model is unsatisfiable or nothing is found
        within the timeout.

        It handles the actual query, and uses a python thread timer to set a
        timeout. If the timeout expires, the prolog thread is killed and only
        the solutions found up to that point are returned. The prolog thread
        only lives for the duration of the query and there is therefore a
        performance penalty that comes with using this function, but it
        provides a way to avoid having to manage a long-lived connection to
        the server.

        @temp_file_name: The name of the file to be queried
        @query_str: A string representing the query to be run
        @n_sols: The number of solutions to find
        """
        time_limit = 60.0
        sols = []
        with PrologMQI() as mqi:
            with PrologThread(mqi) as prolog_thread:
                print(temp_file_name)
                load_query_str = f"['{temp_file_name}']"
                print(load_query_str)
                prolog_thread.query(load_query_str + ".")
                print("file loaded")
                prolog_thread.query_async(query_str, find_all=False)
                # start python thread timer
                # if timer expires, kill the prolog thread
                start_time = time.perf_counter()
                print("starting timer ", start_time)
                while (
                    not (
                        (loop_time := time.perf_counter()) - start_time
                        > time_limit
                    )
                    and len(sols) < n_sols
                ):
                    try:
                        result = prolog_thread.query_async_result(60)
                    except PrologResultNotAvailableError:
                        raise SolverException("something went wrong with the query")
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
