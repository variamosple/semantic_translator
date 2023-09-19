from __future__ import annotations
from dataclasses import dataclass
import uuid
import minizinc
import enum
from utils import uuid_utils


@dataclass
class Solution:
    single_solution: bool
    solutions: list[dict[str, int]]

    @staticmethod
    def _clean_id(id: str) -> str:
        if "UUID_" not in id:
            return id
        return str(uuid_utils.to_uuid_from_underscore(id.split("_", 1)[1]))

    @classmethod
    def from_swi_output(cls, swi_solution: list[dict[str, int]]) -> Solution:
        """Create a Solution from the output of a SWI-Prolog solver"""
        single_solution = len(swi_solution) == 1
        solutions = [
            {Solution._clean_id(var[0]): var[1] for var in solution.items()}
            for solution in swi_solution
        ]
        for solution in solutions:
            print(solution)
        return Solution(single_solution=single_solution, solutions=solutions)

    @classmethod
    def from_minizinc_output(cls, mzn_solution: minizinc.Result) -> Solution:
        """Create a Solution from the output of a Minizinc solver"""
        single_solution = len(mzn_solution) == 1
        if single_solution:
            # If there is a single solution, but it was requested with the
            # n_sols argument, the solution is returned as a list of length 1
            if isinstance(mzn_solution.solution, list):
                sol = mzn_solution.solution[0]
            else:
                sol = mzn_solution.solution
            solutions = [
                {
                    Solution._clean_id(var[0]): var[1]
                    for var in vars(sol).items()
                }
            ]
        else:
            solutions = [
                {
                    Solution._clean_id(var[0]): var[1]
                    for var in vars(solution).items()
                }
                for solution in mzn_solution
            ]
        for solution in solutions:
            print(solution)
        return Solution(single_solution=single_solution, solutions=solutions)

    @classmethod
    def from_z3_output(cls, z3_solution: list[dict[str, int]]) -> Solution:
        single_solution = len(z3_solution) == 1
        solutions = [
            {Solution._clean_id(var[0]): var[1] for var in solution.items()}
            for solution in z3_solution
        ]
        for solution in solutions:
            print(solution)
        return Solution(single_solution=single_solution, solutions=solutions)


# string enum for the status of the solver
class StatusEnum(str, enum.Enum):
    SATISFIED = "satisfied"
    UNSATISFIED = "unsatisfied"
    UNKNOWN = "unknown"


@dataclass
class Result:
    """Represents the results of a solver

    Attributes:
        solution: The solution found by the solver
        status: The status of the solver
    """

    solution: Solution | None
    status: StatusEnum

    # Generate a result from the output of a Minizinc solver
    @classmethod
    def from_minizinc_output(cls, mzn_result: minizinc.Result) -> Result:
        """Create a Result from the output of a Minizinc solver"""
        if mzn_result.status.has_solution():
            status = StatusEnum.SATISFIED
            solution = Solution.from_minizinc_output(mzn_result)
            return Result(solution=solution, status=status)
        else:
            status = StatusEnum.UNSATISFIED
            return Result(solution=None, status=status)

    # Generate a result from the output of the SWI-Prolog solver
    @classmethod
    def from_swi_output(cls, swi_result: list[dict[str, int]]) -> Result:
        """Create a Result from the output of the SWI-Prolog solver"""
        if len(swi_result) > 0:
            status = StatusEnum.SATISFIED
            solution = Solution.from_swi_output(swi_solution=swi_result)
            return Result(solution=solution, status=status)
        else:
            status = StatusEnum.UNSATISFIED
            return Result(solution=None, status=status)

    @classmethod
    def from_z3_output(cls, z3_result: list[dict[str, int]]) -> Result:
        if len(z3_result) > 0:
            status = StatusEnum.SATISFIED
            solution = Solution.from_z3_output(z3_solution=z3_result)
            return Result(solution=solution, status=status)
        else:
            status = StatusEnum.UNSATISFIED
            return Result(solution=None, status=status)
