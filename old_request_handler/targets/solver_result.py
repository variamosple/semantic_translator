from minizinc import Result
from dataclasses import dataclass

PrologSolution = bool | list[dict]
MiniZincSolution = list

@dataclass
class SolverResult:
    result: Result | PrologSolution
    
    def get_values()
