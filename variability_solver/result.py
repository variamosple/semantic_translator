from pydantic import BaseModel, Field


class Result(BaseModel, frozen=True):
    pass


class SATResult(Result, frozen=True):
    satisfiable: bool


class SolveResult(Result, frozen=True):
    solutions: list[dict[str, object]] = Field(default_factory=list)    
    
    
class IterateResult(Result, frozen=True):
    # example: [({"feature1": True, "feature2": False}, SATResult(satisfiable=True)), ...]
    iterations: list[tuple[dict[str, object], Result]]
    