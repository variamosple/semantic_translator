from pydantic import BaseModel, Field


class Result(BaseModel, frozen=True):
    pass


class SATResult(Result, frozen=True):
    satisfiable: bool


class SolveResult(Result, frozen=True):
    solutions: list[dict[str, object]] = Field(default_factory=list)


class Iteration(BaseModel, frozen=True):
    values: dict[str, object]
    result: SATResult


class IterateResult(Result, frozen=True):
    # example: [
    #     {
    #         "values": {"feature1": True, "feature2": False},
    #         "result": SATResult(satisfiable=True),
    #     },
    #     ...
    # ]
    iterations: list[Iteration]
