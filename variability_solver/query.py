from enum import Enum

from pydantic import BaseModel


class Operation(str, Enum):
    CHECK_SATISFIABILITY = "sat"
    SOLVE = "solve"
    OPTIMIZE = "optimize"


class Direction(str, Enum):
    MAXIMIZE = "maximize"
    MINIMIZE = "minimize"


class IterationRule(BaseModel, frozen=True):
    """
    Rule for iterating over elements of the same kind with specific values.
    It represents the cartesian product of the values with the elements.
    All rules are then treated as cartesian products of each other.

    Example:
        Rule 1: elements=["feature1", "feature2"], values=[True, False]
        Rule 2: elements=["maxCost"], values=[100, 200, 300]

        This will generate (2 * 2) * 3 = 12 combinations.
    """

    elements: list[str]
    values: list[object]


class Query(BaseModel, frozen=True):
    operation: Operation
    limit: int | None = None
    target: str | None = None
    direction: Direction | None = None
    iteration_rules: list[IterationRule] | None = None