import pydantic
from enum import Enum
from utils import enums


class OptimizationDirectionEnum(str, Enum):
    min = "min"
    max = "max"


class OperationEnum(str, Enum):
    sat = "sat"
    solve = "solve"
    nsolve = "nsolve"
    get_model = "get_model"
    optimize = "optimize"


class ModelObjectEnum(str, Enum):
    element = "element"
    relationship = "relationship"
    relationship_element = "relationship_element"
    reified = "reified"


class RelationElementEnum(str, Enum):
    target = "target"
    source = "source"


# ReifiedElementSelector = dict[RelationElementEnum, int]


class ModelSelectorSpec(pydantic.BaseModel):
    model_object: ModelObjectEnum
    relationship_element: RelationElementEnum | None  # | ReifiedElementSelector
    object_type: list[str]
    with_value: int | None


IterationSpec = list[ModelSelectorSpec]


class Query(pydantic.BaseModel):
    operation: OperationEnum
    operation_n: pydantic.PositiveInt | None
    iterate_over: IterationSpec | None
    solver: enums.TargetLang
    optimization_target: str | None
    optimization_direction: OptimizationDirectionEnum | None
