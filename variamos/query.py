import pydantic
from enum import Enum
from utils import enums


class OperationEnum(str, Enum):
    sat = "sat"
    solve = "solve"
    nsolve = "nsolve"


class ModelObjectEnum(str, Enum):
    element = "element"
    relationship = "relationship"
    reified = "reified"


class RelationElementEnum(str, Enum):
    target = "target"
    source = "source"


# ReifiedElementSelector = dict[RelationElementEnum, int]


class ModelSelectorSpec(pydantic.BaseModel):
    model_object: ModelObjectEnum
    relationship_element: RelationElementEnum | None  # | ReifiedElementSelector
    object_type: list[str]


IterationSpec = list[ModelSelectorSpec]


class Query(pydantic.BaseModel):
    operation: OperationEnum
    operation_n: pydantic.PositiveInt | None
    iterate_over: IterationSpec | None
    with_value: int | None
    solver: enums.TargetLang
