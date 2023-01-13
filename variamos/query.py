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
