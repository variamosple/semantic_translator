import pydantic
import typing
from utils import camel_handler


class SimpleElementRule(pydantic.BaseModel):
    param: str
    constraint: str
    selected_constraint: str
    deselected_constraint: str

    class Config:
        alias_generator = camel_handler.from_camelcase


class MappingConfig(pydantic.BaseModel):
    unique: bool
    var: str


class ReifiedRelationParameterMapping(pydantic.BaseModel):
    inbound_edges: MappingConfig
    outbound_edges: MappingConfig

    class Config:
        alias_generator = camel_handler.from_camelcase


class ReifiedRelationElementRule(pydantic.BaseModel):
    param: list[str]
    param_mapping: ReifiedRelationParameterMapping
    constraint: dict[str, str]

    class Config:
        alias_generator = camel_handler.from_camelcase


class RelationRule(pydantic.BaseModel):
    params: list[str]
    constraint: str

    class Config:
        alias_generator = camel_handler.from_camelcase


class RelationPropertyLookupRule(pydantic.BaseModel):
    index: int
    key: str


class Rules(pydantic.BaseModel):
    element_types: list[str]
    element_translation_rules: dict[
        str, typing.Union[SimpleElementRule, ReifiedRelationElementRule]
    ]
    relation_reification_types: list[str]
    relation_reification_expansions: dict[str, list[str]]
    relation_reification_property_schema: dict[str, RelationPropertyLookupRule]
    relation_types: list[str]
    relation_property_schema: dict[str, RelationPropertyLookupRule]
    relation_translation_rules: dict[str, RelationRule]

    class Config:
        alias_generator = camel_handler.from_camelcase
