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


class RelationTypedElementRule(pydantic.BaseModel):
    param: list[str]
    relationLookupSchema: dict[str, RelationPropertyLookupRule]
    deriving_relation_inbound: bool
    constraint: str
    class Config:
        alias_generator = camel_handler.from_camelcase


class Rules(pydantic.BaseModel):
    # Simple elements that require only their own information
    element_types: list[str]
    element_translation_rules: dict[
        str,
        typing.Union[SimpleElementRule, ReifiedRelationElementRule],
    ]
    # Relation Typed Elements
    typing_relation_types: list[str]
    typing_relation_translation_rules: dict[str, RelationTypedElementRule]
    # Elements that reify a complex many-to-many relationship
    relation_reification_types: list[str]
    relation_reification_expansions: dict[str, list[str]]
    relation_reification_property_schema: dict[str, RelationPropertyLookupRule]
    relation_reification_type_dependent_expansions: dict[
        str, dict[str, list[str]]
    ]
    # Simple relations that require only their own information
    relation_types: list[str]
    relation_property_schema: dict[str, RelationPropertyLookupRule]
    relation_translation_rules: dict[str, RelationRule]

    class Config:
        alias_generator = camel_handler.from_camelcase
