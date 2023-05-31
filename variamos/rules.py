import pydantic
import typing
from utils import camel_handler


class EnumParameterMapping(pydantic.BaseModel):
    var: str
    attribute: str

    class Config:
        alias_generator = camel_handler.from_camelcase


class SimpleElementRule(pydantic.BaseModel):
    param: str
    constraint: str
    enum_mapping: EnumParameterMapping | None
    selected_constraint: str | None
    deselected_constraint: str | None

    class Config:
        alias_generator = camel_handler.from_camelcase


class MappingConfig(pydantic.BaseModel):
    unique: bool
    var: str


class ReifiedRelationParameterMapping(pydantic.BaseModel):
    node: str | None
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


class AttributeTranslationRule(pydantic.BaseModel):
    parent: str
    param: str
    template: str
    constraint: str
    value: str | None


class HierarchyNodeParameterMapping(pydantic.BaseModel):
    incoming: bool
    var: str
    node: str


class HierarchyNodeRule(pydantic.BaseModel):
    param: list[str]
    param_mapping: HierarchyNodeParameterMapping
    constraint: str

    class Config:
        alias_generator = camel_handler.from_camelcase


class HierarchyTranslationRule(pydantic.BaseModel):
    # There may be a need for a rule for root nodes too
    node_rule: HierarchyNodeRule
    leaf_rule: SimpleElementRule

    class Config:
        alias_generator = camel_handler.from_camelcase


class Rules(pydantic.BaseModel):
    # Simple elements that require only their own information
    element_types: list[str]
    element_translation_rules: dict[
        str,
        SimpleElementRule,
    ]
    # Handling for element attributes
    attribute_types: list[str]
    attribute_translation_rules: dict[str, AttributeTranslationRule]
    # Relation Typed Elements
    typing_relation_types: list[str]
    typing_relation_translation_rules: dict[str, RelationTypedElementRule]
    # Handling for types whose constraints only depend on the hierachical
    # structure of relations with elements of the same type
    # and thus can also have other relations that are translated in other
    # manners
    hierarchy_types: list[str]
    hierarchy_translation_rules: dict[str, HierarchyTranslationRule]
    # Elements that reify a complex many-to-many relationship
    relation_reification_types: list[str]
    relation_reification_translation_rules: dict[
        str, ReifiedRelationElementRule
    ]
    relation_reification_expansions: dict[str, list[str]]
    relation_reification_property_schema: dict[str, RelationPropertyLookupRule]
    relation_reification_type_dependent_expansions: dict[
        str, dict[str, list[str]]
    ]
    # Simple relations that require only their own information
    relation_types: list[str]
    relation_property_schema: dict[str, RelationPropertyLookupRule]
    relation_translation_rules: dict[str, RelationRule]
    # Relations we ignore because we generate the stuff elsewhere
    ignored_relation_types: list[str] | None
    # Since we have some symbols we'd rather remove...
    symbol_map: dict[str, str] | None

    class Config:
        alias_generator = camel_handler.from_camelcase
