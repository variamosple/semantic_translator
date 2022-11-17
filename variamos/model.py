import pydantic
import typing
import uuid

import utils


class Relationship(pydantic.BaseModel):
    id: uuid.UUID
    type: str
    name: str
    source_id: typing.Optional[uuid.UUID]
    target_id: typing.Optional[uuid.UUID]
    properties: list[dict[str, typing.Any]]

    class Config:
        alias_generator = utils.from_camelcase


class Element(pydantic.BaseModel):
    id: uuid.UUID
    type: str
    name: str
    parent_id: typing.Optional[uuid.UUID]
    properties: list[dict[str, typing.Any]]

    class Config:
        alias_generator = utils.from_camelcase


class Model(pydantic.BaseModel):
    id: uuid.UUID
    name: str
    elements: list[Element]
    relationships: list[Relationship]

    class Config:
        alias_generator = utils.from_camelcase
