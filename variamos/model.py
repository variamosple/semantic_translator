import pydantic
import typing
import uuid
import networkx as nx

from solvers import results
from utils import camel_handler


class Relationship(pydantic.BaseModel):
    id: uuid.UUID
    type: str
    name: str
    source_id: uuid.UUID
    target_id: uuid.UUID
    properties: list[dict[str, typing.Any]]
    # extra stuff
    points: list
    min: int
    max: int

    class Config:
        alias_generator = camel_handler.from_camelcase

    def to_nx_rel_attrs(self):
        return {"id": self.id, "properties": self.properties}


class Element(pydantic.BaseModel):
    id: uuid.UUID
    type: str
    name: str
    parent_id: typing.Optional[uuid.UUID]
    properties: list[dict[str, typing.Any]]
    # Visual components
    x: int
    y: int
    width: int
    height: int

    class Config:
        alias_generator = camel_handler.from_camelcase

    def to_nx_node_attrs(self):
        return {
            "type": self.type,
            "name": self.name,
            "parent_id": self.parent_id,
            "properties": self.properties,
        }


class Model(pydantic.BaseModel):
    id: uuid.UUID
    name: str
    elements: list[Element]
    relationships: list[Relationship]
    constraints: str = ""

    class Config:
        alias_generator = camel_handler.from_camelcase

    def construct_graph(self):
        G = nx.DiGraph(constraints=self.constraints)
        for e in self.elements:
            G.add_node(e.id, element=e)
        for r in self.relationships:
            G.add_edge(r.source_id, r.target_id, relation=r)
        return G

    # Handle updating selections from results
    def update_selections(self, solution: dict[str, int]):
        # get the solution
        for elem in self.elements:
            # check if the element is in the solution and that it has a
            # selection property
            if (
                str(elem.id) in solution
                and (
                    # get a reference to the selection property
                    # if it exists otherwise None
                    sel_prop := next(
                        (
                            p
                            for p in elem.properties
                            if p["name"] == "Selected"
                        ),
                        None,
                    )
                )
                is not None
            ):
                # check the value in the solution
                if solution[str(elem.id)] == 1:
                    sel_prop["value"] = "SelectedForced"
                else:
                    sel_prop["value"] = "UnselectedForced"
