from variamos.model import Model, Relationship, Element
import networkx as nx


class FeatureModelRelationship(Relationship):
    def is_cross_tree_constraint(self):
        return len((p0 := self.properties[0])) > 0 and p0["value"] in [
            "Includes",
            "Excludes",
        ]


class FeatureModeElement(Element):
    pass


class FeatureModel(Model):
    elements: list[FeatureModeElement]
    relationships: list[FeatureModelRelationship]

    def get_optional_relationships(self):
        return list(
            filter(
                lambda r: len(r.properties) > 0
                and r.properties[0]["value"] == "Optional",
                self.relationships,
            )
        )

    def get_bundles(self):
        return list(filter(lambda e: e.type == "Bundle", self.elements))

    def check_tree_structure(self, G: nx.DiGraph):
        def is_cross_tree_edge(e1, e2) -> bool:
            return not (
                (rel := G.get_edge_data(e1, e2)) is not None
                and len((ps := rel["properties"])) > 0
                and ps[0]["value"]
                in [
                    "Includes",
                    "Excludes",
                ]
            )

        view = nx.subgraph_view(
            G=G, filter_edge=is_cross_tree_edge  # pyright: ignore
        )

        return nx.is_tree(view)
