import dataclasses
import networkx as nx
from variamos import model, rules
from utils import exceptions


@dataclasses.dataclass(init=False)
class CLIFGenerator:
    rule_set: rules.Rules
    variamos_model: model.Model
    variamos_graph: nx.DiGraph

    def __init__(
        self, rule_set: rules.Rules, variamos_model: model.Model
    ) -> None:
        self.rule_set = rule_set
        self.variamos_model = variamos_model
        self.variamos_graph = variamos_model.construct_graph()

    def generate_logic_model(self):
        """Gen the CLIF string from the VMos Model and translation rules"""
        model_header, model_footer = "(model", ")"
        sentence_strings: list[str] = []
        for element in self.variamos_model.elements:
            sentence_strings.append(
                self.generate_element_sentence(element=element)
            )
        for relationship in self.variamos_model.relationships:
            if (
                generated_string := self.generate_relationship_sentence(
                    relationship=relationship
                )
            ) is not None:
                sentence_strings.append(generated_string)
        return "\n".join([model_header, *sentence_strings, model_footer])

    def generate_element_sentence(self, element: model.Element) -> str:
        # Check if the element is a simple element
        if element.type in self.rule_set.element_types:
            return self.generate_simple_element_sentence(element)
        # Check if the element is a relation reification element
        elif element.type in self.rule_set.relation_reification_types:
            return self.generate_reified_element_sentence(element)
        else:
            raise exceptions.SemanticException("Unknown element type")

    def generate_reified_element_sentence(self, element: model.Element) -> str:
        element_rule = self.rule_set.element_translation_rules[element.type]
        if not isinstance(element_rule, rules.ReifiedRelationElementRule):
            raise exceptions.SemanticException(
                "Reified Elements must have a corresponding rule"
            )
        # get in and out edges
        in_edges, out_edges = (
            self.variamos_graph.in_edges(element.id),
            self.variamos_graph.out_edges(element.id),
        )
        self.check_reified_element_relation_cardinality(element, element_rule)
        reified_relationship_type = ""
        # Determine if the type is buried in the properties of the element
        # and get it
        if "type" in (schema := self.rule_set.relation_property_schema):
            type_schema = schema["type"]
            idx, key = type_schema.index, type_schema.key
            try:
                reified_relationship_type = element.properties[idx][key]
            except KeyError:
                return None
        else:
            reified_relationship_type = element.type
        # Get the constraint based on the type of reified relation
        cons = element_rule.constraint[reified_relationship_type]
        

    def clif_expression_expansion(self):
        pass

    def check_reified_element_relation_cardinality(
        self,
        element: model.Element,
        element_rule: rules.ReifiedRelationElementRule,
    ):
        if (
            element_rule.param_mapping.inbound_edges.unique
            and self.variamos_graph.in_degree(element.id) > 1  # type: ignore
        ):
            raise exceptions.SemanticException(
                "There can only be one incoming edge"
            )
        if (
            element_rule.param_mapping.outbound_edges.unique
            and self.variamos_graph.out_degree(element.id) > 1  # type: ignore
        ):
            raise exceptions.SemanticException(
                "There can only be one incoming edge"
            )

    def generate_simple_element_sentence(self, element: model.Element):
        element_rule = self.rule_set.element_translation_rules[element.type]
        if not isinstance(element_rule, rules.SimpleElementRule):
            raise exceptions.SemanticException(
                "Simple elements must have an element translation rule"
            )
        return element_rule.constraint.replace(
            element_rule.param, model.to_underscore_from_uuid(element.id)
        )

    def generate_relationship_sentence(
        self, relationship: model.Relationship
    ) -> str | None:
        relationship_type = ""
        # Determine if the type is buried in the properties of the relation
        # and get it
        if "type" in (schema := self.rule_set.relation_property_schema):
            type_schema = schema["type"]
            idx, key = type_schema.index, type_schema.key
            try:
                relationship_type = relationship.properties[idx][key]
            except KeyError:
                return None
        else:
            relationship_type = relationship.type
        # Make sure it's part of the declared relation types
        if relationship_type not in self.rule_set.relation_types:
            raise exceptions.SemanticException(
                "The given relationship has not been declared"
            )
        try:
            relation_rule = self.rule_set.relation_translation_rules[
                relationship_type
            ]
        except KeyError:
            raise exceptions.SemanticException(
                "No constraint translation rule for given type"
            )
        if relationship.source_id is None or relationship.target_id is None:
            raise exceptions.SemanticException(
                "Neither the source nor the target can be empty"
            )
        endpoints = (
            model.to_underscore_from_uuid(relationship.source_id),
            model.to_underscore_from_uuid(relationship.target_id),
        )
        cons = relation_rule.constraint
        for (p, uuid) in zip(relation_rule.params, endpoints):
            cons = cons.replace(p, uuid)
        return cons
