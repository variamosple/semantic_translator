import dataclasses
import uuid
import networkx as nx
from variamos import model, rules
from utils import exceptions


@dataclasses.dataclass(init=False)
class CLIFGenerator:
    rule_set: rules.Rules
    # variamos_model: model.Model
    variamos_graph: nx.DiGraph
    _var_prefix = "UUID_"

    def __init__(
        self,
        rule_set: rules.Rules,
        # variamos_model: model.Model,
        variamos_graph: nx.DiGraph,
    ) -> None:
        self.rule_set = rule_set
        self.variamos_graph = variamos_graph

    def generate_logic_model(self):
        """Gen the CLIF string from the VMos Model and translation rules"""
        model_header, model_footer = "(model", ")"
        sentence_strings: list[str] = []
        # Since we have elements whose translation is related to the type of the
        # incoming relation, it makes sense to not only handle these with the
        # element, but make sure we do not regenerate the stuff twice, so we
        # avoid translating that in relationship mapping
        for element_id, element in self.variamos_graph.nodes.data("element"):  # type: ignore
            # Frist make the sentece for the element itself
            sentence_strings.append(
                self.generate_element_sentence(element_id=element_id, element=element)  # type: ignore
            )
            # Next make the sentence for the element's attributes
            sentence_strings.extend(
                self.generate_attribute_senteces(element=element)
            )
        for rel_id_in, rel_id_out, relationship in self.variamos_graph.edges.data("relation"):  # type: ignore
            if (
                generated_string := self.generate_relationship_sentence(
                    relationship=relationship
                )
            ) is not None:
                sentence_strings.append(generated_string)
        return "\n".join([model_header, *sentence_strings, model_footer])

    def generate_element_sentence(
        self, element_id: str, element: model.Element
    ) -> str:
        # Check if the element is a simple element
        if element.type in self.rule_set.element_types:
            # Here we must check if its type is defined by the incoming
            # relationship or whether it suffices to construct its declaration
            # by itself
            if (
                check_tuple := self.check_typing_relation(element_id=element_id)
            ) is not None:
                return self.generate_typing_relation_sentence(
                    element=element,
                    relation=check_tuple[0],
                    rel_type=check_tuple[1],
                )
            else:
                return self.generate_simple_element_sentence(element)
        # Check if the element is a relation reification element
        elif element.type in self.rule_set.relation_reification_types:
            return self.generate_reified_element_sentence(
                element_id=element_id, element=element
            )
        else:
            raise exceptions.SemanticException("Unknown element type")

    def check_typing_relation(
        self, element_id: str
    ) -> tuple[model.Relationship, str] | None:
        for (
            type,
            rule,
        ) in self.rule_set.typing_relation_translation_rules.items():
            for _, _, relation_data_dict in (  # type: ignore
                self.variamos_graph.in_edges(element_id, data=True)
                if rule.deriving_relation_inbound
                else self.variamos_graph.out_edges(element_id, data=True)
            ):
                # Determine if the type is buried in the properties of the element
                # and get it
                relationship: model.Relationship = relation_data_dict[
                    "relation"
                ]
                if "type" in (schema := self.rule_set.relation_property_schema):
                    type_schema = schema["type"]
                    idx, key = type_schema.index, type_schema.key
                    try:
                        relationship_type = relationship.properties[idx][key]
                    # FIXME: Make sure we handle these cases correctly
                    except IndexError:
                        return None
                    except KeyError:
                        return None
                else:
                    relationship_type = relationship.type
                if relationship_type == type:
                    return relationship, type
        # If after the iteration no match is found return None
        return None

    def generate_typing_relation_sentence(
        self,
        element: model.Element,
        relation: model.Relationship,
        rel_type: str,
    ) -> str:
        rule = self.rule_set.typing_relation_translation_rules[rel_type]
        cons = rule.constraint
        for param in rule.param:
            if param in rule.relationLookupSchema:
                schema = rule.relationLookupSchema[param]
                idx, key = schema.index, schema.key
                cons = cons.replace(param, relation.properties[idx][key])
        # TODO: Make this robust
        return cons.replace(
            "F1",
            model.to_underscore_from_uuid(
                relation.target_id
                if rule.deriving_relation_inbound
                else relation.source_id
            ),
        ).replace(
            "F2",
            model.to_underscore_from_uuid(
                relation.source_id
                if rule.deriving_relation_inbound
                else relation.target_id
            ),
        )

    def generate_attribute_senteces(self, element: model.Element) -> list[str]:
        sentences = []
        for property in (p for p in element.properties if p["custom"]):
            if (
                (p_t := property["type"]) not in self.rule_set.attribute_types
                or p_t not in self.rule_set.attribute_translation_rules
            ):
                raise exceptions.SemanticException(
                    "Unknown attribute type", property["type"]
                )
            rule = self.rule_set.attribute_translation_rules[p_t]
            constraint = rule.constraint.replace(
                rule.template,
                model.to_underscore_from_uuid(property[rule.param]),
            )
            sentences.append(constraint)
        return sentences

    def generate_reified_element_sentence(
        self, element_id: str, element: model.Element
    ) -> str | None:
        element_rule = self.rule_set.element_translation_rules[element.type]
        if not isinstance(element_rule, rules.ReifiedRelationElementRule):
            raise exceptions.SemanticException(
                "Reified Elements must have a corresponding rule"
            )
        # get in and out edges
        in_edges, out_edges = (
            list(self.variamos_graph.in_edges(element_id)),
            list(self.variamos_graph.out_edges(element_id)),
        )
        self.check_reified_element_relation_cardinality(element, element_rule)
        reified_relationship_type = ""
        # Determine if the type is buried in the properties of the element
        # and get it
        # We will also reuse the schema binding later on
        if "type" in (
            schema := self.rule_set.relation_reification_property_schema
        ):
            type_schema = schema["type"]
            idx, key = type_schema.index, type_schema.key
            try:
                reified_relationship_type = element.properties[idx][key]
            # FIXME: Make sure we handle these cases correctly
            except IndexError:
                return None
            except KeyError:
                return None
        else:
            reified_relationship_type = element.type
        # Get the constraint based on the type of reified relation
        cons = element_rule.constraint[reified_relationship_type]
        # Replace it with the pattern for the inbound edges
        cons = self.handle_edges(
            incoming=True,
            edges=in_edges,
            constraint=cons,
            element_rule=element_rule,
        )
        # Replace it with the pattern for the outbound edges
        cons = self.handle_edges(
            incoming=False,
            edges=out_edges,
            constraint=cons,
            element_rule=element_rule,
        )
        # Now that the expressions relating to the edges have been replaced
        # we must replace the expressions tied to properties in the bundle.
        # First check if there are these type dependent rules for the given
        # element type
        if element.type in (
            rtde := self.rule_set.relation_reification_type_dependent_expansions
        ) and reified_relationship_type in (type_lookups := rtde[element.type]):
            cons = self.handle_type_dependent_expansions(
                constraint=cons,
                element=element,
                lookup_schema=schema,
                lookups=type_lookups[reified_relationship_type],
            )
        # Finally return the constraint.
        return cons

    def handle_edges(
        self,
        incoming: bool,
        edges: list,
        constraint: str,
        element_rule: rules.ReifiedRelationElementRule,
    ) -> str:
        # Get the mapping depening on whether or not it's the incoming case
        param_mapping: rules.MappingConfig = (
            element_rule.param_mapping.inbound_edges
            if incoming
            else element_rule.param_mapping.outbound_edges
        )
        # The cardinalities have already been checked by the
        # check_reified_element_relation_cardinality method
        # First we deal with the basic case of a single variable
        if param_mapping.unique:
            if incoming:
                node_uuid, _ = edges[0]
            else:
                _, node_uuid = edges[0]
            binding_var = param_mapping.var
            node_uuid = self._var_prefix + model.to_underscore_from_uuid(
                node_uuid
            )
            return constraint.replace(binding_var, node_uuid)
        # Handle case where the node refies multiple edges coming out
        else:
            # Now we must deal with collapsing these edges according to
            # them being incoming or not...
            tuple_idx = 0 if incoming else 1
            node_uuids: list[uuid.UUID] = [
                edge_tuple[tuple_idx] for edge_tuple in edges
            ]
            # Now add the prefixes and set their
            node_str_uuids: list[str] = [
                self._var_prefix + model.to_underscore_from_uuid(node_id)
                for node_id in node_uuids
            ]
            binding_var = param_mapping.var
            return self.clif_expression_expansion(
                node_str_uuids, binding_var, constraint
            )

    def clif_expression_expansion(
        self, node_str_uuids: list[str], binding_var: str, constraint: str
    ) -> str:
        # FIXME: This needs better handling than just a string lookup
        # I should also consider building a cleaner representation
        if (
            binding_var
            not in self.rule_set.relation_reification_expansions["params"]
        ):
            raise exceptions.SemanticException(
                "No declared parameter for fun expansion"
            )
        # FIXME: As before, this should be a structured representation
        for func in self.rule_set.relation_reification_expansions["functions"]:
            expansion_expression = f"{func}({binding_var})"
            # Now we must calculate the replacement
            replacement: str
            match func:
                case "sum":
                    replacement = " + ".join(node_str_uuids)
                case "len":
                    replacement = str(len(node_str_uuids))
                case _:
                    raise exceptions.SemanticException("Unimplemented function")
            # Now we must iteratively replace the constraint expression
            constraint = constraint.replace(expansion_expression, replacement)
        # Now render the completed constraint
        return constraint

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

    def handle_type_dependent_expansions(
        self,
        constraint: str,
        element: model.Element,
        lookup_schema: dict[str, rules.RelationPropertyLookupRule],
        lookups: list[str],
    ) -> str:
        for lookup in lookups:
            type_schema = lookup_schema[lookup]
            idx, key = type_schema.index, type_schema.key
            replacement = element.properties[idx][key]
            constraint = constraint.replace(lookup, replacement)
        return constraint

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
            # FIXME: Make sure we handle these cases correctly
            except IndexError:
                return None
            except KeyError:
                return None
        else:
            relationship_type = relationship.type
        # Make sure it's part of the declared relation types
        if relationship_type not in self.rule_set.relation_types:
            # check if its a type deriving relation
            if relationship_type in self.rule_set.typing_relation_types:
                return None
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
        for (p, node_uuid) in zip(relation_rule.params, endpoints):
            cons = cons.replace(p, node_uuid)
        return cons
