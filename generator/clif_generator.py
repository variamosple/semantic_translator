import dataclasses
import uuid
import networkx as nx
import re
from variamos import model, rules
from utils import exceptions
from textx import metamodel_from_file, get_children_of_type
from grammars import clif


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
        ################################################
        # HACK: To accomodate the stuff relating to the complex constraints
        # we will gather all the ids and names in a data structure.
        names_ids = []
        for element_id, element in self.variamos_graph.nodes.data("element"):  # type: ignore
            # Frist make the sentece for the element itself
            sentence_strings.append(
                self.generate_element_sentence(
                    element_id=element_id, element=element  # type: ignore
                )
            )
            # Next make the sentence for the element's attributes
            sentence_strings.extend(
                self.generate_attribute_senteces(element=element)
            )
            # HACK: add the name property into the above data structure
            # HACK: turn off checks
            names_ids.append((element.name, element_id))  # pyright: ignore
        # Handle relationship generation
        for rel_id_in, rel_id_out, relationship in self.variamos_graph.edges.data("relation"):  # type: ignore
            if (
                generated_string := self.generate_relationship_sentence(
                    relationship=relationship
                )
            ) is not None:
                sentence_strings.append(generated_string)
        # Handle attribute accesses from the complex constraints
        if (arb_constraints := self.variamos_graph.graph["constraints"]) != "":
            self.generate_arbitrary_constraint_sentences(
                arb_constraints=arb_constraints, names_ids=names_ids
            )
        return "\n".join(
            [model_header, *sentence_strings, arb_constraints, model_footer]
        )

    # TODO: We should have it return and not be a side-effect on arb_constraints
    def generate_arbitrary_constraint_sentences(
        self,
        arb_constraints: str,
        names_ids: list[tuple[str, uuid.UUID]],
    ) -> None:
        # We need to do extra stuff whenever we encounter arbitrary
        # constraints such that we can replace normal stuff with the correct
        # variable names (UUIDs)
        # We are going to do a simple hack for now
        # HACK: We merely gather element names above and replace anywhere
        # they are found
        # TODO: Make this robust, capable of checking and so on...
        print("######################################################")
        print(names_ids)
        print("######################################################")
        # We now construct the regular expression that finds attribute
        # accesses with the '::' character.
        # TODO: replace regex string juggling with partial parsing???
        pattern = re.compile(r"\b(\w+)::(\w+)\b")
        matches: list[tuple[str, str]] = pattern.findall(arb_constraints)
        matches_dict: dict[str, str] = dict(matches)
        for name, id in names_ids:
            if name in arb_constraints:
                # Before we do the replacement we must handle the
                # attribute access
                # Check that the name is the first element of the matches
                if name in matches_dict:
                    props: list = self.variamos_graph.nodes[id][
                        "element"
                    ].properties
                    # TODO: Check what happens when there are multiple attributes # noqa: E501
                    # found flags whether we did find the attribute
                    found = False
                    for prop in props:
                        if prop["name"] == matches_dict[name]:
                            # Replace the expression with the matching id
                            expression = name + "::" + prop["name"]
                            arb_constraints = arb_constraints.replace(
                                expression,
                                "UUID_"
                                + model.to_underscore_from_uuid(prop["id"]),
                            )
                            found = True
                    # Here if we arrive at the end fo the iteration and we
                    # have not found any matches in the for loop
                    # it means it is not part of the property list of the
                    # element and must be an error
                    if not found:
                        raise exceptions.SemanticException(
                            f"The {matches_dict[name]} attribute does not belong to {name}"  # noqa: E501
                        )
                # Now that we have handled all the attribute accesses we can perform the simple
                # replacements
                # NOTE: This probably means that this will not work correctly if an attribute
                # shares a name with a feature...
                arb_constraints = arb_constraints.replace(
                    name, "UUID_" + model.to_underscore_from_uuid(id)
                )
                # TODO: Do the regex logic to handle attribute selection

    def generate_element_sentence(
        self, element_id: uuid.UUID, element: model.Element
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
        elif element.type in self.rule_set.hierarchy_types:
            return self.generate_hierarchical_sentence(
                element_id=element_id, element=element
            )
        else:
            raise exceptions.SemanticException(
                "Unknown element type:", element.type
            )

    def generate_hierarchical_sentence(
        self, element_id: uuid.UUID, element: model.Element
    ) -> str:
        if element.type not in self.rule_set.hierarchy_translation_rules:
            raise exceptions.SemanticException(
                "Missing translation rule for ", element.type
            )
        rule = self.rule_set.hierarchy_translation_rules[element.type]
        # TODO: Check this is sound, shouldn't it only be
        # for outgoing nodes?
        # Now we must perform an analysis whether our node is a leaf node
        # w.r.t. the hierarchy or merely a node on the hierarchy.
        # The simplest way to do this is to check the parameter mapping config
        # and check whether there are in/out edges of the corresponding type
        # rule.node_rule.param_mapping.incoming
        if rule.node_rule.param_mapping.incoming:
            edges = self.variamos_graph.in_edges(element_id, data="relation")
            is_node = any(
                self.variamos_graph.nodes.data("element")[source].type
                == element.type
                for (source, _, _) in edges
            )
        else:
            edges = self.variamos_graph.out_edges(element_id, data="relation")
            is_node = any(
                self.variamos_graph.nodes.data("element")[target].type
                == element.type
                for (_, target, _) in edges
            )
        if is_node:
            cons = rule.node_rule.constraint
            # Replace self
            cons = cons.replace(
                rule.node_rule.param_mapping.node,
                model.to_underscore_from_uuid(element.id),
            )
            # Now run the expression expansion
            tuple_idx = 0 if rule.node_rule.param_mapping.incoming else 1
            # HACK: We will trim node_uuids such that we only leave those of a given type
            # when the request is made, since we are dealing with hierarchies
            # TODO: maybe make this more robust
            node_uuids: list[tuple[uuid.UUID, model.Relationship]] = [
                (edge_tuple[tuple_idx], edge_tuple[2])  # type: ignore
                for edge_tuple in edges
                if self.variamos_graph.nodes.data("element")[  # type: ignore
                    edge_tuple[tuple_idx]
                ].type  # type: ignore
                == element.type
            ]
            # We can reuse the expansion rules for complex expressions
            cons = self.clif_expression_expansion(
                node_uuids=node_uuids,
                binding_var=rule.node_rule.param_mapping.var,
                constraint=cons,
                same_type=True,
            )
            return cons
        else:
            cons = rule.leaf_rule.constraint
            return cons.replace(
                rule.leaf_rule.param, model.to_underscore_from_uuid(element.id)
            )

    def check_typing_relation(
        self, element_id: uuid.UUID
    ) -> tuple[model.Relationship, str] | None:
        for (
            type,
            rule,
        ) in self.rule_set.typing_relation_translation_rules.items():
            for _, _, relation_data_dict in (  # type: ignore
                self.variamos_graph.in_edges(element_id, data="relation")  # type: ignore # noqa: E501
                if rule.deriving_relation_inbound
                else self.variamos_graph.out_edges(element_id, data="relation")  # type: ignore # noqa: E501
            ):
                # Determine if the type is buried in the properties of the element
                # and get it
                relationship: model.Relationship = relation_data_dict  # type: ignore # noqa: E501
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
        self, element_id: uuid.UUID, element: model.Element
    ) -> str | None:
        element_rule = self.rule_set.relation_reification_translation_rules[
            element.type
        ]
        if not isinstance(element_rule, rules.ReifiedRelationElementRule):
            raise exceptions.SemanticException(
                "Reified Elements must have a corresponding rule"
            )
        # get in and out edges
        in_edges, out_edges = (
            list(
                self.variamos_graph.in_edges(
                    element_id, data="relation"  # type: ignore
                )
            ),
            list(
                self.variamos_graph.out_edges(
                    element_id, data="relation"  # type: ignore
                )
            ),
        )
        # Perform a check on the parameter config or raise exception
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
        # HACK: FIXME: Since we do partial parses for some part of the analysis,
        # and we have term expansions, this can break the analysis done on
        # some portions, so we must make sure that the expansions have been
        # done before we go ahead and make the partial parse
        # The hacky way we will do this is order the calls as a function of
        # what's in the rules.
        if element_rule.param_mapping.inbound_edges.unique:
            # Replace it with the pattern for the outbound edges
            cons = self.handle_edges(
                incoming=False,
                edges=out_edges,
                constraint=cons,
                element_rule=element_rule,
            )
            # Replace it with the pattern for the inbound edges
            cons = self.handle_edges(
                incoming=True,
                edges=in_edges,
                constraint=cons,
                element_rule=element_rule,
            )
        else:
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

        # Check if there's a self node expresssion and if so add the constraint
        if element_rule.param_mapping.node is not None:
            cons = cons.replace(
                element_rule.param_mapping.node,
                # TODO: Check that these types are correct
                self._var_prefix + model.to_underscore_from_uuid(element_id),
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
        edges: list[tuple[uuid.UUID, uuid.UUID, model.Relationship]],
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
                node_uuid, _, relation = edges[0]
            else:
                _, node_uuid, relation = edges[0]
            binding_var = param_mapping.var
            node_uuid = self._var_prefix + model.to_underscore_from_uuid(
                node_uuid
            )
            # We must now do the same lookup analysis as before
            # We must do the partial parse as before
            # But this time we need not go so deep
            # TODO: Refactor and reuse the common code to both procedures
            # This time we do a simpler lookup because all we need is the one
            # lookup
            expanded_cons = "(model " + constraint + " )"
            lookup_mm = clif.clif_meta_model()
            lookup_ast: clif.Text = lookup_mm.model_from_str(expanded_cons)
            lookup_instances: list[clif.AttributeLookup] = get_children_of_type(
                clif.AttributeLookup, lookup_ast
            )
            # Find the instance corresponding to the node we're interest in
            lookup_instance = next(
                (
                    li
                    for li in lookup_instances
                    if li.element.element == binding_var
                ),
                None,
            )
            if lookup_instance is not None:
                expression = expanded_cons[lookup_instance._tx_position : lookup_instance._tx_position_end]  # type: ignore # noqa: E501
                if (fun := lookup_instance.element.fun) is not None:
                    if fun == "edge":
                        # Perform attribute lookup on
                        # the edge's properties
                        attribute = next(
                            (
                                prop["value"]
                                for prop in relation.properties
                                if prop["name"] == lookup_instance.attribute
                            ),
                            None,
                        )
                        if attribute is not None:
                            # HACK: We are doing this so the damn thing compiles
                            if (
                                self.rule_set.symbol_map is not None
                                and attribute in self.rule_set.symbol_map
                            ):
                                attribute = self.rule_set.symbol_map[attribute]
                            constraint = constraint.replace(
                                expression, attribute
                            )
                        else:
                            raise exceptions.SemanticException(
                                f"The {attribute}"
                                " attribute does not belong to"
                                f" {relation.type} relations"  # noqa: E501
                            )
                    else:
                        raise exceptions.SemanticException(
                            "Unknown function type", fun
                        )
            return constraint.replace(binding_var, node_uuid)
        # Handle case where the node refies multiple edges coming out
        else:
            # Now we must deal with collapsing these edges according to
            # them being incoming or not...
            tuple_idx = 0 if incoming else 1
            node_uuids: list[tuple[uuid.UUID, model.Relationship]] = [
                (edge_tuple[tuple_idx], edge_tuple[2]) for edge_tuple in edges  # type: ignore
            ]
            binding_var = param_mapping.var
            return self.clif_expression_expansion(
                node_uuids, binding_var, constraint
            )

    def clif_expression_expansion(
        self,
        node_uuids: list[tuple[uuid.UUID, model.Relationship]],
        binding_var: str,
        constraint: str,
        same_type: bool = False,
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
        # Now we must deal with the hairy problem of dealing with the term
        # the term expansion of foralls in the constraint so we can do
        # the equivalent of loop unfolding
        # First we check if there is a forall in the expression
        # Otherwise proceed as usual
        # TODO: Handle this logic in the correct place
        if "forall" in constraint:
            # HACK: we will run a partial parse on this expression
            # such that we can do the loop unfolding before handing it over
            # to the code generators, because otherwise we would need to
            # create some quite complex logic for both SWI+MZN
            expanded_cons = "(model " + constraint + " )"
            forall_mm = clif.clif_meta_model()
            forall_ast: clif.Text = forall_mm.model_from_str(expanded_cons)
            forall_instances: list[clif.QuantSentence] = get_children_of_type(
                clif.QuantSentence, forall_ast
            )
            new_constraint = constraint[:]
            for instance in forall_instances:
                # perform a check on the bindings, since we only have one set to
                # draw from
                if len(instance.boundlist.vars) != 1:
                    raise exceptions.SemanticException(
                        "Forall unfolding failed, only one binding allowed per expression"
                    )
                # make sure we are doing the unfolding for the right expression
                if instance.boundlist.vars[0].set_from != binding_var:
                    break
                sub_var = instance.boundlist.vars[0].var
                # get the forall subexpression
                start: int = instance._tx_position  # type: ignore
                end: int = instance._tx_position_end  # type: ignore
                expr = expanded_cons[start:end]
                # get the inner expression
                start_inner: int = instance.sentence._tx_position  # type: ignore # noqa: E501
                end_inner: int = instance.sentence._tx_position_end  # type: ignore # noqa: E501
                inner_expr = expanded_cons[start_inner:end_inner]
                # Use the parsed instance to find attribute lookups
                lookup_instances: list[
                    clif.AttributeLookup
                ] = get_children_of_type(clif.AttributeLookup, instance)
                lookup_map: dict[str, dict[str, str | None]] = {}
                # HACK: This only works once
                for lo in lookup_instances:
                    lookup_map[lo.element.element] = {
                        "fun": lo.element.fun,
                        "attribute": lo.attribute,
                        "expression": expanded_cons[lo._tx_position : lo._tx_position_end],  # type: ignore # noqa: E501
                    }
                # Now make the list of inner expression expanded out
                inner_expresion_list: list[str] = []
                for (node_uuid, relation) in node_uuids:
                    new_inner_expr = inner_expr[:]
                    # Handle the expansion of the expression inside the generated
                    # inner expressions
                    if sub_var in lookup_map:
                        attr_lookup = lookup_map[sub_var]
                        if (fun := attr_lookup["fun"]) is not None:
                            if fun == "edge":
                                # Perform attribute lookup on
                                # the edge's properties
                                attribute: str | None = next(
                                    (
                                        prop["value"]
                                        for prop in relation.properties
                                        if prop["name"]
                                        == attr_lookup["attribute"]
                                    ),
                                    None,
                                )
                                if (
                                    attribute is not None
                                    and attr_lookup["expression"] is not None
                                ):
                                    # HACK: We are doing this so the damn thing compiles
                                    if (
                                        self.rule_set.symbol_map is not None
                                        and attribute
                                        in self.rule_set.symbol_map
                                    ):
                                        attribute = self.rule_set.symbol_map[
                                            attribute
                                        ]
                                    new_inner_expr = new_inner_expr.replace(
                                        attr_lookup["expression"], attribute
                                    )
                                else:
                                    raise exceptions.SemanticException(
                                        f"The {attr_lookup['attribute']}"
                                        " attribute does not belong to"
                                        f" {relation.type} relations"  # noqa: E501
                                    )
                            else:
                                raise exceptions.SemanticException(
                                    "Unknown function type", fun
                                )
                    # Once the attribute lookups have been handled,
                    # put up replace the rest of the ocurrences of the
                    # quantified var
                    inner_expresion_list.append(
                        new_inner_expr.replace(
                            sub_var,
                            self._var_prefix
                            + model.to_underscore_from_uuid(
                                node_uuid
                            ),  # type: ignore # noqa: E501
                        )
                    )
                # now construct the replacement expression
                new_expr = "(and " + " ".join(inner_expresion_list) + ")"
                new_constraint = new_constraint.replace(expr, new_expr)
            constraint = new_constraint
            # Now handle the expansion of attribute lookups with functions
        #############################
        # FIXME: As before, this should be a structured representation
        node_str_uuids: list[str] = [
            self._var_prefix + model.to_underscore_from_uuid(node_id)
            for (node_id, _) in node_uuids
        ]
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
            # HACK: This ignores the non-property having relations when one
            # derives the type from the properties and just happened to work
            # fine for feature models
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
            # check if its ignored
            if (
                self.rule_set.ignored_relation_types is not None
                and relationship_type in self.rule_set.ignored_relation_types
            ):
                return None
            raise exceptions.SemanticException(
                "The given relationship has not been declared:",
                relationship_type,
                relationship.name,
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
