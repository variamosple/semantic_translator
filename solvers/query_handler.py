import networkx as nx
from variamos import query, model, rules
from utils import enums
from utils import exceptions
from solvers import solver_control
from grammars import clif


class QueryHandler:
    query_obj: query.Query
    controller: solver_control.SolverController
    translation_rules: rules.Rules

    def __init__(
        self,
        model_obj: model.Model,
        nx_graph: nx.DiGraph,
        query_obj: query.Query,
        clif_model: clif.Text,
        translation_rules: rules.Rules,
    ) -> None:
        self.query_obj = query_obj
        self.translation_rules = translation_rules
        # build the controller
        self.controller = solver_control.SolverController(
            target_lang=query_obj.solver,
            clif_model=clif_model,
            vmos_model=model_obj,
            vmos_graph=nx_graph,
            translation_rules=translation_rules,
        )

    def run_query(self, project_json, idx, feature_model):
        # there can either be iteration or not
        if (i_spec := self.query_obj.iterate_over) is not None:
            # This branch implies that we need to handle the iterative
            # execution of the solver
            # TODO: Check for empty iteration spec
            # TODO: Figure out how to connect deeply to the solver
            # TODO: Make sure we are covering the different cases of how the
            # vars are constructed
            return self.controller.run_iteration_operation(
                iteration_spec=i_spec
            )
        # If not handle the operation to be executed
        # Pydantic lets us assume all the other types are ok
        else:
            match self.query_obj.operation:
                case query.OperationEnum.sat:
                    return self.controller.sat()
                case query.OperationEnum.solve:
                    # FIXME: We do not yet handle multiple product lines
                    self.controller.update_model(
                        fm=feature_model,
                        rules=self.translation_rules,
                        result=self.controller.solve_one(),
                    )
                    project_json["productLines"][0]["domainEngineering"][
                        "models"
                    ][idx] = feature_model
                    return project_json
                case query.OperationEnum.nsolve:
                    return self.controller.solve_n(self.query_obj.operation_n)
