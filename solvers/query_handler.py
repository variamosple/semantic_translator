from variamos import query
from utils import enums
from utils import exceptions
from solvers import solver_control
from grammars import clif


class QueryHandler:
    query_obj: query.Query
    controller: solver_control.SolverController

    def __init__(
        self,
        query_obj: query.Query,
        clif_model: clif.Text,
    ) -> None:
        self.query_obj = query_obj
        # build the controller
        self.controller = solver_control.SolverController(
            target_lang=query_obj.solver, clif_model=clif_model
        )

    def run_query(self):
        # there can either be iteration or not
        if (i_spec := self.query_obj.iterate_over) is not None:
            # This branch implies that we need to handle the iterative
            # execution of the solver
            # TODO: Figure out how to connect deeply to the solver
            # TODO: Make sure we are covering the different cases of how the
            # vars are constructed
            self.controller.set_iteration_parameters(iteration_spec=i_spec)
            # TODO: Check for empty iteration spec
            # make sure there is a with value spec if iteration is specified
            if (v_spec := self.query_obj.with_value) is None:
                raise exceptions.QueryException("Empty with_value spec")
            self.controller.set_iteration_variable_fix(value=v_spec)
        # Now handle the operation to be executed
        # Pydantic lets us assume all the other types are ok
        match self.query_obj.operation:
            case query.OperationEnum.sat:
                return self.controller.sat()
            case query.OperationEnum.solve:
                return self.controller.solve_one()
            case query.OperationEnum.nsolve:
                return self.controller.solve_n(self.query_obj.operation_n)
