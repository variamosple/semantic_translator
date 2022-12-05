from grammars.clif import Text
from targets.minzinc.minizinc_bridge import MiniZincBridge
from targets.minzinc.minizinc_model import clif_to_MZN_objects
from targets.solver_model import SolverModel
from targets.swi.prolog_bridge import SWIBridge
from targets.swi.swi_model import clif_to_SWI_objects
from utils.enums import TargetLang
from utils.exceptions import SolverException


class SolverController:
    """A class defining a unified control mechanism for the solvers.

    Attributes:
        target_lang (str):
    """
    constraint_model: SolverModel

    def __init__(self, target_lang, clif_model: Text) -> None:
        self.target_lang = target_lang
        match target_lang:
            case TargetLang.minizinc:
                self.constraint_model = clif_to_MZN_objects(clif_model)
                self.bridge = MiniZincBridge()
            case TargetLang.swi:
                self.constraint_model = clif_to_SWI_objects(clif_model)
                self.bridge = SWIBridge()
            case _:
                raise TypeError("Language unsupported")

    # For now we will work under the assumption that the value is an integer
    def sat_with_value(self, variable: str, value: int):
        self.constraint_model.fix_variable(variable, value)
        return self.sat()

    def sat(self) -> bool:
        try:
            self.solve_one()
            return True
        except SolverException:
            return False

    def solve_one(self):
        return self.solve_n(1)

    def solve_n(self, n_sols):
        # Maybe improve the api to make the typecheck pass
        return self.bridge.solve(self.constraint_model, n_sols)

    def update_model(self, fm, rules, result):
        self.bridge.update_model(fm, rules, result)

    # def do_query(self, query: Query):
    #     if query.n_sols > 0:
    #         result = self.solve(query.n_sols)
    #         query.v_to_bind[query.sol_var] = result
