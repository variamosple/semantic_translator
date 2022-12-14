from abc import ABC, abstractmethod
from grammars import clif


class SolverModel(ABC):
    @abstractmethod
    def generate_program(self):
        pass

    @abstractmethod
    def fix_variable(self, variable: str, value: int):
        pass

    @classmethod
    # @abstractmethod
    def from_clif_text(cls, clif_model: clif.Text):
        pass
