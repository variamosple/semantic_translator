from abc import ABC, abstractmethod


class SolverModel(ABC):
    @abstractmethod
    def generate_program(self):
        pass

    @abstractmethod
    def fix_variable(self, variable: str, value: int):
        pass
