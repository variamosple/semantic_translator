from abc import ABC, abstractmethod

from variability_solver.ir.model import Model
from variability_solver.options import Options
from variability_solver.query import Query
from variability_solver.result import Result


class Backend(ABC):
    """
    Abstract class for all backends.
    Backend are bridges from the IR Model to their family-specific representation.
    """

    @property
    @abstractmethod
    def name(self) -> str:
        """
        Human-readable name of the backend.
        """
        raise NotImplementedError

    @property
    @abstractmethod
    def description(self) -> str:
        """
        Description of the backend.
        """
        raise NotImplementedError

    @abstractmethod
    def execute(self, model: Model, query: Query, options: Options) -> Result:
        """
        Execute the model with the given query.
        """
        raise NotImplementedError
