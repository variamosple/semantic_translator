from abc import ABC, abstractmethod

from variability_solver.ir.model import Model


class Frontend(ABC):
    """
    Abstract base class for all frontends.
    """

    @property
    @abstractmethod
    def name(self) -> str:
        """
        Human-readable name of the frontend.
        """
        raise NotImplementedError

    @property
    @abstractmethod
    def description(self) -> str:
        """
        Description of the frontend.
        """
        raise NotImplementedError

    @abstractmethod
    def parse(self, input_str: str) -> Model:
        """
        Parse the input string and return a Model.
        """
        raise NotImplementedError
