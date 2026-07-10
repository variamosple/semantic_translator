from pydantic import BaseModel

from .constraints import Formula
from .variables import Variable


class Model(BaseModel, frozen=True):
    variables: list[Variable]
    constraints: list[Formula]