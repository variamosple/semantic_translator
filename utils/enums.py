from enum import Enum, unique


class StrEnum(str, Enum):
    pass


@unique
class TargetLang(StrEnum):
    minizinc = "minizinc"
    swi = "swi"
    z3 = "z3"

@unique
class InputEnum(StrEnum):
    vmos = "vmos"
    uvl = "uvl"