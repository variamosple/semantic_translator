from enum import Enum, unique


class StrEnum(str, Enum):
    pass


@unique
class TargetLang(StrEnum):
    minizinc = "minizinc"
    swi = "swi"
