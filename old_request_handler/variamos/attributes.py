import math
import re
import unicodedata

Z3_LANG = 'z3'
PROLOG_LANG = 'prolog'
MINIZINC_LANG = 'minizinc'

PREFIX_VARIABLE_TRANSLATION = {
    Z3_LANG: "a_",
    PROLOG_LANG: "_",
    MINIZINC_LANG: "a_",
}

ATTRIBUTE_PROPERTY_TYPE = 'String'

def get_selected_solution(element_id, solution):
    solution_key = str(element_id)
    selected_solution = solution[solution_key]
    if type(selected_solution) != int:
        return selected_solution.as_long()
    
    return solution[solution_key]

# A range of values, as the PLE lets people write it: "[0..100]", "0..10000",
# "[0.00..10.00]".
_RANGE = re.compile(
    r"^\s*\[?\s*(-?\d+(?:\.\d+)?)\s*\.\.\s*(-?\d+(?:\.\d+)?)\s*\]?\s*$"
)


def parse_range(possible_values) -> tuple[int, int] | None:
    """Bounds of a range of possible values, or None if it is not a range.

    The solvers work on integers: decimal bounds are widened to the nearest
    integers that contain the range.
    """
    if not isinstance(possible_values, str):
        return None
    match = _RANGE.match(possible_values)
    if match is None:
        return None
    lower = math.floor(float(match.group(1)))
    upper = math.ceil(float(match.group(2)))
    if lower > upper:
        return None
    return lower, upper


def enum_token(value: str) -> str:
    """Name of a possible value in the logic model.

    The grammar only accepts names (letters, digits, "_") and integers in an
    enumeration: accents are removed, other characters become "_", and a
    name that would start with a digit gets a "v_" prefix.
    """
    ascii_value = (
        unicodedata.normalize("NFKD", value).encode("ascii", "ignore").decode()
    )
    token = re.sub(r"[^A-Za-z0-9_]+", "_", ascii_value).strip("_")
    if token.isdigit():
        return token
    if not token or token[0].isdigit():
        return "v_" + token
    return token


def split_possible_values(possible_values) -> list[str]:
    """Possible values of an attribute, in order, without duplicates.

    People separate them with "," or ";" and put spaces around them. Two
    values with the same name in the logic model are the same value.
    """
    if not isinstance(possible_values, str) or parse_range(possible_values):
        return []
    values: list[str] = []
    tokens: set[str] = set()
    for part in re.split(r"[,;]", possible_values):
        value = part.strip()
        token = enum_token(value).lower() if value else ""
        if value and token not in tokens:
            values.append(value)
            tokens.add(token)
    return values


def translate_from(current_solution, solution):
    solution_value = solution[str(current_solution["id"])]
    possible_values = current_solution["possibleValues"]
    values = split_possible_values(possible_values)
    if type(solution_value) == int and current_solution["type"] == "Boolean":
        return "true" if solution_value == 1 else "false"
    elif type(solution_value) == int and values and 0 <= solution_value < len(values):
        return values[solution_value].lower()
    elif str(solution_value)[:2] == "a_":
        token = str(solution_value)[2:]
        for value in values:
            if enum_token(value).lower() == token:
                return value.lower()
        return token

    return str(solution_value)
    
def translate_to(lang: str, values: list[str]) -> list[str]:
    prefix = PREFIX_VARIABLE_TRANSLATION[lang]
    return ['{}{}'.format(prefix, str(v).lower()) for v in values]
