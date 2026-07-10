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

def translate_from(current_solution, solution):
    solution_value = solution[str(current_solution["id"])]
    possible_values = current_solution["possibleValues"]
    if type(solution_value) == int and current_solution["type"] == "Boolean":
        return "true" if solution_value == 1 else "false"
    elif type(solution_value) == int and possible_values:
        return possible_values.split(",")[solution_value].lower()
    elif str(solution_value)[:2] == "a_":
        return str(solution_value).replace("a_", "")
    
    return str(solution_value)
    
def translate_to(lang: str, values: list[str]) -> list[str]:
    prefix = PREFIX_VARIABLE_TRANSLATION[lang]
    return ['{}{}'.format(prefix, str(v).lower()) for v in values]
