import humps


def from_camelcase(string):
    return humps.camelize(string)
