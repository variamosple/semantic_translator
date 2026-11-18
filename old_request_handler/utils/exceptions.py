class SemanticException(Exception):
    pass


class QueryException(Exception):
    pass


class SolverException(Exception):
    pass


class InvalidModelException(Exception):
    """The model sent by the user cannot be translated as it is written.

    Its message is meant for the user: the handler answers 400 with it.
    """
