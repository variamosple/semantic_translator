from pydantic import BaseModel

# ======================================================================
# Base classes
# ======================================================================


class Sort(BaseModel, frozen=True):
    pass


class Domain(BaseModel, frozen=True):
    pass


class Variable(BaseModel, frozen=True):
    var_id: int
    name: str
    sort: Sort
    domain: Domain


# ======================================================================
# Sorts
# ======================================================================


class BoolSort(Sort, frozen=True):
    pass


class IntSort(Sort, frozen=True):
    pass


# ======================================================================
# Domains
# ======================================================================


class FullDomain(Domain, frozen=True):
    pass


class IntervalDomain(Domain, frozen=True):
    lower: int | float | None
    upper: int | float | None


class FiniteDomain(Domain, frozen=True):
    values: frozenset
