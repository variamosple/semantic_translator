import uuid


def to_underscore_from_uuid(id: uuid.UUID) -> str:
    return str(id).replace("-", "_")


def to_uuid_from_underscore(id: str) -> uuid.UUID:
    return uuid.UUID(id.replace("_", "-"))