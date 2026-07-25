from dataclasses import dataclass
from datetime import datetime
from uuid import UUID


@dataclass
class HistoryItem:
    id: UUID
    occured_at: datetime
    url: str
    request_method: str
    request_content: dict | None
    response_status: int
    response_content: dict | None
