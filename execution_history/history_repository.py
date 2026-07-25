from psycopg.types.json import Jsonb

from database import Database

from .history_item import HistoryItem


class HistoryRepository:
    def __init__(self, database: Database) -> None:
        self._database = database

    def save(self, item: HistoryItem) -> None:
        """Save a history item to the database."""
        with self._database.cursor() as cur:
            cur.execute(
                """
                INSERT INTO history (
                    id,
                    occured_at,
                    url,
                    request_method,
                    request_content,
                    response_status,
                    response_content
                )
                VALUES (%s, %s, %s, %s, %s, %s, %s)
                """,
                (
                    item.id,
                    item.occured_at,
                    item.url,
                    item.request_method,
                    Jsonb(item.request_content),
                    item.response_status,
                    Jsonb(item.response_content),
                ),
            )
