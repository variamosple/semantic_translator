from collections.abc import Iterator
from contextlib import contextmanager

from psycopg import Connection
from psycopg.rows import dict_row
from psycopg_pool import ConnectionPool

from config import DB_URL


class Database:
    def __init__(
        self,
        conninfo: str,
        *,
        min_size: int = 2,
        max_size: int = 10,
    ) -> None:
        self._pool = ConnectionPool(
            conninfo=conninfo,
            min_size=min_size,
            max_size=max_size,
            open=False,
        )

    def open(self) -> None:
        self._pool.open()

    def close(self) -> None:
        self._pool.close()

    @contextmanager
    def connection(self) -> Iterator[Connection]:
        with self._pool.connection() as conn:
            yield conn

    @contextmanager
    def cursor(self) -> Iterator:
        with self.connection() as conn:
            with conn.cursor(row_factory=dict_row) as cur:
                yield cur


db = Database(conninfo=DB_URL)
