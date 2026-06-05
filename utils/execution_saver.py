from datetime import datetime
import os
from typing import Any, Optional

from dotenv import load_dotenv
import psycopg2

load_dotenv()

class Execution:
    """
    Represents a single execution.
    
    Attributes:
        remote_addr (str): The IP address of the client.
        timestamp (datetime): The timestamp of the execution.
        query (str): The query that was executed (JSON string).
        result (str): The result of the execution (JSON string).
        statistics (str): The statistics of the execution (JSON string).
    """
    def __init__(
        self,
        remote_addr: str,
        timestamp: datetime,
        query: str,
        result: str,
        statistics: str,
    ) -> None:
        self.remote_addr = remote_addr
        self.timestamp = timestamp
        self.query = query
        self.result = result
        self.statistics = statistics

    @classmethod
    def table_schema(cls, schema: str = "public") -> str:
        """Return PostgreSQL table schema for storing executions."""
        return f"CREATE TABLE IF NOT EXISTS {schema}.executions (id SERIAL PRIMARY KEY, remote_addr VARCHAR(255) NOT NULL, timestamp TIMESTAMP NOT NULL, query TEXT NOT NULL, result TEXT NOT NULL, statistics TEXT NOT NULL);"

    @classmethod
    def from_dict(cls, data: dict) -> "Execution":
        """Create an Execution object from a dictionary (e.g., from database row)."""
        return cls(
            remote_addr=str(data.get("remote_addr", "unknown")),
            timestamp=datetime.fromisoformat(data.get("timestamp", "1970-01-01T00:00:00")),
            query=str(data.get("query", "{}")),
            result=str(data.get("result", "{}")),
            statistics=str(data.get("statistics", "{}")),
        )

    @classmethod
    def insert_query(cls, schema: str = "public") -> str:
        """Return the PostgreSQL INSERT query."""
        return f"INSERT INTO {schema}.executions (remote_addr, timestamp, query, result, statistics) VALUES (%s, %s, %s, %s, %s)"

    def to_dict(self) -> dict[str, Any]:
        """Convert the Execution object to a dictionary for database storage."""
        return {
            "remote_addr": self.remote_addr,
            "timestamp": self.timestamp,
            "query": self.query,
            "result": self.result,
            "statistics": self.statistics,
        }

    def to_insert_params(self) -> tuple[Any, ...]:
        """Return the parameters for an INSERT statement."""
        return (
            self.remote_addr,
            self.timestamp,
            self.query,
            self.result,
            self.statistics,
        )

    def __repr__(self) -> str:
        return f"Execution(remote_addr={self.remote_addr}, timestamp={self.timestamp}, query={self.query}, result={self.result}, statistics={self.statistics})"

    def __str__(self) -> str:
        return self.__repr__()

class ExecutionSaver:
    """
    Class for saving execution data to PostgreSQL database.
    
    Attributes:
        db_config: Dictionary containing database connection parameters
        schema: PostgreSQL schema name
        conn: PostgreSQL connection object
    """
    
    def __init__(self) -> None:
        """
        Initialize the ExecutionSaver for PostgreSQL.
        """
        required_vars = ["DB_HOST", "DB_PORT", "DB_NAME", "DB_USER", "DB_PASSWORD", "DB_SCHEMA"]
        for var in required_vars:
            if var not in os.environ:
                raise ValueError(f"Missing required environment variable: {var}")
        
        self.db_config: dict[str, str | int] = {
            "host": os.environ["DB_HOST"],
            "port": int(os.environ["DB_PORT"]),
            "database": os.environ["DB_NAME"],
            "user": os.environ["DB_USER"],
            "password": os.environ["DB_PASSWORD"],
        }
        self.schema: str = os.environ["DB_SCHEMA"]
        self.conn: Optional[psycopg2.extensions.connection] = None
        self._connect()
        self._ensure_schema_exists()
        self._ensure_table_exists()
    
    def _connect(self) -> None:
        """Establish database connection."""
        try:
            self.conn = psycopg2.connect(**self.db_config)
        except psycopg2.Error as e:
            raise RuntimeError(f"Failed to connect to database: {e}")
    
    def _check_connection(self) -> None:
        """Check if connection is alive, reconnect if needed."""
        if self.conn is None or self.conn.closed:
            self._connect()
    
    def _ensure_schema_exists(self) -> None:
        """Ensure the schema exists in the database."""
        try:
            with self.conn.cursor() as cursor:
                cursor.execute(f"CREATE SCHEMA IF NOT EXISTS {self.schema}")
            self.conn.commit()
        except psycopg2.Error as e:
            self.conn.rollback()
            raise RuntimeError(f"Failed to create schema: {e}")
    
    def __enter__(self) -> "ExecutionSaver":
        """Context manager entry."""
        return self
    
    def __exit__(self, exc_type: Optional[type], exc_val: Optional[BaseException], exc_tb: Optional[object]) -> None:
        """Context manager exit - close connection."""
        self.close()
    
    def close(self) -> None:
        """Close the database connection."""
        if self.conn:
            self.conn.close()
            self.conn = None
    
    def _ensure_table_exists(self) -> None:
        """
        Ensure the executions table exists in the database.
        """
        self._check_connection()
        try:
            with self.conn.cursor() as cursor:
                cursor.execute(Execution.table_schema(self.schema))
            self.conn.commit()
        except psycopg2.Error as e:
            self.conn.rollback()
            raise RuntimeError(f"Failed to create table: {e}")
    
    def save_execution(self, execution: Execution) -> None:
        """
        Save an execution to the database.
        
        Args:
            execution: Execution object to save
        """
        self._check_connection()
        
        try:
            with self.conn.cursor() as cursor:
                cursor.execute(Execution.insert_query(self.schema), execution.to_insert_params())
            self.conn.commit()
        except psycopg2.Error as e:
            self.conn.rollback()
            raise RuntimeError(f"Failed to save execution: {e}")