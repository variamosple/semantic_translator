import os

from dotenv import load_dotenv

load_dotenv()


def get_env(name: str) -> str:
    value = os.getenv(name)
    if value is None:
        raise RuntimeError(f"Missing required environment variable: {name}")
    return value


DB_DIALECT = get_env("SEMANTIC_TRANSLATOR_DB_DIALECT")
DB_HOST = get_env("SEMANTIC_TRANSLATOR_DB_HOST")
DB_PORT = get_env("SEMANTIC_TRANSLATOR_DB_PORT")
DB_NAME = get_env("SEMANTIC_TRANSLATOR_DB_NAME")
DB_SCHEMA = get_env("SEMANTIC_TRANSLATOR_DB_SCHEMA")
DB_USER = get_env("SEMANTIC_TRANSLATOR_DB_USER")
DB_PASSWORD = get_env("SEMANTIC_TRANSLATOR_DB_PASSWORD")

DB_URL = f"{DB_DIALECT}://{DB_USER}:{DB_PASSWORD}@{DB_HOST}:{DB_PORT}/{DB_NAME}?options=-csearch_path%3D{DB_SCHEMA}"
