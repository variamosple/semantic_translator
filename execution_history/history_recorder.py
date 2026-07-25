import uuid
from datetime import UTC, datetime

from flask import g, request

from database import db
from execution_recorder.history_item import HistoryItem
from execution_recorder.history_repository import HistoryRepository


def init_history_recorder(app):

    @app.before_request
    def record_request():
        g.request_id = uuid.uuid4()
        g.occurred_at = datetime.now(UTC)
        g.request_path = request.path
        g.request_method = request.method
        g.request_content = request.get_json(silent=True) if request.is_json else None

    @app.after_request
    def record_response(response):
        g.response_status = response.status_code
        g.response_content = response.get_json(silent=True) if response.is_json else None

        HistoryRepository(database=db).save(
            HistoryItem(
                id=g.request_id,
                occured_at=g.occurred_at,
                url=g.request_path,
                request_method=g.request_method,
                request_content=g.request_content,
                response_status=g.response_status,
                response_content=g.response_content,
            )
        )

        return response
