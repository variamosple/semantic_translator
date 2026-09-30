import os
import time
import resource
from datetime import datetime, timezone
from typing import Literal, Optional, TypedDict, Tuple, Any

HealthStatus = Literal["UP", "DEGRADED", "DOWN"]

class SystemMemoryInfo(TypedDict):
    usedMb: float
    totalMb: float
    percentage: float

class DependencyCheck(TypedDict, total=False):
    status: HealthStatus
    latencyMs: float
    message: Optional[str]

class HealthChecks(TypedDict):
    database: DependencyCheck
    memory: SystemMemoryInfo

class HealthPayload(TypedDict):
    status: HealthStatus
    serviceName: str
    version: str
    uptimeSeconds: int
    timestamp: str
    responseTimeMs: float
    checks: HealthChecks

_SERVICE_START_TIME = time.time()

def check_database() -> DependencyCheck:
    t0 = time.time()
    try:
        from database import db
        with db.cursor() as cur:
            cur.execute("SELECT 1")
            cur.fetchone()
        latency_ms = round((time.time() - t0) * 1000, 2)
        return {"status": "UP", "latencyMs": latency_ms}
    except Exception as exc:
        latency_ms = round((time.time() - t0) * 1000, 2)
        return {"status": "DOWN", "latencyMs": latency_ms, "message": str(exc)}

def get_system_memory() -> SystemMemoryInfo:
    try:
        usage = resource.getrusage(resource.RUSAGE_SELF)
        used_mb = round(usage.ru_maxrss / 1024.0, 2)

        total_mb = 0.0
        if os.path.exists("/proc/meminfo"):
            with open("/proc/meminfo", "r") as f:
                for line in f:
                    if line.startswith("MemTotal:"):
                        total_kb = float(line.split()[1])
                        total_mb = round(total_kb / 1024.0, 2)
                        break

        percent = round((used_mb / total_mb * 100), 1) if total_mb > 0 else 0.0
    except Exception:
        used_mb, total_mb, percent = 0.0, 0.0, 0.0

    return {
        "usedMb": used_mb,
        "totalMb": total_mb,
        "percentage": percent,
    }

def get_health_status() -> Tuple[HealthPayload, int]:
    req_start = time.time()
    db_check = check_database()
    mem_info = get_system_memory()

    overall_status: HealthStatus = "UP" if db_check["status"] == "UP" else "DEGRADED"
    status_code = 200 if overall_status == "UP" else 503

    payload: HealthPayload = {
        "status": overall_status,
        "serviceName": "semantic_translator",
        "version": os.getenv("APP_VERSION", "1.0.0"),
        "uptimeSeconds": int(time.time() - _SERVICE_START_TIME),
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "responseTimeMs": round((time.time() - req_start) * 1000, 2),
        "checks": {
            "database": db_check,
            "memory": mem_info,
        },
    }

    return payload, status_code
