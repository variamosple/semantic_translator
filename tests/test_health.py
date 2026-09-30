import unittest
from unittest.mock import patch
from health import get_health_status

class HealthTestCase(unittest.TestCase):
    @patch("health.check_database")
    def test_health_status_up(self, mock_check_db):
        mock_check_db.return_value = {"status": "UP", "latencyMs": 4.5}
        
        payload, status_code = get_health_status()
        
        self.assertEqual(status_code, 200)
        self.assertEqual(payload["status"], "UP")
        self.assertEqual(payload["serviceName"], "semantic_translator")
        self.assertEqual(payload["checks"]["database"]["status"], "UP")
        self.assertGreaterEqual(payload["checks"]["memory"]["usedMb"], 0)

    @patch("health.check_database")
    def test_health_status_degraded(self, mock_check_db):
        mock_check_db.return_value = {"status": "DOWN", "latencyMs": 50.0, "message": "Database error"}
        
        payload, status_code = get_health_status()
        
        self.assertEqual(status_code, 503)
        self.assertEqual(payload["status"], "DEGRADED")
        self.assertEqual(payload["checks"]["database"]["status"], "DOWN")
        self.assertEqual(payload["checks"]["database"]["message"], "Database error")

if __name__ == "__main__":
    unittest.main()
