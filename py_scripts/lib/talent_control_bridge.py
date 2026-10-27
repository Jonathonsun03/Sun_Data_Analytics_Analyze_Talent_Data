"""Small authenticated HTTP adapter to the existing lifecycle authority."""

import hmac
import json
import re
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

from py_scripts.lib.talent_lifecycle.policy import control_root, records
from py_scripts.lib.talent_lifecycle.store import ACTIONS, StaleRevision, update_control

MAX_BODY = 4096
ACTOR_PATTERN = re.compile(r"^[^\s@]+@[^\s@]+\.[^\s@]+$")


def current_state(talent, saved):
    """Project the saved authority, preserving rollout defaults for absent records."""
    return {"talent_code": talent["talent_code"], "display_name": talent["display_name"],
            **(saved or {"revision": 0, "retrieval_enabled": True,
                         "processing_enabled": True})}


def make_server(address, *, secret, catalog_reader):
    """Create a server without starting it; credentials never appear in logs."""
    if not isinstance(secret, str) or len(secret) < 32 or not secret.isascii():
        raise ValueError("Configure an ASCII bridge secret of at least 32 characters")

    class Handler(BaseHTTPRequestHandler):
        def log_message(self, *_args):
            # Access URLs, headers, bodies and credentials are deliberately not logged.
            pass

        def reply(self, status, body):
            data = json.dumps(body).encode()
            self.send_response(status)
            self.send_header("Content-Type", "application/json")
            self.send_header("Cache-Control", "no-store")
            self.send_header("X-Content-Type-Options", "nosniff")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

        def authorized(self):
            supplied = self.headers.get("Authorization", "")
            return hmac.compare_digest(supplied.encode(), ("Bearer " + secret).encode())

        def route(self):
            if not self.authorized():
                return self.reply(401, {"error": "Bridge authentication required"})
            actor = self.headers.get("X-SDA-Lifecycle-Actor", "")
            if len(actor) > 254 or not ACTOR_PATTERN.fullmatch(actor):
                return self.reply(400, {"error": "Verified actor is required"})
            if self.path != "/v1/talents" and not re.fullmatch(
                    r"/v1/talents/[A-Za-z][A-Za-z0-9_-]{0,79}", self.path):
                return self.reply(404, {"error": "Not found"})
            if control_root() is None:
                raise RuntimeError("Lifecycle controls are not configured")
            catalog = catalog_reader()
            saved = {record["talent_code"]: record for record in records()}
            if self.path == "/v1/talents":
                if self.command != "GET":
                    return self.reply(405, {"error": "Use GET"})
                return self.reply(200, {"talents": [current_state(t, saved.get(code))
                                                  for code, t in catalog.items()]})
            code = self.path.rsplit("/", 1)[1]
            if code not in catalog:
                return self.reply(404, {"error": "Talent is not in the canonical catalog"})
            if self.command == "GET":
                return self.reply(200, current_state(catalog[code], saved.get(code)))
            if self.command != "POST":
                return self.reply(405, {"error": "Use GET or POST"})
            return self.save(code, actor, catalog[code])

        def save(self, code, actor, talent):
            if self.headers.get("Transfer-Encoding"):
                return self.reply(400, {"error": "Transfer-Encoding is unsupported"})
            if self.headers.get("Content-Type", "").split(";", 1)[0] != "application/json":
                return self.reply(415, {"error": "Use application/json"})
            length = int(self.headers.get("Content-Length", "0"))
            if not 0 < length <= MAX_BODY:
                return self.reply(413, {"error": "Invalid body size"})
            body = json.loads(self.rfile.read(length))
            if (not isinstance(body, dict) or set(body) != {"action", "expected_revision"}
                    or not isinstance(body["action"], str) or body["action"] not in ACTIONS
                    or type(body["expected_revision"]) is not int
                    or body["expected_revision"] < 0):
                return self.reply(400, {"error": "Invalid lifecycle change"})
            record = update_control(code, body["action"], actor=actor,
                                    expected_revision=body["expected_revision"],
                                    channel_ids=talent["channel_ids"], aliases=talent["aliases"])
            return self.reply(200, current_state(talent, record))

        def dispatch(self):
            self.connection.settimeout(10)
            try:
                self.route()
            except StaleRevision:
                self.reply(409, {"error": "Talent control changed; refresh before saving again"})
            except (ValueError, UnicodeError):
                self.reply(400, {"error": "Invalid lifecycle request or saved record"})
            except Exception:
                self.reply(503, {"error": "Lifecycle authority is temporarily unavailable"})

        do_GET = do_POST = do_PUT = do_DELETE = do_PATCH = do_OPTIONS = dispatch

    return ThreadingHTTPServer(address, Handler)
