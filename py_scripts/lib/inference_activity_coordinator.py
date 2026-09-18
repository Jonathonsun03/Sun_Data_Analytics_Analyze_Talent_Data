"""Atomic request draining for the single-worker NLP inference API."""

from __future__ import annotations

import asyncio
import ipaddress
import re

from fastapi import FastAPI, Request
from fastapi.responses import JSONResponse


TOKEN_PATTERN = re.compile(r"^[0-9a-f]{64}$")
BATCH_TOKEN_PATTERN = re.compile(r"^[A-Za-z0-9_-]{32,128}$")


class InferenceActivity:
    """Track admitted inference requests and hold a shutdown drain barrier."""

    def __init__(self) -> None:
        self.lock = asyncio.Lock()
        self.active_requests = 0
        self.drain_token: str | None = None
        self.batch_tokens: set[str] = set()


def install_inference_activity(app: FastAPI, *, ctid: int = 106) -> None:
    """Add local-only drain routes and account for every admitted API request."""
    activity = InferenceActivity()
    app.state.inference_activity = activity

    @app.middleware("http")
    async def track_inference_requests(request: Request, call_next):
        if not request.url.path.startswith("/v1/"):
            return await call_next(request)
        async with activity.lock:
            if activity.drain_token is not None:
                return JSONResponse(
                    status_code=503,
                    content={"detail": "Inference admission is draining for host shutdown."},
                )
            activity.active_requests += 1
        try:
            return await call_next(request)
        finally:
            async with activity.lock:
                activity.active_requests -= 1

    def local_request(request: Request) -> bool:
        try:
            return ipaddress.ip_address(request.client.host).is_loopback
        except (AttributeError, ValueError):
            return False

    @app.post("/internal/shutdown/prepare/{token}")
    async def prepare_shutdown(token: str, request: Request):
        if not local_request(request) or not TOKEN_PATTERN.fullmatch(token):
            return JSONResponse(status_code=404, content={"detail": "Not found"})
        async with activity.lock:
            if activity.drain_token not in (None, token):
                return JSONResponse(status_code=409, content={"detail": "Drain already held"})
            activity.drain_token = token
            return {
                "protocol": 1,
                "request_id": token,
                "ctid": ctid,
                "coverage": "all_inference_work",
                "admission_closed": True,
                "drain_persistent": True,
                "active_requests": activity.active_requests,
                "queued_jobs": 0,
                "active_batches": len(activity.batch_tokens),
            }

    @app.post("/internal/shutdown/release/{token}")
    async def release_shutdown(token: str, request: Request):
        if not local_request(request) or not TOKEN_PATTERN.fullmatch(token):
            return JSONResponse(status_code=404, content={"detail": "Not found"})
        async with activity.lock:
            if activity.drain_token == token:
                activity.drain_token = None
            return {"released": activity.drain_token is None}

    @app.post("/internal/batches/reserve/{token}")
    async def reserve_batch(token: str, request: Request):
        if not local_request(request) or not BATCH_TOKEN_PATTERN.fullmatch(token):
            return JSONResponse(status_code=404, content={"detail": "Not found"})
        async with activity.lock:
            if activity.drain_token is not None:
                return JSONResponse(status_code=503, content={"detail": "Host is draining"})
            activity.batch_tokens.add(token)
            return {"reserved": True}

    @app.post("/internal/batches/release/{token}")
    async def release_batch(token: str, request: Request):
        if not local_request(request) or not BATCH_TOKEN_PATTERN.fullmatch(token):
            return JSONResponse(status_code=404, content={"detail": "Not found"})
        async with activity.lock:
            activity.batch_tokens.discard(token)
            return {"released": True}
