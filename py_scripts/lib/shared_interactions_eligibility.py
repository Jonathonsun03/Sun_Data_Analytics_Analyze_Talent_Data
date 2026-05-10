from __future__ import annotations

import argparse
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Iterable, List, Optional


DEFAULT_TALENT_ROOT = Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data")
AGGREGATE_TALENT_DIR_NAMES = {
    "VarianceProject",
    "Northstar Story Lab Demo",
}


@dataclass(frozen=True)
class TalentEligibility:
    name: str
    path: Path
    eligible: bool
    reason: str
    profile_path: Optional[Path]
    profile_mtime: Optional[datetime]
    open_codebook_path: Optional[Path]
    open_evidence_path: Optional[Path]
    open_profile_path: Optional[Path]


def _is_non_talent_dir(path: Path) -> bool:
    return path.name in AGGREGATE_TALENT_DIR_NAMES


def _first_nonempty(paths: Iterable[Path]) -> Optional[Path]:
    for path in paths:
        if path.exists() and path.is_file() and path.stat().st_size > 0:
            return path
    return None


def _mtime(path: Path) -> datetime:
    return datetime.fromtimestamp(path.stat().st_mtime, tz=timezone.utc)


def _open_coding_paths(talent_dir: Path) -> tuple[Optional[Path], Optional[Path], Optional[Path]]:
    base = talent_dir / "stream_summaries" / "overall_themes" / "personality_open_coding"
    candidates = [
        (
            base / "v3" / "current" / "open_codebook_v3.csv",
            base / "v3" / "current" / "open_coding_evidence_v3.csv",
            base / "v3" / "current" / "personality_profile_v3_open_coding.md",
        ),
        (
            base / "v2" / "open_codebook_v2.csv",
            base / "v2" / "open_coding_evidence_v2.csv",
            base / "v2" / "personality_profile_v2_open_coding.md",
        ),
    ]
    for codebook, evidence, profile in candidates:
        if (
            codebook.exists()
            and codebook.stat().st_size > 0
            and evidence.exists()
            and evidence.stat().st_size > 0
            and profile.exists()
            and profile.stat().st_size > 0
        ):
            return codebook, evidence, profile
    return None, None, None


def _profile_path(talent_dir: Path) -> Optional[Path]:
    base = talent_dir / "stream_summaries" / "overall_themes" / "personality_profile"
    return _first_nonempty(
        [
            base / "overall" / "personality_overall_highlights_codex.md",
            base / "overall" / "personality_overall_evidence_log_codex.csv",
        ]
    )


def discover_shared_interaction_eligibility(
    talent_root: Path = DEFAULT_TALENT_ROOT,
    profile_days: int = 31,
    require_recent_profile: bool = True,
    now: Optional[datetime] = None,
) -> List[TalentEligibility]:
    now_utc = now.astimezone(timezone.utc) if now else datetime.now(timezone.utc)
    cutoff = now_utc - timedelta(days=profile_days)
    results: List[TalentEligibility] = []

    for talent_dir in sorted(talent_root.iterdir(), key=lambda path: path.name.lower()):
        if not talent_dir.is_dir() or _is_non_talent_dir(talent_dir):
            continue

        codebook, evidence, open_profile = _open_coding_paths(talent_dir)
        profile = _profile_path(talent_dir)
        profile_mtime = _mtime(profile) if profile else None

        if not codebook or not evidence or not open_profile:
            results.append(
                TalentEligibility(
                    name=talent_dir.name,
                    path=talent_dir,
                    eligible=False,
                    reason="missing non-empty personality_open_coding outputs",
                    profile_path=profile,
                    profile_mtime=profile_mtime,
                    open_codebook_path=codebook,
                    open_evidence_path=evidence,
                    open_profile_path=open_profile,
                )
            )
            continue

        if not profile:
            results.append(
                TalentEligibility(
                    name=talent_dir.name,
                    path=talent_dir,
                    eligible=False,
                    reason="missing non-empty personality_profile overall output",
                    profile_path=None,
                    profile_mtime=None,
                    open_codebook_path=codebook,
                    open_evidence_path=evidence,
                    open_profile_path=open_profile,
                )
            )
            continue

        if require_recent_profile and profile_mtime and profile_mtime < cutoff:
            results.append(
                TalentEligibility(
                    name=talent_dir.name,
                    path=talent_dir,
                    eligible=False,
                    reason=f"personality profile older than {profile_days} days",
                    profile_path=profile,
                    profile_mtime=profile_mtime,
                    open_codebook_path=codebook,
                    open_evidence_path=evidence,
                    open_profile_path=open_profile,
                )
            )
            continue

        results.append(
            TalentEligibility(
                name=talent_dir.name,
                path=talent_dir,
                eligible=True,
                reason="eligible",
                profile_path=profile,
                profile_mtime=profile_mtime,
                open_codebook_path=codebook,
                open_evidence_path=evidence,
                open_profile_path=open_profile,
            )
        )

    return results


def main() -> None:
    parser = argparse.ArgumentParser(description="List talents eligible for shared-interactions analysis.")
    parser.add_argument("--profile-days", type=int, default=31)
    parser.add_argument("--include-stale-profiles", action="store_true")
    parser.add_argument("--talent-root", type=Path, default=DEFAULT_TALENT_ROOT)
    args = parser.parse_args()

    rows = discover_shared_interaction_eligibility(
        talent_root=args.talent_root,
        profile_days=args.profile_days,
        require_recent_profile=not args.include_stale_profiles,
    )
    for row in rows:
        status = "ELIGIBLE" if row.eligible else "SKIP"
        profile_mtime = row.profile_mtime.isoformat() if row.profile_mtime else ""
        profile_path = str(row.profile_path) if row.profile_path else ""
        print("\t".join([status, row.name, row.reason, profile_mtime, profile_path]))


if __name__ == "__main__":
    main()
