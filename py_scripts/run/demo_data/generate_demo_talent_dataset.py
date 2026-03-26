from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
import os
import random
from datetime import date, datetime, time, timedelta, timezone
from pathlib import Path


CONTENT_TYPE_WEIGHTS = (
    ("short", 0.40),
    ("video", 0.35),
    ("live", 0.25),
)

CONTENT_PROFILES = {
    "short": {
        "median_views": 28000,
        "view_sigma": 0.55,
        "duration_range": (24, 58),
        "avp_range": (0.72, 0.94),
        "rpm_range": (0.40, 1.10),
        "sub_gain_range": (0.00025, 0.00060),
        "topic_weights": {
            "gaming": 0.30,
            "music": 0.20,
            "commentary": 0.20,
            "community": 0.30,
        },
    },
    "video": {
        "median_views": 12500,
        "view_sigma": 0.50,
        "duration_range": (360, 1100),
        "avp_range": (0.34, 0.58),
        "rpm_range": (2.20, 4.80),
        "sub_gain_range": (0.00045, 0.00120),
        "topic_weights": {
            "gaming": 0.34,
            "music": 0.16,
            "commentary": 0.24,
            "community": 0.26,
        },
    },
    "live": {
        "median_views": 6200,
        "view_sigma": 0.45,
        "duration_range": (4200, 14400),
        "avp_range": (0.14, 0.30),
        "rpm_range": (3.20, 7.20),
        "sub_gain_range": (0.00070, 0.00160),
        "topic_weights": {
            "gaming": 0.36,
            "music": 0.16,
            "commentary": 0.18,
            "community": 0.30,
        },
    },
}

TOPIC_BOOSTS = {
    "gaming": 1.00,
    "music": 1.10,
    "commentary": 0.95,
    "community": 0.92,
}

TOPIC_TAGS = {
    "gaming": ["challenge", "boss_fight", "cozy", "rpg", "strategy", "reaction"],
    "music": ["cover", "acoustic", "karaoke", "performance", "vocal", "arrangement"],
    "commentary": ["breakdown", "opinion", "analysis", "storytime", "trend_watch", "hot_take"],
    "community": ["q_and_a", "milestone", "behind_the_scenes", "community_pick", "tier_list", "collab"],
}

TOPIC_REFERENCES = {
    "gaming": ["Starfall Frontier", "Moonwake Trials", "Pixel Harbor", "Dungeon Relay"],
    "music": ["Midnight Echo", "Silver Carousel", "Starlit Avenue", "Summer Static"],
    "commentary": ["Creator Economy", "Platform Strategy", "Audience Habits", "Release Calendar"],
    "community": ["Community Showcase", "Studio Update", "Member Mail", "Milestone Special"],
}

TITLE_FRAGMENTS = {
    "gaming": [
        "Can We Beat",
        "First Look at",
        "Ranked Run in",
        "Community Tries",
        "The Chaos of",
    ],
    "music": [
        "Acoustic Session",
        "Live Cover of",
        "Studio Take on",
        "Singing Through",
        "Reworking",
    ],
    "commentary": [
        "What Creators Keep Missing About",
        "The Real Story Behind",
        "Breaking Down",
        "Why We Changed Our Approach to",
        "What Actually Worked in",
    ],
    "community": [
        "Answering Your Questions About",
        "Behind the Scenes of",
        "Community Picks for",
        "Milestone Recap and",
        "Studio Check-In for",
    ],
}

AGE_GROUPS = ("18-24", "25-34", "35-44", "45-54")
GENDERS = ("Female", "Male")
COUNTRIES = ("US", "CA", "GB", "PH", "AU", "DE", "JP", "BR")
COLLAB_GUESTS = (
    "Juniper Vale",
    "Mira Sol",
    "Kairo North",
    "Ember Wren",
    "Sable Orbit",
    "Nova Finch",
)


def resolve_default_talent_root() -> Path:
    env_root = os.getenv("TALENT_DATALAKE_ROOT", "").strip()
    if env_root:
        return Path(env_root)

    chat_root = os.getenv("CHAT_OUTPUT_ROOT", "").strip()
    if chat_root:
        return Path(chat_root)

    candidates = (
        Path("/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data"),
        Path("/mnt/datalake/Datalake/Sun_Data_Analytics/Talent_data"),
        Path("/mnt/router_data/DataLake/Sun_Data_Analytics/Talent_data"),
    )
    for candidate in candidates:
        if candidate.exists():
            return candidate

    raise RuntimeError(
        "Could not resolve talent datalake root. "
        "Pass --output-root or set TALENT_DATALAKE_ROOT."
    )


def weighted_choice(rng: random.Random, weighted_items):
    total = sum(weight for _, weight in weighted_items)
    pick = rng.uniform(0, total)
    upto = 0.0
    for item, weight in weighted_items:
        upto += weight
        if pick <= upto:
            return item
    return weighted_items[-1][0]


def slugify(value: str) -> str:
    cleaned = []
    last_was_sep = False
    for ch in value.lower():
        if ch.isalnum():
            cleaned.append(ch)
            last_was_sep = False
        elif not last_was_sep:
            cleaned.append("_")
            last_was_sep = True
    slug = "".join(cleaned).strip("_")
    return slug or "demo_talent"


def iso8601_duration(seconds: int) -> str:
    hours, rem = divmod(int(seconds), 3600)
    minutes, secs = divmod(rem, 60)
    out = "PT"
    if hours:
        out += f"{hours}H"
    if minutes:
        out += f"{minutes}M"
    if secs or out == "PT":
        out += f"{secs}S"
    return out


def clamp(value: float, low: float, high: float) -> float:
    return max(low, min(high, value))


def format_ts(dt: datetime) -> str:
    return dt.strftime("%Y-%m-%d %H:%M:%S")


def format_date(dt: datetime) -> str:
    return dt.strftime("%Y-%m-%d")


def lognormal_from_median(rng: random.Random, median: float, sigma: float) -> float:
    return math.exp(rng.normalvariate(math.log(median), sigma))


def normalize_percentages(raw_values: list[float]) -> list[float]:
    total = sum(raw_values) or 1.0
    scaled = [100.0 * val / total for val in raw_values]
    rounded = [round(val, 2) for val in scaled]
    rounded[-1] = round(rounded[-1] + (100.0 - sum(rounded)), 2)
    return rounded


def build_demographic_percentages(
    rng: random.Random,
    content_type: str,
    topic: str,
) -> list[tuple[str, str, float]]:
    age_bias = {
        "gaming": [1.4, 1.2, 0.8, 0.5],
        "music": [1.0, 1.3, 0.9, 0.6],
        "commentary": [0.7, 1.2, 1.1, 0.8],
        "community": [0.8, 1.1, 1.2, 0.9],
    }[topic]
    gender_bias = {
        "short": [0.58, 0.42],
        "video": [0.60, 0.40],
        "live": [0.55, 0.45],
    }[content_type]

    raw = []
    for age_ix, age_group in enumerate(AGE_GROUPS):
        for gender_ix, gender in enumerate(GENDERS):
            noise = rng.uniform(0.85, 1.20)
            raw.append(
                (
                    age_group,
                    gender,
                    age_bias[age_ix] * gender_bias[gender_ix] * noise,
                )
            )

    normalized = normalize_percentages([value for _, _, value in raw])
    return [
        (age_group, gender, pct)
        for (age_group, gender, _), pct in zip(raw, normalized)
    ]


def build_geography_percentages(rng: random.Random) -> list[tuple[str, float]]:
    base = [4.6, 1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.7]
    noisy = [weight * rng.uniform(0.75, 1.30) for weight in base]
    normalized = normalize_percentages(noisy)
    return list(zip(COUNTRIES, normalized))


def build_title(
    rng: random.Random,
    topic: str,
    primary_reference: str,
    content_type: str,
    is_collab: bool,
    guest_name: str | None = None,
) -> str:
    opener = rng.choice(TITLE_FRAGMENTS[topic])
    suffix_bits = []
    if content_type == "short":
        suffix_bits.append("Quick Clip")
    elif content_type == "live":
        suffix_bits.append("Live Session")
    else:
        suffix_bits.append("Deep Dive")
    if is_collab:
        suffix_bits.append("Collab")
    title = f"{opener} {primary_reference} | {' | '.join(suffix_bits)}"
    if is_collab and guest_name:
        title = f"{title} ft. {guest_name}"
    return title


def make_video_id(seed_text: str) -> str:
    digest = hashlib.sha1(seed_text.encode("utf-8")).hexdigest()
    return digest[:11]


def bool_string(value: bool) -> str:
    return "TRUE" if value else "FALSE"


def build_record(
    rng: random.Random,
    talent_name: str,
    talent_slug: str,
    channel_id: str,
    snapshot_dt: datetime,
    start_dt: datetime,
    video_index: int,
    force_collab: bool = False,
) -> dict:
    content_type = weighted_choice(rng, CONTENT_TYPE_WEIGHTS)
    profile = CONTENT_PROFILES[content_type]
    topic = weighted_choice(rng, tuple(profile["topic_weights"].items()))
    primary_reference = rng.choice(TOPIC_REFERENCES[topic])
    is_collab = force_collab or (rng.random() < 0.22)
    guest_name = rng.choice(COLLAB_GUESTS) if is_collab else None
    tags = set(rng.sample(TOPIC_TAGS[topic], k=3))
    if is_collab:
        tags.add("collab")
        tags.add("guest")
        tags.add(f"with_{slugify(guest_name)}")
    if content_type == "short":
        tags.add("short_form")
    elif content_type == "live":
        tags.add("live_session")
    else:
        tags.add("feature_video")

    total_days = max(1, (snapshot_dt - start_dt).days)
    days_after_start = int(total_days * clamp(rng.betavariate(2.3, 1.4), 0.0, 1.0))
    publish_dt = start_dt + timedelta(days=days_after_start)
    publish_dt = publish_dt.replace(
        hour=rng.randint(11, 21),
        minute=rng.choice((0, 5, 10, 15, 20, 30, 40, 45, 50, 55)),
        second=0,
    )

    duration_seconds = rng.randint(*profile["duration_range"])
    recency_share = days_after_start / total_days
    recency_boost = 0.88 + (0.32 * recency_share)
    topic_boost = TOPIC_BOOSTS[topic]
    collab_boost = 1.26 if is_collab else 1.0
    outlier_boost = rng.uniform(1.8, 2.9) if rng.random() < 0.08 else 1.0

    views = lognormal_from_median(
        rng,
        median=profile["median_views"],
        sigma=profile["view_sigma"],
    )
    views *= recency_boost * topic_boost * collab_boost * outlier_boost
    views_int = max(250, int(round(views)))

    avp = rng.uniform(*profile["avp_range"])
    if content_type == "video" and duration_seconds > 900:
        avp -= 0.04
    if content_type == "live" and duration_seconds > 10800:
        avp -= 0.03
    avp = clamp(avp, 0.10, 0.96)

    avg_view_duration_seconds = int(round(duration_seconds * avp * rng.uniform(0.92, 1.08)))
    avg_view_duration_seconds = max(10, min(duration_seconds - 1, avg_view_duration_seconds))
    estimated_minutes_watched = int(round((views_int * avg_view_duration_seconds) / 60.0))

    rpm = rng.uniform(*profile["rpm_range"])
    if topic == "community":
        rpm *= 1.08
    if topic == "music":
        rpm *= 0.95
    if is_collab:
        rpm *= 1.10
    estimated_revenue = round((views_int / 1000.0) * rpm * rng.uniform(0.92, 1.10), 2)
    cpm = round(rpm * rng.uniform(1.15, 1.45), 2)

    subs_gained_rate = rng.uniform(*profile["sub_gain_range"])
    subscribers_gained = int(round(views_int * subs_gained_rate))
    subscribers_lost = int(round(subscribers_gained * rng.uniform(0.04, 0.16)))

    title = build_title(
        rng=rng,
        topic=topic,
        primary_reference=primary_reference,
        content_type=content_type,
        is_collab=is_collab,
        guest_name=guest_name,
    )
    video_id = make_video_id(f"{talent_name}-{video_index}-{title}-{format_ts(publish_dt)}")

    booleans = {
        "collaborative_energy": is_collab,
        "community_milestones": topic == "community",
        "interactive_entertainment": topic in {"gaming", "community"},
        "meme_viral": "trend_watch" in tags or "reaction" in tags,
        "monetization": topic == "commentary" and rng.random() < 0.30,
        "narrative_serialization": topic == "gaming" and rng.random() < 0.25,
        "performance_artistry": topic == "music",
        "personality_conversation": topic in {"commentary", "community"},
    }

    return {
        "channel_name": talent_name,
        "channel_id": channel_id,
        "video_id": video_id,
        "title": title,
        "published_at": publish_dt,
        "content_type": content_type,
        "duration_seconds": duration_seconds,
        "duration_iso": iso8601_duration(duration_seconds),
        "views": views_int,
        "estimated_minutes_watched": estimated_minutes_watched,
        "average_view_duration_seconds": avg_view_duration_seconds,
        "average_view_percentage": round(avp * 100.0, 2),
        "subscribers_gained": subscribers_gained,
        "subscribers_lost": subscribers_lost,
        "estimated_revenue": estimated_revenue,
        "cpm": cpm,
        "topic": topic,
        "primary_reference": primary_reference,
        "tags": sorted(tags),
        "created_at": snapshot_dt,
        "title_hash": hashlib.sha1(title.encode("utf-8")).hexdigest()[:16],
        "talent_profile": talent_slug,
        "talent_id": f"demo_{talent_slug}",
        "classification_json": json.dumps(
            {
                "topic": topic,
                "language": "en",
                "tags": sorted(tags),
                "primary_reference": primary_reference,
                "referenced_entities": [primary_reference] + ([guest_name] if guest_name else []),
                **booleans,
            },
            separators=(",", ":"),
        ),
        "booleans": booleans,
        "guest_name": guest_name,
    }


def write_csv(path: Path, fieldnames: list[str], rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def build_outputs(args: argparse.Namespace) -> dict:
    rng = random.Random(args.seed)
    snapshot_day = datetime.strptime(args.snapshot_date, "%Y-%m-%d").date()
    snapshot_dt = datetime.combine(snapshot_day, time(12, 0, 0), tzinfo=timezone.utc).replace(tzinfo=None)
    start_dt = snapshot_dt - timedelta(days=(args.months * 30))

    output_root = Path(args.output_root).expanduser().resolve()
    talent_dir = output_root / args.talent_name
    raw_root = talent_dir / "raw_data"
    reports_root = talent_dir / "reports"
    demo_input_root = reports_root / "demo_inputs"

    channel_id = "UC" + hashlib.sha1(args.talent_name.encode("utf-8")).hexdigest()[:22]
    snapshot_suffix = snapshot_day.isoformat()

    analytics_rows = []
    monetary_rows = []
    demographic_rows = []
    geography_rows = []
    title_rows = []
    collab_target = max(18, int(round(args.videos * 0.30)))
    forced_collab_indices = set(rng.sample(range(1, args.videos + 1), k=min(args.videos, collab_target)))

    for video_index in range(1, args.videos + 1):
        record = build_record(
            rng=rng,
            talent_name=args.talent_name,
            talent_slug=slugify(args.talent_name),
            channel_id=channel_id,
            snapshot_dt=snapshot_dt,
            start_dt=start_dt,
            video_index=video_index,
            force_collab=video_index in forced_collab_indices,
        )
        published_at_ts = format_ts(record["published_at"])
        published_at_date = format_date(record["published_at"])

        analytics_rows.append(
            {
                "Channel Name": record["channel_name"],
                "Channel ID": record["channel_id"],
                "Video ID": record["video_id"],
                "Title": record["title"],
                "Published At": published_at_ts,
                "Content Type": record["content_type"],
                "DurationSeconds": record["duration_seconds"],
                "DurationISO": record["duration_iso"],
                "video": f"https://youtu.be/{record['video_id']}",
                "views": record["views"],
                "estimatedMinutesWatched": record["estimated_minutes_watched"],
                "averageViewDuration": record["average_view_duration_seconds"],
                "averageViewPercentage": record["average_view_percentage"],
                "subscribersGained": record["subscribers_gained"],
                "subscribersLost": record["subscribers_lost"],
                "Estimated Revenue": record["estimated_revenue"],
                "CPM": record["cpm"],
            }
        )

        monetary_rows.append(
            {
                "Channel Name": record["channel_name"],
                "Channel ID": record["channel_id"],
                "Video ID": record["video_id"],
                "Title": record["title"],
                "Published At": published_at_ts,
                "video": f"https://youtu.be/{record['video_id']}",
                "views_monetary_check": record["views"],
                "Estimated Revenue": record["estimated_revenue"],
                "CPM": record["cpm"],
            }
        )

        for age_group, gender, viewer_pct in build_demographic_percentages(
            rng=rng,
            content_type=record["content_type"],
            topic=record["topic"],
        ):
            demographic_rows.append(
                {
                    "Video ID": record["video_id"],
                    "Title": record["title"],
                    "Published At": published_at_date,
                    "Viewer Age": age_group,
                    "Viewer Gender": gender,
                    "Viewer Percentage": viewer_pct,
                    "Channel ID": record["channel_id"],
                    "Channel Name": record["channel_name"],
                }
            )

        for country, viewer_pct in build_geography_percentages(rng=rng):
            share = viewer_pct / 100.0
            geography_rows.append(
                {
                    "Channel Name": record["channel_name"],
                    "Channel ID": record["channel_id"],
                    "Video ID": record["video_id"],
                    "Title": record["title"],
                    "Published At": published_at_date,
                    "Country": country,
                    "Viewer Percentage": viewer_pct,
                    "Est Views (calc)": int(round(record["views"] * share)),
                    "Est Minutes Watched (calc)": int(round(record["estimated_minutes_watched"] * share)),
                }
            )

        title_rows.append(
            {
                "video_id": record["video_id"],
                "talent_name": args.talent_name,
                "talent_profile": record["talent_profile"],
                "model": "gpt-5-mini",
                "confidence": round(rng.uniform(0.79, 0.97), 2),
                "title_raw": record["title"],
                "content_type": record["content_type"],
                "published_at": format_ts(record["published_at"]),
                "topic": record["topic"],
                "language": "en",
                "tags": ", ".join(record["tags"]),
                "primary_reference": record["primary_reference"],
                "referenced_entities": ", ".join(
                    [record["primary_reference"]] + ([record["guest_name"]] if record["guest_name"] else [])
                ),
                "taxonomy_version": "v_demo",
                "prompt_version": "v_demo",
                "created_at": format_ts(record["created_at"]),
                "title_hash": record["title_hash"],
                "talent_id": record["talent_id"],
                "classification_json": record["classification_json"],
                "collaborative_energy": bool_string(record["booleans"]["collaborative_energy"]),
                "community_milestones": bool_string(record["booleans"]["community_milestones"]),
                "interactive_entertainment": bool_string(record["booleans"]["interactive_entertainment"]),
                "meme_viral": bool_string(record["booleans"]["meme_viral"]),
                "monetization": bool_string(record["booleans"]["monetization"]),
                "narrative_serialization": bool_string(record["booleans"]["narrative_serialization"]),
                "performance_artistry": bool_string(record["booleans"]["performance_artistry"]),
                "personality_conversation": bool_string(record["booleans"]["personality_conversation"]),
            }
        )

    analytics_path = raw_root / "video_analytics" / f"video_analytics_{snapshot_suffix}.csv"
    monetary_path = raw_root / "video_monetary" / f"video_monetary_{snapshot_suffix}.csv"
    demographics_path = raw_root / "video_demographics" / f"video_demographics_{snapshot_suffix}.csv"
    geography_path = raw_root / "video_geography" / f"video_geography_{snapshot_suffix}.csv"
    titles_path = demo_input_root / "demo_title_classifications.csv"
    manifest_path = demo_input_root / "demo_dataset_manifest.json"

    write_csv(
        analytics_path,
        [
            "Channel Name",
            "Channel ID",
            "Video ID",
            "Title",
            "Published At",
            "Content Type",
            "DurationSeconds",
            "DurationISO",
            "video",
            "views",
            "estimatedMinutesWatched",
            "averageViewDuration",
            "averageViewPercentage",
            "subscribersGained",
            "subscribersLost",
            "Estimated Revenue",
            "CPM",
        ],
        analytics_rows,
    )
    write_csv(
        monetary_path,
        [
            "Channel Name",
            "Channel ID",
            "Video ID",
            "Title",
            "Published At",
            "video",
            "views_monetary_check",
            "Estimated Revenue",
            "CPM",
        ],
        monetary_rows,
    )
    write_csv(
        demographics_path,
        [
            "Video ID",
            "Title",
            "Published At",
            "Viewer Age",
            "Viewer Gender",
            "Viewer Percentage",
            "Channel ID",
            "Channel Name",
        ],
        demographic_rows,
    )
    write_csv(
        geography_path,
        [
            "Channel Name",
            "Channel ID",
            "Video ID",
            "Title",
            "Published At",
            "Country",
            "Viewer Percentage",
            "Est Views (calc)",
            "Est Minutes Watched (calc)",
        ],
        geography_rows,
    )
    write_csv(
        titles_path,
        [
            "video_id",
            "talent_name",
            "talent_profile",
            "model",
            "confidence",
            "title_raw",
            "content_type",
            "published_at",
            "topic",
            "language",
            "tags",
            "primary_reference",
            "referenced_entities",
            "taxonomy_version",
            "prompt_version",
            "created_at",
            "title_hash",
            "talent_id",
            "classification_json",
            "collaborative_energy",
            "community_milestones",
            "interactive_entertainment",
            "meme_viral",
            "monetization",
            "narrative_serialization",
            "performance_artistry",
            "personality_conversation",
        ],
        title_rows,
    )

    summary = {
        "talent_name": args.talent_name,
        "video_count": args.videos,
        "target_collaboration_count": len(forced_collab_indices),
        "months": args.months,
        "seed": args.seed,
        "snapshot_date": snapshot_suffix,
        "talent_dir": str(talent_dir),
        "title_classifications_path": str(titles_path),
        "render_examples": {
            "bundle_a": (
                "Rscript r_scripts/run/bundle_A/render_bundle_A.R "
                f"--talent \"{args.talent_name}\" --data-source datalake "
                f"--titles-path \"{titles_path}\""
            ),
            "bundle_b": (
                "Rscript r_scripts/run/bundle_B/render_bundle_B.R "
                f"--talent \"{args.talent_name}\" --data-source datalake "
                f"--titles-path \"{titles_path}\""
            ),
        },
    }
    manifest_path.parent.mkdir(parents=True, exist_ok=True)
    manifest_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")

    return summary


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Generate a synthetic demo talent dataset that matches the Bundle A/B "
            "report input schema without using any real talent data."
        )
    )
    parser.add_argument(
        "--talent-name",
        default="Northstar Story Lab Demo",
        help="Name of the synthetic talent folder to generate.",
    )
    parser.add_argument(
        "--output-root",
        default=str(resolve_default_talent_root()),
        help="Parent directory that contains talent folders.",
    )
    parser.add_argument(
        "--snapshot-date",
        default=date.today().isoformat(),
        help="Snapshot date used in the raw_data filenames. Format: YYYY-MM-DD.",
    )
    parser.add_argument(
        "--videos",
        type=int,
        default=96,
        help="Number of synthetic videos to generate.",
    )
    parser.add_argument(
        "--months",
        type=int,
        default=12,
        help="Approximate time span covered by the generated publish dates.",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=42,
        help="Random seed for reproducible output.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.videos < 12:
        raise SystemExit("--videos must be at least 12 for stable bundle-level summaries.")
    if args.months < 3:
        raise SystemExit("--months must be at least 3 so the time-series sections remain useful.")

    summary = build_outputs(args)
    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
