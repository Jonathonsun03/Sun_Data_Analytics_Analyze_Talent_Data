# Qualitative Analysis Runner Cleanup

Last updated: 2026-04-19

## Summary

This note documents the qualitative-analysis runner and prompt cleanup completed on 2026-04-19.

The main decisions were:

- Keep per-stream summaries separate from whole-channel thematic summaries.
- Use simple prompt/runner pairs for Codex-driven qualitative workflows.
- Add a dedicated `overall_themes` category for channel-level synthesis that may include personality, monetary behavior, pacing, community rituals, and other recurring themes.
- Add `--talent "<exact folder name>"` scoping to active qualitative Codex runners.
- Configure the scheduled stream-summary catch-up run to process only missing or empty summaries, capped at 20 per run, during the 12:04 AM-5:05 AM reset window.

## Folder Model

Target `stream_summaries/` shape:

```text
stream_summaries/
  stream_summary_codex/
    <stream_stem>_summary.md

  overall_channel_summary/
    current/
      overall_channel_summary.md
      overall_channel_summary_state.json
    snapshots/
      overall_channel_summary_YYYY-MM-DD_HH-MM-SS_±HHMM.md

  overall_themes/
    monetary_analysis/
    money_timestamps.csv
    money_timestamps/
    personality_open_coding/
    personality_unique_features/
    personality_profile/
```

Interpretation:

- `stream_summary_codex/` is one markdown summary per stream.
- `overall_channel_summary/` is the cumulative whole-channel summary derived from the per-stream summaries.
- `overall_themes/` is reserved for deeper thematic-analysis families such as monetary and personality analysis.

Existing older outputs for the original Variance Project talents still exist under:

```text
stream_summaries/overall_themes/summary_classification/current/overall_themes_codex.md
```

New overall-channel work should use:

```text
stream_summaries/overall_channel_summary/current/overall_channel_summary.md
```

Downstream personality/shared-baseline scripts were adjusted to prefer the new `overall_channel_summary` path while falling back to the legacy `summary_classification` path.

## Prompt And Runner Organization

The maintained qualitative prompt tree now mirrors the categorized shell runners:

```text
prompts/
  monetary/
  overall_themes/
  personality/
  shared_qualities/
  summaries/

bin/linux/codex_prompts/
  monetary/
  overall_themes/
  personality/
  shared_qualities/
  summaries/
```

Important pairing added:

```text
prompts/overall_themes/overall_channel_summary.md
bin/linux/codex_prompts/overall_themes/overall_channel_summary.sh
```

The old `summarizing_stream_v2` prompt/runner was replaced by this clearer overall-channel naming:

```text
old:
  prompts/summaries/summarizing_stream_v2.md
  bin/linux/codex_prompts/summaries/summarizing_stream_v2.sh

new:
  prompts/overall_themes/overall_channel_summary.md
  bin/linux/codex_prompts/overall_themes/overall_channel_summary.sh
```

`summaries/` is now reserved for per-stream summary generation.

## Active Codex Runners

The active thematic qualitative runners now use the same basic shape:

```bash
cat "$PROMPT_FILE" | codex exec \
  --cd "$REPO" \
  --dangerously-bypass-approvals-and-sandbox \
  --output-last-message "$FINAL_MSG_FILE" \
  -
```

Active runners:

```text
bin/linux/codex_prompts/summaries/stream_summary_codex.sh
bin/linux/codex_prompts/overall_themes/overall_channel_summary.sh
bin/linux/codex_prompts/monetary/monetary_summary_classification.sh
bin/linux/codex_prompts/monetary/money_timestamps_incremental.sh
bin/linux/codex_prompts/personality/personality_open_coding.sh
bin/linux/codex_prompts/personality/personality_unique_features.sh
bin/linux/codex_prompts/personality/personality_qualitative_codebook.sh
bin/linux/codex_prompts/personality/personality_profile.sh
bin/linux/codex_prompts/shared_qualities/shared_behavior_baseline.sh
```

`personality_open_coding.sh` was changed from a Python-direct runner to a Codex prompt runner so it matches the rest of the thematic workflow family.

## Talent Scoping

Active thematic runners now support:

```bash
--talent "<exact talent folder name>"
```

Examples:

```bash
bin/linux/codex_prompts/summaries/stream_summary_codex.sh --talent "Rius Isonder Ch" --limit 1

bin/linux/codex_prompts/overall_themes/overall_channel_summary.sh --talent "Nova Aokami Ch"

bin/linux/codex_prompts/personality/personality_open_coding.sh --talent "Leia Memoria【Variance Project】"
```

Implementation:

- The runner creates a temporary scoped prompt.
- It appends a block with `TALENT_SLUG`.
- Codex is instructed to process only the exact talent folder when the workflow is per-talent.
- For cross-talent workflows, such as shared baseline or qualitative codebook, `--talent` means focus the refresh on that talent while using other talents only as comparison context when needed.

The active prompt files now document this optional `TALENT_SLUG` behavior.

## Rius Stream-Summary Smoke Test

Test command:

```bash
bin/linux/codex_prompts/summaries/stream_summary_codex.sh --talent "Rius Isonder Ch" --limit 1
```

Result:

```text
Total eligible text replay CSVs: 263
Created: 1
Skipped: 262
Failed: 0
```

Created output:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Rius Isonder Ch/stream_summaries/stream_summary_codex/Big_Time_Halation_Self_Cover_(feat._Cora_Velorum)___(Big_Time_Rush_x_Snow_Halation)_qY0yXhvJLsI_summary.md
```

This confirmed that:

- `--talent` scoping works for the stream-summary runner.
- Rius can enter the qualitative stream-summary pipeline.
- The runner correctly skips all other pending Rius streams after the one-file smoke-test cap.

## Nightly Stream-Summary Catch-Up

Runner:

```text
bin/linux/codex_prompts/summaries/stream_summary_codex_scheduled.sh
```

Behavior:

- Scans all talent folders by default.
- Counts only missing or empty per-stream summary outputs.
- Launches the stream-summary Codex runner only if pending summaries exist.
- Caps each run at 20 summaries by default.
- Enforces a time window of `00:04-05:05` by default.
- Uses a lock directory so overlapping scheduled runs do not stack.

Default command:

```bash
bin/linux/codex_prompts/summaries/stream_summary_codex_scheduled.sh
```

Dry-run check:

```bash
bin/linux/codex_prompts/summaries/stream_summary_codex_scheduled.sh --force --dry-run
```

Dry-run backlog at the time of setup:

```text
TOTAL 419 pending / 826 eligible
Avaritia Hawthorne 【Variance Project】 13 / 104
Katya Sable 【Variance Project】 13 / 86
Nova Aokami Ch 109 / 130
Rius Isonder Ch 262 / 263
Terberri Solaris Ch 22 / 104
```

Suggested cron entry:

```cron
4 0 * * * cd /home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data && bin/linux/codex_prompts/summaries/stream_summary_codex_scheduled.sh
```

The script also has its own time-window guard, so it will exit without launching Codex outside `00:04-05:05` unless `--force` is passed.

## Current Run Order For New Talents

Recommended qualitative-analysis order for a new talent:

1. `stream_summary_codex.sh`
2. `overall_channel_summary.sh`
3. `money_timestamps_incremental.sh` if money-event timestamps are needed
4. `monetary_summary_classification.sh`
5. `personality_open_coding.sh`
6. `shared_behavior_baseline.sh` when enough talent-local open coding exists for comparison
7. `personality_unique_features.sh`
8. `personality_qualitative_codebook.sh`
9. `personality_profile.sh`

The nightly scheduled runner addresses step 1 over time by steadily shrinking the missing-summary backlog.

## Notes And Caveats

- Title classification is separate from this prompt tree. These changes should not affect the title-classification prompt/config system under `classification/`.
- The old `summary_classification` naming still exists in some Python helper paths and legacy outputs, but the user-facing output path for whole-channel summaries is now `overall_channel_summary`.
- Existing generated analysis outputs were treated as read-only unless directly targeted by the current task.
- No raw data files were modified.
