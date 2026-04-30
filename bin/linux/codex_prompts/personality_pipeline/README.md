# Personality Pipeline Runner

Start here when you want to run the full personality pipeline in the maintained order.

## Main command

Run the pipeline for one talent:

```bash
bin/linux/codex_prompts/personality_pipeline/run_personality_pipeline.sh --talent "Nova Aokami Ch"
```

Run the pipeline for all eligible talents:

```bash
bin/linux/codex_prompts/personality_pipeline/run_personality_pipeline.sh
```

Preview the commands without executing them:

```bash
bin/linux/codex_prompts/personality_pipeline/run_personality_pipeline.sh --dry-run --talent "Nova Aokami Ch"
```

## Date Windows

Use a rolling window when you only want to refresh recent material:

```bash
bin/linux/codex_prompts/personality_pipeline/run_personality_pipeline.sh --talent "Nova Aokami Ch" --recent-months 2
```

Use an explicit date range when you want a fixed window:

```bash
bin/linux/codex_prompts/personality_pipeline/run_personality_pipeline.sh --talent "Nova Aokami Ch" --since 2026-03-01 --until 2026-04-30
```

The window is passed to every step as prompt scope. Each step should focus new analysis on source material inside the requested window while using older current outputs only for continuity, cumulative state, and comparison context.

## Pipeline order

The runner calls these existing step scripts:

1. `bin/linux/codex_prompts/personality/personality_open_coding.sh`
2. `bin/linux/codex_prompts/shared_qualities/shared_behavior_baseline.sh`
3. `bin/linux/codex_prompts/personality/personality_unique_features.sh`
4. `bin/linux/codex_prompts/personality/personality_qualitative_codebook.sh`
5. `bin/linux/codex_prompts/personality/personality_profile.sh`

## Model override

The step runners default to `gpt-5.4` so they do not depend on the global Codex config. To force a different compatible model:

```bash
CODEX_MODEL=gpt-5.3-codex bin/linux/codex_prompts/personality_pipeline/run_personality_pipeline.sh --talent "Nova Aokami Ch"
```

## Logs

Each step writes its own log and final-message markdown under:

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Logs/codex_prompts/
```

If the full runner stops early, check the log path printed by the last step before the failure message.
