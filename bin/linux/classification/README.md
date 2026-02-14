# Classification Shell Wrappers

## `run_talent_profile_builder.sh`
Wrapper for:
`scripts/run/Title_classification/build_talent_profile.R`

This runs the talent profile builder, which creates/updates:
- `classification/config/talents/<talent_slug>.json`
- `classification/prompts/talents/<talent_slug>/overlay.txt`
- `classification/config/talent_profiles.json` (when `--update-master-config` is used)

### Run
From repo root:
`bin/linux/classification/run_talent_profile_builder.sh --csv notes/titles.csv --all-talents --talent-col talent --title-col "Title" --content-type-col "Content Type" --write-overlay --update-master-config`

### Single Talent
`bin/linux/classification/run_talent_profile_builder.sh --csv notes/titles.csv --talent "Terberri_Solaris_Ch" --talent-col talent --title-col "Title" --content-type-col "Content Type" --write-overlay --update-master-config`

### GPT Discovery Mode (Optional)
`OPENAI_MODEL=gpt-5-mini bin/linux/classification/run_talent_profile_builder.sh --csv notes/titles.csv --all-talents --talent-col talent --title-col "Title" --content-type-col "Content Type" --write-overlay --update-master-config --use-gpt --sample-size 250`
