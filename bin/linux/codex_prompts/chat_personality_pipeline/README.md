# Chat Personality Pipeline

Start here when you want to run the full chat personality pipeline in the maintained order.

The pipeline keeps chat/community behavior separate from streamer personality behavior.

## Usage

Run one talent:

```bash
bin/linux/codex_prompts/chat_personality_pipeline/run_chat_personality_pipeline.sh --talent "Nova Aokami Ch"
```

Common talent commands:

```bash
bin/linux/codex_prompts/chat_personality_pipeline/run_chat_personality_pipeline.sh --talent "Avaritia Hawthorne 【Variance Project】"
bin/linux/codex_prompts/chat_personality_pipeline/run_chat_personality_pipeline.sh --talent "Katya Sable 【Variance Project】"
bin/linux/codex_prompts/chat_personality_pipeline/run_chat_personality_pipeline.sh --talent "Leia Memoria【Variance Project】"
bin/linux/codex_prompts/chat_personality_pipeline/run_chat_personality_pipeline.sh --talent "Nova Aokami Ch"
```

Run all eligible talents:

```bash
bin/linux/codex_prompts/chat_personality_pipeline/run_chat_personality_pipeline.sh
```

Preview commands:

```bash
bin/linux/codex_prompts/chat_personality_pipeline/run_chat_personality_pipeline.sh --dry-run --talent "Nova Aokami Ch"
```

## Pipeline Order

1. `bin/linux/codex_prompts/chat_personality/chat_personality_open_coding.sh`
2. `bin/linux/codex_prompts/chat_personality/chat_shared_behavior_baseline.sh`
3. `bin/linux/codex_prompts/chat_personality/chat_personality_unique_features.sh`
4. `bin/linux/codex_prompts/chat_personality/chat_personality_qualitative_codebook.sh`
5. `bin/linux/codex_prompts/chat_personality/chat_personality_profile.sh`

## Main Talent-Local Output Root

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<Talent Name>/qualitative coding/chat data/
```

## Shared Output Roots

```text
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/chat_shared_interactions/
/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/Talent_Data/Chat Qualitative Codebook/
```
