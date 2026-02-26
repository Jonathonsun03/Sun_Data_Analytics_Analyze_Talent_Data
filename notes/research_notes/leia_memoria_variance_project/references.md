# Leia Memoria - References

## Primary Corpus
- Directory analyzed: `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Leia Memoria【Variance Project】/stream_summary_codex`
- File count: `127` summaries

## Representative Stream References

### High Paid-Message Intensity Examples
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Leia Memoria【Variance Project】/stream_summary_codex/AN_ARKNIGHTS_PLAYER'S_FIRST_IMPRESSION_OF_ENDFIELD_vF1_gGzgTN4_summary.md`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Leia Memoria【Variance Project】/stream_summary_codex/A_few_pulls_and_a_dream___2.7_Qiuyuan_Story_Reaction【Wuthering_Waves】_DP27knUR7I8_summary.md`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Leia Memoria【Variance Project】/stream_summary_codex/Getting_over_it_but_I_can't_see_1t1qqMZd6lw_summary.md`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Leia Memoria【Variance Project】/stream_summary_codex/Last_Stream....._of_2025_!!!!_BSobLXKnheA_summary.md`

### No-Paid but High-Reciprocity Examples
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Leia Memoria【Variance Project】/stream_summary_codex/Coming_back_to_play!_What_did_I_miss_!___1.7_patch_reaction【Zenless_Zone_Zero】_06TpmfMjmKg_summary.md`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Leia Memoria【Variance Project】/stream_summary_codex/Reading_Arknights_story_so_you_don't_have_to【STORY_PLAYTHROUGH_PT.2】_gk8V_2cAx7E_summary.md`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Leia Memoria【Variance Project】/stream_summary_codex/Well…_this_was_depressing【The_Beginner's_Guide】_6094wW-RNns_summary.md`

### Narrative-Template Format Examples
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Leia Memoria【Variance Project】/stream_summary_codex/A..._Twelve_Minutes_stream_we8Ncgh3SvQ_summary.md`
- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/Leia Memoria【Variance Project】/stream_summary_codex/Always_go_left....【A_Little_to_the_Left】_V-T6oOry5cw_summary.md`

## Evidence Tables

### Reciprocity Distribution (normalized)
- High: 116
- Moderate: 4
- Low: 7

### Pacing Distribution (normalized)
- Rapid: 72
- Moderate: 44
- Slow: 5
- Unknown: 6

### Paid Message Visibility
- With visible paid-message inventory: 92
- Without visible paid-message inventory: 35

### Money-Role Frequency (paid-message streams)
- Support: 89
- Bonding: 37
- Influence attempts: 33
- Ritual participation: 13
- Joke amplification: 12
- Attention-seeking: 9

### Top Title-Tag Clusters
- Doodle_&_Chat: 20
- Wuthering_Waves: 16
- Karaoke___歌枠: 6
- Variance_Project: 3
- Zenless_Zone_Zero: 2
- Working_Stream: 2
- Uma_Musume: 2

## Repro Notes
Counts above were derived by scanning all `*_summary.md` files in the corpus and extracting:
- reciprocity labels (including bullet and inline variants)
- pacing descriptors
- paid-message inventory entries
- section-4 money-role keywords
- first bracketed title tags from filenames
