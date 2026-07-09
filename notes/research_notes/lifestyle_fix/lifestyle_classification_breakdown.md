# Lifestyle classification breakdown

Date: 2026-05-07

Source reviewed: `notes/research_notes/lifestyle_fix/lifestyle.csv`

## Summary

The current `lifestyle` set contains 196 titles. It is not one coherent topic; it is a broad "personality and creator-life" bucket that currently absorbs casual chat, work streams, ASMR, art/handcam streams, debuts, birthdays, Q&A, memberships, short-form opinion bits, and some game/external-media edge cases.

High-level dataset shape:

- Rows: 196
- Content type: 151 live, 29 short, 16 video
- Language: all English
- Talent distribution: Leia Memoria 64, Avaritia Hawthorne 52, Rius Isonder 50, Katya Sable 15, Terberri Solaris 15
- Confidence distribution: 96 rows at 0.90 or higher, 56 rows from 0.80 to 0.89, 18 rows from 0.70 to 0.79, 26 rows below 0.70
- Classifier flags: `personality_conversation` is true for 171 of 196 rows, while `performance_artistry` is never true. That suggests the current definition is treating lifestyle mostly as personality/chat, even when the title signals art, ASMR, performance, or milestone content.

## Practical subtopics inside lifestyle

These are approximate title-review buckets. Some rows fit multiple buckets, so the purpose is to clarify the topic space rather than produce a strict one-label audit.

### 1. Free chat, zatsu, life updates, and opinion talk

This is the strongest "true lifestyle" core. Titles are built around unstructured conversation, creator mood, recent events, travel, opinions, or general hangout framing.

Examples:

- Rius: `Just Chatting`, `Just Yappin`, `Last stream before vacation`, `Offkai Thoughts and Upcoming Year Updates`
- Leia: `Hi I'm back`, `I lived !!! Hi I'm back let's chat`, `Leia's life struggles`
- Katya: `AFTERHOURS IN THE BAR`, `Rantcast`, `CHATTING IN THE CASINO`
- Avaritia: `FREE CHAT`, `DID YOU MISS ME OR SOMETHING!?`, `Do you ever get a little bit tired of life?`

Recommended classifier treatment:

- Keep this in lifestyle.
- Strong positive cues: `just chatting`, `just chattin`, `yapping`, `zatsu`, `free talk`, `free chat`, `rantcast`, `storytime`, `life update`, `thoughts`, `did you miss me`, `vacation`, `offkai`, `back`.
- Distinguish from "gaming with chat" when a game title appears in brackets or as the main content anchor.

### 2. Work streams, productivity, schedules, and creator operations

These titles are about working while live, planning, recording, scheduling, or getting creator tasks done. They are lifestyle-adjacent because the content is the creator's work process, not a separate entertainment object.

Examples:

- Rius: `Work Stream`, `Work With Me`, `Quick work stream`, `Gotta get things done`
- Leia: `Working Stream`, `In the recording mines`, `AAA I HAVE TOO MUCH TO RECORD`, `SCHEDULE + FREE TALK`

Recommended classifier treatment:

- Keep in lifestyle if the main anchor is work-along, scheduling, recording, or casual productivity.
- Consider a subtag such as `creator_operations` or `work_stream`.
- Positive cues: `work stream`, `work with me`, `working stream`, `recording`, `schedule`, `goals`, `resolutions`, `too much to do`.

### 3. Milestones, debuts, birthdays, anniversaries, and community celebrations

These are community lifecycle events rather than ordinary lifestyle content. They are currently mixed into lifestyle and often have high confidence.

Examples:

- Rius: `6 MONTH ANNIVERSARY`, `FIRST BIRTHDAY STREAM`, `2.0 Debut Stream`, `2nd Anniversary`, `Subathon Wrap-Up`
- Terberri: `Vtuber Debut`, `2.0 Model Debut`, `Thank you from me! (1st debut anni)`
- Leia: `DEBUT STREAM`, `DEBUT 2.0`, `BIRTHDAY STREAM DAY 1/2/3`, `Last Stream of 2025`
- Katya: `6 MONTH EQUILIBRIUM CELEBRATION`, `DEBUT 2.0`
- Avaritia: `Ava Hawthorne Debut Test`, `WE HIT 5K SUBS!?`

Recommended classifier treatment:

- Do not rely on lifestyle as the primary topic if there is a stronger milestone/community category.
- If the taxonomy only allows one topic, these may deserve `community_milestone` as the primary topic or a high-priority override before lifestyle.
- Positive cues: `debut`, `model debut`, `2.0`, `birthday`, `anniversary`, `subathon`, `celebration`, `we hit`, `5k`, `last stream`, `first stream`, `membership open`.

### 4. ASMR, sleep, relaxation, and 3DIO handcam

This is a very coherent Leia-heavy cluster. The titles are not just casual lifestyle; they describe a specific format and viewer use case: sleep, relaxation, tingles, ear cleaning, whispers, and 3DIO.

Examples:

- `3DIO ASMR | Can't fall asleep?`
- `Helping you fall asleep with relaxing tingles`
- `Soft whispers and ear massage`
- `Cleaning your ears until you fall asleep`
- `Triggers to put you to sleep`
- `Relaxing sounds of Autumn`

Recommended classifier treatment:

- Give ASMR its own primary topic or at least a strong subtype under lifestyle.
- If `asmr`, `3dio`, `tingles`, `ear massage`, `whispers`, `fall asleep`, `good night sleep`, `relaxing sounds`, or `triggers` appear, classify as `asmr_relaxation` before generic lifestyle.
- These should not be explained only as `personality_conversation`; they are format-driven.

### 5. Art, doodle-and-chat, handcam crafts, food, and tactile creator activity

This bucket combines active making/showing streams. Leia's `Doodle & Chat` titles are the cleanest art subcluster. Avaritia has handcam food/nails/cooking titles. Rius has one art/emote stream.

Examples:

- Leia: `Doodle & Chat`, `Drawing members profile pic`, `Drawing your custom pfp`, `Showing off art tips`, `Nerding out about my artbooks`, `Halloween Leia art`
- Avaritia: `GOTTA DO MY NAILS`, `DINNER AND ALCOHOL [HANDCAM]`, `EATING TACO BELL`, `COOKING MARSHMALLOWS`, `beating meat [HANDCAM]`
- Rius: `Drawing YOU and an emote`

Recommended classifier treatment:

- Split art from general lifestyle when the activity is explicit.
- Possible subtopics: `art_creation`, `handcam_food`, `handcam_craft`, `unboxing`.
- Positive cues: `doodle`, `drawing`, `draw`, `art`, `pfp`, `emote`, `artbooks`, `handcam`, `nails`, `cooking`, `dinner`, `eating`, `unboxing`.
- Boundary: handcam is not automatically lifestyle. If the handcam title is ASMR, send it to ASMR. If it is cooking or food, use food/handcam. If it is art, use art.

### 6. Q&A, marshmallow, advice, and ask-me-anything formats

This is an interactive conversation format, mostly Rius with a few Leia/Avaritia rows.

Examples:

- Rius: `500 Sub QnA With X2 Buldak Noodles`, `Q&A with 3x Buldak Ramen`, `Answering questions and giving horrible advice`
- Leia: `Replying to Marshmallow questions and yapping`
- Avaritia: `ASK ME ANYTHING`

Recommended classifier treatment:

- Keep under lifestyle if the taxonomy has no interaction/Q&A topic, but assign a subtype.
- Positive cues: `q&a`, `qna`, `ask me anything`, `ama`, `marshmallow`, `answering questions`, `advice`.
- If Q&A is tied to a milestone, let milestone win and add Q&A as a secondary tag.

### 7. Shorts, skits, one-off opinions, and ambiguous personality bits

Shorts are a major source of low-confidence lifestyle rows. Many are personality bits rather than classifiable lifestyle topics. They often lack enough context to classify confidently.

Examples:

- Rius: `My one bad food take`, `A MESSAGE to Baskin Robbins America`, `formal invitation`
- Terberri: `Help! Tasukete!!!`, `Nah, I'm good`, `Lost and Confuse`, `Overworked and underpaid`, `Is This ACTUALLY Cozy?!?!?!`
- Avaritia: `My first impression of Twitter`, `Dirty talk`, `I'm not coping at all`, `Do you see me now?`, `making out with another vtuber`
- Katya: `Plastic straws my beloved`, `Self Shelfulization`

Recommended classifier treatment:

- Do not over-trust lifestyle for short-form titles unless there is a clear lifestyle cue.
- Add a fallback subtype such as `personality_short` or `short_form_bit`.
- Lower confidence when a short only contains a vague line plus `#vtuber`.
- Watch for alternate primary labels: meme/viral, opinion, clip, relationship/romance bit, food, mental health, or unclear.

## Edge cases and likely misroutes

These titles were classified as lifestyle but appear to have stronger anchors elsewhere:

- `PEAK`, `SCHOOL DAZE`, `COFFIN OF ANDY & LEYLEY`, and `ARCTIC EGGS` look like game or external-media titles. If the bracketed segment is a known game/show/content title, do not classify as lifestyle just because the title has conversational wording.
- `VTUBER CARD GAME?! [Unboxing Handcam]` may be unboxing or tabletop/card-game content before lifestyle.
- `SIBLING "LOVE" IS SO INTENSE...? [COFFIN OF ANDY & LEYLEY #2]` and `THE SIBLINGS FWICKED? [COFFIN OF ANDY & LEYLEY #3]` should likely be game/media reaction, not lifestyle.
- `LET ME COOK FOR YOU [ARCTIC EGGS]` contains a food phrase, but the bracketed anchor indicates game content.
- `FF Host Audition`, `Nijisanji Audition`, and audition-related rows may deserve `audition/career` or `creator_milestone`, depending on taxonomy goals.

## Proposed lifestyle definition

Use `lifestyle` for content where the primary subject is the creator's ordinary life, personality, schedule, mood, opinions, community relationship, or work process, and there is no stronger domain anchor such as a named game, performance format, ASMR format, art creation, music, news, or external media.

Strong lifestyle positives:

- Casual conversation: `just chatting`, `zatsu`, `yapping`, `free talk`, `free chat`, `rantcast`, `storytime`
- Creator-life context: `life update`, `vacation`, `offkai`, `schedule`, `goals`, `resolutions`, `work stream`, `work with me`
- Community relationship: `membership`, `subathon wrap-up`, `thank you`, `we hit`, `anniversary`, `birthday`, if no dedicated milestone topic exists
- Interactive creator talk: `q&a`, `marshmallow`, `ask me anything`

Strong lifestyle negatives or lower-priority overrides:

- Named game/media in brackets or title: classify as game/media first.
- ASMR/3DIO/sleep/tingles/ear cleaning: classify as ASMR/relaxation first.
- Doodle/drawing/art/pfp/emote/art tips: classify as art/creative first.
- Debut/model debut/birthday/anniversary/subathon/5K: classify as milestone first if that topic exists.
- Vague shorts with only a punchline plus `#vtuber`: use short-form/personality fallback and lower confidence.

## Suggested subtopic labels

If `lifestyle` remains a broad primary topic, add subtopic tags so it can be broken down downstream:

- `free_chat_zatsu`
- `creator_life_update`
- `work_stream_productivity`
- `community_milestone`
- `qna_marshmallow`
- `asmr_sleep_relaxation`
- `art_doodle_chat`
- `handcam_food_craft`
- `personality_short`
- `game_media_edge_case`

## Classifier cleanup recommendations

1. Add priority rules before generic lifestyle:
   - `asmr/3dio/sleep/tingles/ear` -> ASMR/relaxation
   - `doodle/drawing/art/pfp/emote` -> art/creative
   - bracketed known game/media title -> game/media
   - `debut/birthday/anniversary/subathon/we hit` -> milestone/community

2. Narrow lifestyle so it is not a catch-all for "title sounds personal."
   - Require a lifestyle-positive cue or an explicit creator-life context.
   - Penalize vague shorts with no topical anchor.

3. Treat `handcam` as a format cue, not a topic.
   - Route by surrounding words: ASMR, cooking/food, art/craft, unboxing.

4. Revisit flags:
   - `performance_artistry` is false for every row despite many art/ASMR/handcam titles.
   - `personality_conversation` is true for 171 rows, which makes it too broad to distinguish actual subtopics.

5. Keep examples in the prompt.
   - Lifestyle positives: `Just Chatting`, `Zatsu`, `Work With Me`, `Schedule + Free Talk`, `Offkai Thoughts`, `Hi I'm back`.
   - Lifestyle negatives: `3DIO ASMR`, `Doodle & Chat`, `Coffin of Andy & Leyley`, `Arctic Eggs`, `Debut 2.0` when those topics are available.

