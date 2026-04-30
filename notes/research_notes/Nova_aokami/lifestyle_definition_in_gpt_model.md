# Nova Aokami: how `lifestyle` is defined in the GPT title model

Created: 2026-04-30

## Short answer

The current GPT title-classification prompt does not define a Nova-specific `lifestyle` category in prose. For Nova, `lifestyle` is one allowed `topic` enum value in the schema, and it is mostly reached through the global `personality_conversation` definition when the title looks like direct speaking, Q&A, life updates, reactions, storytime, or open conversation rather than a clearer domain such as gaming or music.

In other words, for Nova the model treats `lifestyle` as a broad conversation/life/personal-stream bucket, not as a tightly specified genre.

## Source prompt evidence

- `classification/prompts/talents/nova_aokami_ch/full_prompt/20260417_230812_Nova_Aokami_Ch_v6_0f08c66624cc.txt`
  - The schema allows `topic` values: `music`, `gaming`, `lifestyle`, `education`, `news_commentary`, `comedy`, `sports`, `technology`, `other`.
  - `lifestyle` appears only as an enum option, not with its own dedicated inclusion/exclusion criteria.
- `classification/prompts/definitions/personality_conversation.txt`
  - Defines the related boolean code as content centered on direct speaking, Q&A, life updates, reactions, or open conversation.
  - Inclusion examples: `chat`, `talk`, `Q&A`, `questions`, `storytime`, `life update`, `reacts to`.
  - Mapping guidance says this code is usually mapped to `topic = lifestyle`, `education`, `comedy`, or `news_commentary` depending on subject.
- `classification/prompts/talents/nova_aokami_ch/overlay.txt`
  - Nova-specific guidance says square brackets are weak or rare signals.
  - Parentheses are usually modifiers such as versions, events, live cuts, or edits.
  - Short, meme-like, or context-poor titles should get conservative confidence.
  - Explicit karaoke, cover, concert, acapella, or music words are strong music signals.

## Practical interpretation for Nova

A Nova title is likely to be labeled `topic = lifestyle` when:

- The title foregrounds talking, chatting, questions, life updates, reactions, personal topics, storytime, or general audience conversation.
- The title does not provide a stronger game/IP anchor that would make `topic = gaming`.
- The title does not provide a stronger karaoke/cover/concert/acapella/song-title signal that would make `topic = music`.
- The title is not mainly educational, news/commentary, comedy, sports, technology, or otherwise more specific.

The companion boolean `personality_conversation` is likely to be `TRUE` for many of these records, but it is not identical to `lifestyle`: the prompt allows personality/conversation content to map to lifestyle, education, comedy, or news commentary depending on the title subject.

## Current ambiguity

Because `lifestyle` has no direct definition file, the boundary is under-specified. This matters for Nova because the classifier may use `lifestyle` as a fallback for:

- Just-chatting or open-ended audience interaction.
- Personal update or self-disclosure titles.
- Reaction/discussion titles with no specific news, educational, or comedy anchor.
- Context-poor or meme-like titles that are not clearly gaming or music.

That means the preliminary 90-day analysis should treat Nova's `lifestyle` count as a broad GPT-inferred bucket until we add a sharper rule.

## Refinement questions

- Should Nova `lifestyle` be limited to explicit life/personal/chat/update titles, or should it include all non-gaming, non-music conversational streams?
- Should meme-like Nova shorts default to `comedy`, `other`, or low-confidence `lifestyle` when no clear activity is named?
- Should member streams, milestones, or announcements remain `lifestyle` unless another domain is explicit?
- Do we want a dedicated `lifestyle` definition file with positive and negative examples, parallel to the existing boolean definition files?

