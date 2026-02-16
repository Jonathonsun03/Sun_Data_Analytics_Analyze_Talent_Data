# Research Notes

Last updated: 2026-02-16

## Current Project Options

### 1) Titles and View Correlation (ENA)
Goal: Identify whether title features are correlated with higher views.

Core idea:
- Build ENA across all processed video titles.
- Extract lineweights per title/video.
- Test correlation between ENA-derived features and view counts.

Assumptions to validate:
- Title language has measurable relationship with views.
- Existing processed title set is large enough for meaningful signal.

Immediate tasks:
1. Confirm list of all processed titles/videos.
2. Run ENA on full processed title corpus.
3. Export lineweights and join with video-level views.
4. Run exploratory correlations and rank strongest relationships.
5. Document whether signal is stable or likely noise.

Risks:
- Correlation may be weak or confounded by channel size, stream topic, or event timing.

---

### 2) When Do People Give Money? (Live Chat Context)
Goal: Understand when monetary events occur in live chat and what contextual patterns precede them.

Core idea:
- Identify timing and context of money events in replay chat.
- Add qualitative coding around surrounding chat context.
- Build stream-level summaries, then cross-stream summary synthesis.

Dependencies:
1. Define what counts as a money event.
2. Build/confirm extraction pipeline for money events + surrounding windows.
3. Create qualitative coding scheme for context.
4. Build summarization pipeline:
   - Per-stream summary
   - Cross-stream aggregate summary

Immediate tasks:
1. Draft qualitative codebook for money-event context.
2. Build a small pilot dataset (5-10 streams).
3. Test event-window extraction and summary quality.
4. Refine coding + summarization workflow before scaling.

Risks:
- This project has more setup steps and slower time-to-insight than the titles project.

---

## Suggested Execution Order

1. Start with `Titles and View Correlation` for faster validation and quicker feedback.
2. Run `Money Event Context` as the deeper follow-up once coding and summarization pipeline design is stable.

Rationale:
- Titles project is closer to analysis-ready.
- Money project is likely higher depth but higher setup cost.

## Weekly Checkpoint Template

Use this each week:

- What was completed:
- What failed / blocked:
- New evidence for/against assumptions:
- Dataset/pipeline changes made:
- Next week priority:
