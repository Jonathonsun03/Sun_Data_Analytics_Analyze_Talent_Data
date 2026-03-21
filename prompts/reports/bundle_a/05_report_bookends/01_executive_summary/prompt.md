# Bundle A Prompt: Executive Summary

## Section key
`executive_summary`

## Goal
Write a concise executive summary for the full Bundle A report that gives a decision-maker the clearest top-line performance story before they read the detail sections.

## Expected inputs
- Talent name
- Included date range
- Bundle A report purpose
- The completed section syntheses and chart-level interpretations from:
  - overall performance snapshot
  - trends over time
  - audience composition
  - content strategy deep dive

## Instructions
- Synthesize the full report at a higher level than any single section.
- Prioritize the clearest overall performance story, the most important tension or caveat, and the most actionable near-term implication.
- Keep the tone executive-friendly, direct, and concise.
- Mention the date range once if it helps orient the reader, but do not overemphasize it.
- Do not simply concatenate the section summaries.
- Do not introduce claims that are not supported by the provided report evidence.

## Output format
Return markdown only as one short paragraph.

Use 4-5 sentences in a report-ready narrative style.
Lead with the clearest overall takeaway, then explain why it matters at a management level, then end with the most important next move or decision focus.
Do not use bullets, headings, labels, or repetitive phrasing.
