# Bundle B Interpretation Prompts

This folder stores prompt specs for Bundle B interpretations, organized to match the Bundle B report section structure.

Structure:

- top-level folders match the report's major section headings
- each prompt lives in its own subfolder with `prompt.md`
- section-level synthesis prompts sit beside chart/table prompts when they help bridge the section
- `06_report_bookends/` stores the executive summary and conclusion prompts
- `07_editorial_review/` stores internal QA and the full-report rewrite prompt

Recommended workflow:

1. Build Bundle B artifacts under the talent datalake report folder.
2. Write one `input.md` per prompt folder.
3. Run the section/chart prompts and the report bookend prompts with `codex exec`.
4. Run the editorial rewrite prompt to overwrite all client-facing `output.md` files.
5. Render the matching `output.md` below the relevant section intro, chart, or table.

Expected datalake output pattern:

- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<Talent>/reports/bundle_B/interpretations/<major_section>/<prompt_key>/output.md`

Style contract for all Bundle B prompts:

- markdown only
- one short paragraph only
- interpretation first
- then why it matters
- then a natural recommended follow-up or bridge
- no bullets, no headings, no labeled sections, no repetitive phrasing
