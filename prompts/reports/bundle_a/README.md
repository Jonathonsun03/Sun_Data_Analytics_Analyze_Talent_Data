# Bundle A Interpretation Prompts

This folder stores prompt specs for Bundle A interpretations, organized to match the report's section structure and the goals described in `r_scripts/run/bundle_A/Bundle_A_Desc.md`.

Structure:

- top-level folders match the report's major sections
- later folders may add report-level synthesis and editorial QA stages
- each prompt lives in its own subfolder so multi-part workflows can be added later
- section folders may include:
  - a section-level synthesis prompt
  - chart-level interpretation prompts
  - table-level interpretation prompts when needed

Recommended per-prompt workflow:

- `prompt.md`: maintained instructions for that prompt
- `input.md` or `input.json`: generated section-specific input payload
- `output.md`: generated interpretation text

Recommended report workflow:

1. Build Bundle A chart and table inputs.
2. Write one `input` file per prompt folder.
3. Run the prompt and overwrite that folder's `output.md`.
4. Render the matching `output.md` below the relevant chart, table, or section heading.
5. Optionally run report-level synthesis prompts such as executive summary and conclusion.
6. Optionally run editorial-review prompts for internal QA before delivery.

Suggested datalake output pattern at render time:

- `/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data/<Talent>/reports/bundle_A/interpretations/<major_section>/<prompt_key>.md`

These interpretation outputs should be overwritten on each report run so the narrative matches the current data window.
