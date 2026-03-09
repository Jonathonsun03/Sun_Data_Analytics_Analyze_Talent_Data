## Bundle A: Performance & Growth Overview

**Best for clients asking:**
“How is the channel performing overall, and where are the major changes?”

**Included Analyses:**
1. Overall snapshot:
Views by content type (total and average per video), combined views + revenue trends, total revenue by content type, engagement distribution and median engagement by content type.
2. Trends over time:
Views and revenue by content type over month-level periods.
3. Audience composition:
Audience age + gender trends and Core vs Potential audience stability.
4. Monetization quality:
Views-to-dollars ratio table.
5. Content-type deep dive:
Weekend vs weekday distribution, day-of-week contribution, collaboration effectiveness (average per video and lift), topic performance, tag performance (total + average views per tag).

**Key Questions Answered:**
How have views and revenue changed over time?
Which content formats and topics/tags perform best by total and by per-video average?
How stable is the core audience vs potential audience share?
Are weekends, weekdays, or specific days associated with stronger performance?
Do collaborations outperform non-collaborative uploads on a per-video basis?

**Delivered Outputs:**
Interactive charts and tables across performance, audience, monetization, timing, collaboration, topics, and tags.
Narrative section headers that explain what each graph shows and how to read it.

**Strategic Use:**
Baseline performance assessment; Stakeholder updates; Early-stage strategy alignment

**Notes for interpretation:**
- Core and Potential audience shares are complementary (Potential = 100% - Core), shown on the same 0-100% scale.
- Tag totals are directional because tags can overlap (not mutually exclusive).

## Rendering Bundle A

- Single talent:
  `Rscript scripts/run/bundle_A/render_bundle_A.R --talent Ava`
- Batch (comma-separated):
  `Rscript scripts/run/bundle_A/render_bundle_A.R --talents "Ava,Avaritia"`
- Batch from file (one talent per line):
  `Rscript scripts/run/bundle_A/render_bundle_A.R --talents-file notes/talent_list.txt`
- Render all available talents:
  `Rscript scripts/run/bundle_A/render_bundle_A.R --all`
- Custom output directory:
  `Rscript scripts/run/bundle_A/render_bundle_A.R --talent Ava --output-dir reports/bundle_A`
- Custom output filename prefix:
  `Rscript scripts/run/bundle_A/render_bundle_A.R --talent Ava --output-prefix Bundle_A_client_v1`
- Custom input Rmd:
  `Rscript scripts/run/bundle_A/render_bundle_A.R --input templates/reports/Bundle_A/Bundle_A.Rmd --talent Ava`
- Quiet render (less console output):
  `Rscript scripts/run/bundle_A/render_bundle_A.R --talent Ava --quiet`
