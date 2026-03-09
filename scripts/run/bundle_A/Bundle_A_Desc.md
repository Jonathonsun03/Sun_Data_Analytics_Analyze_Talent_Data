## Bundle A: Performance & Growth Overview

**Best for clients asking:**
“How is the channel performing overall, and where are the major changes?”

**Included Analyses:**
Performance Trends Over Time; Engagement Metrics by Content Category; Demographic Composition Over Time

**Key Questions Answered:**
How have views and revenue changed over time? Which content types drive stronger engagement? Who is the core audience, and how stable is it?

**Delivered Outputs:**
Time-series views and revenue visualizations; Engagement distributions by content type; Audience age and gender trends; Narrative interpretation of growth patterns

**Strategic Use:**
Baseline performance assessment; Stakeholder updates; Early-stage strategy alignment

## Rendering Bundle A

- Single talent:
  `Rscript scripts/run/bundle_A/render_bundle_A.R --talent Ava`
- Batch (comma-separated):
  `Rscript scripts/run/bundle_A/render_bundle_A.R --talents "Ava,Avaritia"`
- Batch from file (one talent per line):
  `Rscript scripts/run/bundle_A/render_bundle_A.R --talents-file notes/talent_list.txt`
- Render all available talents:
  `Rscript scripts/run/bundle_A/render_bundle_A.R --all`
