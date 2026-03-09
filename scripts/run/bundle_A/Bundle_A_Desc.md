## Bundle A: Performance and Growth Overview

### 1) Who this report is for
- Talent managers who need a fast performance readout and clear areas to prioritize.
- Talent/creators who want format, timing, audience, and collaboration insights.

### 2) What this bundle answers
- Is channel performance improving or declining over time (views and revenue)?
- Which content types perform best by total output and by per-video average?
- Is audience composition stable, concentrated, or broadening?
- Do timing choices (weekday/weekend/day-of-week) matter?
- Do collaborations outperform non-collaborative uploads on a per-video basis?
- Which topics and tags are associated with stronger outcomes?

### 3) How to navigate quickly (recommended reading paths)
Manager fast path (5-10 minutes):
1. Overall Performance Snapshot
2. Trends Over Time
3. Collaboration Effectiveness
4. Wrap-up

Talent/creator optimization path:
1. Engagement Distribution by Content Type
2. Timing Effects (Weekend/Weekday + Day-of-Week)
3. Topic Performance
4. Tag Performance and Average Views per Tag

Audience strategy path:
1. Audience Age + Gender Trends
2. Core vs Potential Audience Stability

### 4) Section map (question -> where to look)
- Performance pulse -> Combined Performance Trends (Views + Revenue)
- Total vs per-video format performance -> Views by Content Type (Total vs Average)
- Revenue mix by format -> Total Revenue by Content Type
- Engagement consistency/volatility -> Engagement Distribution + Median Engagement
- Audience stability -> Core Audience Stability
- Monetization efficiency -> Monetization Quality (Views to Dollars)
- Timing decisions -> Weekend vs Weekday + Day-of-Week
- Collaboration value -> Collaboration Effectiveness
- Creative direction -> Topic Performance + Tag Performance + Average Views per Tag

### 5) Included analyses (full scope)
1. Overall snapshot:
Views by content type (total and average per video), combined views + revenue trends, total revenue by content type, engagement distribution, median engagement by content type.
2. Trends over time:
Views and revenue by content type by month.
3. Audience composition:
Audience age + gender trends and Core vs Potential audience stability.
4. Monetization quality:
Views-to-dollars ratio table.
5. Content-type deep dive:
Weekend vs weekday distribution, day-of-week contribution, collaboration effectiveness (average per video and lift), topic performance, tag performance (total + average views per tag).

### 6) Interpretation guardrails
- This report is observational, not causal. Differences across groups do not prove one factor caused the outcome.
- Tag/topic effects can be confounded by release timing, promotion, seasonality, and publishing volume.
- Core and Potential audience shares are complementary (Potential = 100% - Core), plotted on the same 0-100% scale.
- Tag totals are directional because tags can overlap (tags are not mutually exclusive).

### 7) Data requirements
- Talent-level staging data must be available under the configured staging root.
- Required source domains: analytics, monetary, and demographics for each talent.
- Title classification export must be available for content-type/topic/tag enrichment.
- Expected report template: `templates/reports/Bundle_A/Bundle_A.Rmd`.

### 8) Rendering Bundle A
- Single talent:
  `Rscript scripts/run/bundle_A/render_bundle_A.R --talent Ava`
- Single talent + days-back window:
  `Rscript scripts/run/bundle_A/render_bundle_A.R --talent Ava --window-days 90`
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
- Quiet render:
  `Rscript scripts/run/bundle_A/render_bundle_A.R --talent Ava --quiet`

Common window presets:
- 90 days: `--window-days 90`
- 60 days: `--window-days 60`
- 30 days: `--window-days 30`
- 14 days: `--window-days 14`
- 7 days: `--window-days 7`
- Omit `--window-days` to use all available staged data.
- If a selected window is too narrow to include analytics + monetary rows, rendering will stop with a message to use a larger window.
- If a section has sparse data inside a valid window (for example demographics or tags), the report will show a note for that section instead of failing the whole render.
