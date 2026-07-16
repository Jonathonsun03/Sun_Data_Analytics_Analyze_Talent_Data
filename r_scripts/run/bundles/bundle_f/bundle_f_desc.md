## Bundle F: Day-of-Week Performance

### 1) Who this report is for
- Talent managers and creators who want to understand whether publish weekday appears associated with stronger or weaker viewership.
- Scheduling and content-planning workflows that need a compact, repeatable weekday read for live streams, videos, and Shorts.

### 2) What this bundle answers
- Which weekdays have the highest mean and median views for each content type?
- How much evidence exists for each weekday comparison, based on upload counts and confidence intervals?
- Does weekday explain meaningful view variation in simple raw-view and log-view models?
- How do live streams, videos, and Shorts differ in weekday performance patterns?

### 3) Included analyses
1. Weekday summary tables by content type.
2. Median-ranked weekday tables by content type.
3. Raw-view weekday linear models.
4. Log-view weekday linear models.
5. Approximate percentage differences from the selected reference weekday.
6. Raw, mean-with-confidence-interval, and log-view plots by weekday.

### 4) Interpretation guardrails
- This report is observational, not causal.
- Weekday effects can be confounded by content topic, event timing, collaboration, seasonality, duration, and platform recommendation behavior.
- Small weekday sample sizes should be treated as directional evidence only.
- Log-view models are usually more stable when a small number of unusually large uploads dominate raw view counts.

### 5) Data requirements
- Video analytics exports with stable video identifiers, view counts, published timestamps, and content type.
- Title classification enrichment is used when available by the shared video prep pipeline.

### 6) Bundle F concept summary
- Bundle F turns the existing day-of-week exploratory notebook into a scheduled report bundle.
- It shares the day-of-week modeling and display helpers under `r_scripts/lib/linear_regression/day_of_week/`.
- It renders into the standard per-talent report layout under `reports/bundle_f/`.
