# Company Performance Dashboard

## Goal

This dashboard is the internal analytical source of truth for company-level and
talent-level performance. It brings validated metrics, comparisons, content
patterns, audience evidence, representative examples, and data-quality context
into one place so those findings can support many downstream uses.

## Role boundary

The dashboard reflects the role of a data provider performing analysis. It is
not a bid packet, media kit, sales tool, pricing tool, sponsorship proposal, or
product pitch. It should provide defensible evidence without selling the
company, predicting partnership outcomes, or converting observations into
marketing claims.

Management or design collaborators may later use dashboard evidence in pitch
decks, graphics, company reports, investor materials, partnership discussions,
or event proposals. Those downstream materials remain separate deliverables.

## Current scope

- Select a configured company.
- Include all authorized company talents or a selected subset.
- Select an analytics snapshot window using the same semantics as the creator
  dashboard.
- Aggregate each selected talent at its latest available snapshot inside that
  window, preventing cumulative daily snapshots from being double-counted.
- Report company totals for views, releases, total content duration, and
  video-page subscription events.
- Reconcile company totals through a per-talent breakdown and snapshot-coverage
  table.
- Analyze monthly publishing, talent-level weekday publishing, collaborations,
  classified topics, and included references.
- Compare average, median, watch-time, retention, and video-page subscription
  performance by content type and topic.
- Show talent contribution shares, content mix, collaboration-tagged share, and
  topic breadth.
- Explore searchable video-level evidence and reproducibly selected
  representative content.
- Download company, talent, content, and video-level tables for downstream
  analysis or design.
- Report fun statistics, metric definitions, source coverage, and unsupported
  audience fields without estimating missing values.

Company membership is maintained in
`config/dashboard/company_talents.csv`. Add one row per company/talent pair when
another company should become available.

## Analytical roadmap note

This iteration implements the roadmap as a broad analytical platform without
turning the dashboard into a bid packet:

1. **Typical performance:** average and median views, video-page subscription
   events per release, and comparisons by content type and topic.
2. **Audience attention:** watch hours, aggregate average view duration,
   views-weighted average view percentage, views per content hour,
   and video-page subscription events per 1,000 views.
3. **Talent portfolio:** each talent's share of releases, views, watch time, and
   video-page subscription events, plus content mix, collaboration-tagged
   share, and topic breadth.
4. **Content relevance:** topic, reference, format, and collaboration
   performance with top-three-per-talent representative content selected by a
   visible cumulative-view criterion.
5. **Representative-content explorer:** searchable video details,
   classifications, observed performance, engagement measures, and source URLs.
6. **Fun statistics:** continuous release days, common release day, busiest
   publishing month, topic breadth, and view milestones.
7. **Data extraction:** downloadable company, talent, topic, and video-level CSV
   tables that retain the active filters.
8. **Methodology and quality:** definitions, contributing-row counts, included
   talents, snapshot dates, missing-data indicators, and interpretation limits.

Audience geography, age, gender, unique viewers, unique reach, and average
concurrent livestream viewers remain explicitly unavailable because the current
source does not provide those fields. They should be added only when exact
source metrics become available, never inferred from total views.

The roadmap deliberately excludes sponsorship rates, partnership packages,
contact details, sales language, predicted campaign results, and bid-packet or
media-kit generation.

## Local preview

The dashboard uses fail-closed talent authorization. For a local Variance
preview from this directory:

```bash
export DASHBOARD_AUTH_MODE=development
export DASHBOARD_DEV_ALLOWED_TALENT_CODES=AVA1,KAT2,LEI3
export DASHBOARD_DEV_EMAIL=local@example.com

quarto serve dashboard.qmd --browser
```

Production access must continue to use the trusted Cloudflare headers described
in `r_scripts/lib/dashboard/README.md`.
