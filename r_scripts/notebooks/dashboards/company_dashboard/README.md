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
- Report company totals for views, releases, streamed time, and subscribers
  gained.
- Reconcile company totals through a per-talent breakdown and snapshot-coverage
  table.
- Analyze monthly publishing, talent-level weekday publishing, collaborations,
  classified topics, and included references.

Company membership is maintained in
`config/dashboard/company_talents.csv`. Add one row per company/talent pair when
another company should become available.

## Analytical roadmap note

Future development should expand the dashboard as a broad analytical platform,
not turn it into a bid packet. Candidate additions are:

1. **Typical performance**
   - Average, median, and quartile views per release.
   - Subscriber performance per release.
   - Comparisons by talent, content type, and topic.
2. **Audience attention**
   - Watch hours, average view duration, and views-weighted average view
     percentage.
   - Views per streamed hour and subscribers gained per 1,000 views.
   - Average concurrent livestream viewers only when that exact source metric
     is available.
3. **Audience profile**
   - Geography, age, gender, and format differences where source coverage is
     sufficient.
   - Clear distinctions among total views, unique viewers, and unique reach.
4. **Talent portfolio**
   - Each talent's share of views, watch time, releases, and subscriber gains.
   - Content mix, collaboration rate, engagement, and topic breadth by talent.
5. **Content relevance**
   - Topic, reference, franchise, format, and collaboration performance.
   - Representative content selected with visible, reproducible criteria.
6. **Representative-content explorer**
   - Searchable video details, classifications, performance, engagement, and
     source links.
7. **Fun statistics**
   - Graphic-friendly but factual observations such as continuous content days,
     common release days, publishing peaks, topic breadth, and milestone counts.
8. **Data extraction**
   - Company, talent, content, audience, and representative-video tables that
     can be exported for downstream analysis or design work.
9. **Methodology and quality**
   - Definitions, contributing-row counts, included talents, snapshot dates,
     missing-data indicators, and interpretation limits for reusable metrics.

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
