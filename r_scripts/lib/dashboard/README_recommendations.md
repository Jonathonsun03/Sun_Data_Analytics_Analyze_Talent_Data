# Dashboard Recommendation System

## 1. Purpose

The creator analytics dashboard Recommendations tab is a deterministic,
rule-based story layer. It does not generate freeform AI prose at runtime.
Instead, it converts existing dashboard summary tables into structured
recommendation objects, then renders those objects as dashboard cards.

High-level flow:

```text
filtered source data
-> dashboard_data
-> summary tables
-> recommendation rules
-> structured recommendation tibble/list
-> recommendation cards
```

The recommendation layer is meant to summarize planning signals already present
elsewhere in the dashboard. It should be read as descriptive evidence, not as a
causal decision engine.

## 2. Where The Recommendation Logic Lives

Primary recommendation logic:

- `r_scripts/lib/dashboard/dashboard_recommendations.R`

Dashboard rendering helpers:

- `r_scripts/lib/dashboard/dashboard_sections.R`

Dashboard data assembly:

- `r_scripts/lib/dashboard/dashboard_data.R`

Dashboard page usage:

- `r_scripts/notebooks/dashboards/talent_dashboard/index.qmd`

Main functions:

- `dashboard_build_recommendations()`: combines all domain-specific rules,
  ranks them, creates domain subsets, selects top recommendations, and creates
  caveat rows.
- `dashboard_recommendations_publishing()`: builds publishing/timing
  recommendations from weekday and weekend/weekday summaries.
- `dashboard_recommendations_content_strategy()`: builds topic, tag, and
  collaboration recommendations.
- `dashboard_recommendations_audience()`: builds audience demographic and
  geography recommendations.
- `dashboard_recommendations_performance()`: builds top-video concentration
  and content-type performance recommendations.
- `dashboard_recommendation()`: creates one structured recommendation row.
- `dashboard_recommendation_confidence()`: assigns `insufficient`, `weak`,
  `moderate`, or `strong` from sample size and lift.
- `dashboard_recommendation_priority()`: assigns `high`, `medium`, or `low`
  from confidence and lift.
- `dashboard_recommendation_bind()`: safely combines recommendation tibbles and
  ignores `NULL` or empty inputs.
- `dashboard_recommendation_cards()`: renders recommendation rows as dashboard
  cards.
- `dashboard_recommendation_caveat_cards()`: renders data-limit and weak-signal
  caveats.

The final recommendation object is attached in `dashboard_data.R`:

```r
out$recommendations$story <- dashboard_build_recommendations(out)
```

The dashboard then calls this object in `index.qmd`, for example:

```r
dashboard_recommendation_cards(dashboard_data$recommendations$story$top)
dashboard_recommendation_cards(dashboard_data$recommendations$story, domain = "publishing")
dashboard_recommendation_cards(dashboard_data$recommendations$story, domain = "content")
dashboard_recommendation_cards(dashboard_data$recommendations$story, domain = "audience")
dashboard_recommendation_cards(dashboard_data$recommendations$story, domain = "performance")
dashboard_recommendation_caveat_cards(dashboard_data$recommendations$story)
```

## 3. Recommendation Object Schema

`dashboard_recommendation()` returns a one-row tibble with a fixed schema.
`dashboard_recommendation_empty()` returns the same columns with zero rows.

Fields:

- `domain`: Recommendation domain. Current values are `publishing`, `content`,
  `audience`, and `performance`. Used for grouping and filtering cards.
- `priority`: Planning priority: `high`, `medium`, or `low`. Used for sorting
  and displayed on cards.
- `title`: Short card title shown at the top of the recommendation card.
- `finding`: Plain-language statement of what the rule observed.
- `recommendation`: Suggested action or interpretation. Should remain
  descriptive and evidence-bound.
- `evidence`: Numeric/supporting evidence shown on the card.
- `caveat`: Limitation or interpretation warning shown on the card.
- `metric_name`: Name of the primary metric behind the rule. Useful for audit
  and debugging.
- `metric_value`: Numeric value for the winning or focal group.
- `comparison_value`: Numeric baseline or comparison value when available.
- `sample_size`: Sample size used by the rule, usually video count or minimum
  group count.
- `confidence`: Evidence confidence: `strong`, `moderate`, `weak`, or
  `insufficient`. Used for sorting, filtering, caveat extraction, and the
  confidence badge.
- `supporting_plot_or_table`: Dashboard plot/table that supports the rule.
  Shown indirectly through documentation and useful for audit.
- `rule_id`: Stable identifier for the rule. Useful for debugging and future
  tests.
- `source_table`: Source object inside `dashboard_data`, such as
  `weekday_summary` or `content_strategy$topic_summary`.
- `sort_order`: Numeric order within and across domains. Used after priority
  and confidence sorting.
- `data_limit_flag`: Logical flag for recommendations that primarily report
  missing/weak evidence. Used to sort data-limit cards lower and to populate
  the caveats list.

Simplified example:

```r
dashboard_recommendation(
  domain = "content",
  priority = "medium",
  title = "Test more around topic: music",
  finding = "`music` has the strongest average views per video among groups with enough examples.",
  recommendation = "Use `music` as a candidate for repeatable testing, while checking fit with audience and production constraints.",
  evidence = "Average views per video: 1,200 (15% vs the weighted group baseline) across 8 videos.",
  caveat = "Groups can overlap with format, publishing timing, collaboration, and title effects. This rule is descriptive.",
  metric_name = "average views per video",
  metric_value = 1200,
  comparison_value = 1043,
  sample_size = 8,
  confidence = "moderate",
  supporting_plot_or_table = "Content Strategy: Topic Performance",
  rule_id = "content_top_topic",
  source_table = "content_strategy$topic_summary",
  sort_order = 200
)
```

## 4. Confidence And Priority Logic

### `dashboard_recommendation_confidence()`

Current signature:

```r
dashboard_recommendation_confidence(
  sample_size,
  lift = NA_real_,
  min_strong_n = 8,
  min_moderate_n = 4
)
```

Logic:

- `insufficient`: `sample_size` is missing or less than `2`.
- `strong`: `sample_size >= min_strong_n` and finite absolute lift is at least
  `20%`.
- `moderate`: `sample_size >= min_moderate_n` and either lift is not finite or
  finite absolute lift is at least `10%`.
- `weak`: anything else that is not insufficient.

The default thresholds are:

```text
min_strong_n = 8
min_moderate_n = 4
strong lift threshold = abs(lift) >= 0.20
moderate lift threshold = abs(lift) >= 0.10
minimum usable sample size = 2
```

Some rules override the sample-size thresholds:

- Weekend vs weekday uses `min_strong_n = 10`, `min_moderate_n = 5`.
- Collaboration lift uses `min_strong_n = 5`, `min_moderate_n = 3`.

### `dashboard_recommendation_priority()`

Current logic:

- `high`: confidence is `strong` and lift is missing/non-finite or absolute
  lift is at least `20%`.
- `medium`: confidence is `strong` or `moderate`.
- `low`: all other cases.

These are heuristic thresholds. They should be reviewed as more dashboards are
produced and as the recommendation system matures.

## 5. Domain-Specific Recommendation Rules

### Publishing Recommendations

Function:

```r
dashboard_recommendations_publishing(dashboard_data)
```

Primary inputs:

- `dashboard_data$weekday_summary`
- `dashboard_data$weekend_weekday_distribution`

Implemented rules:

#### Best weekday by per-video metric

The rule looks for a per-video views metric in `weekday_summary`, preferring:

1. `MedianViewsPerVideo`
2. `AverageViewsPerVideo`

It also requires a sample-size column found by `dashboard_views_count_col()`,
which checks names such as `VideoCount`, `VideoCountViews`, `video_count`,
`videos`, and `tracked_videos`.

Eligibility:

- A weekday must have at least `3` videos.
- The selected metric and sample size must be finite.

Baseline:

- Weighted mean of the weekday metric across available weekdays.
- Weights are `pmax(video_count, 1)`.

Lift:

```text
best weekday metric / weighted weekday baseline - 1
```

The resulting rule id is:

```text
publishing_best_weekday
```

Safeguard:

- The rule avoids total-volume fields and uses per-video metrics only.

Fallbacks:

- `publishing_weekday_missing`: no weekday summary.
- `publishing_weekday_incomplete`: missing per-video metric or sample-size
  column.
- `publishing_weekday_small_n`: no weekday has at least three videos.

#### Weekend vs weekday comparison

The rule uses:

```r
dashboard_data$weekend_weekday_distribution$summary
```

It looks for metric labels:

- `Views per video`
- `View per video`

And groups:

- `Weekday`
- `Weekend`

It computes:

```text
weekend average / weekday average - 1
```

The sample size is the smaller of the weekday and weekend video counts. It uses
stricter confidence thresholds:

```text
min_strong_n = 10
min_moderate_n = 5
```

The resulting rule id is:

```text
publishing_weekend_weekday
```

### Content Strategy Recommendations

Function:

```r
dashboard_recommendations_content_strategy(dashboard_data)
```

Primary inputs:

- `dashboard_data$content_strategy$topic_summary`
- `dashboard_data$content_strategy$tag_summary`
- `dashboard_data$content_strategy$collab_summary`

#### Top topic signal

Uses `dashboard_top_group_recommendation()` with:

```text
group_col = "topic_group"
rule_id = "content_top_topic"
source_table = "content_strategy$topic_summary"
supporting_plot_or_table = "Content Strategy: Topic Performance"
```

The helper chooses the first available metric from:

1. `AverageViewsPerVideo`
2. `MedianViewsPerVideo`
3. `AverageRevenuePerVideo`

It requires a count column from `dashboard_views_count_col()` and filters out:

- missing groups;
- empty group labels;
- `Other topics`;
- non-finite metrics/counts.

Eligibility:

- Group must have at least `3` videos.

Baseline:

- Weighted mean across groups, weighted by group sample size.

#### Top tag signal

Also uses `dashboard_top_group_recommendation()` with:

```text
group_col = "tag_group"
rule_id = "content_top_tag"
source_table = "content_strategy$tag_summary"
supporting_plot_or_table = "Content Strategy: Tag Performance"
```

It uses the same metric selection, sample-size rule, and weighted baseline
logic as the topic rule.

#### Collaboration lift signal

Uses:

```r
dashboard_data$content_strategy$collab_summary
```

Required columns:

- `collab_group`
- `AverageViewsPerVideo`

Expected groups:

- `Collaborative`
- `Non-collaborative`

Lift:

```text
Collaborative average views per video / Non-collaborative average views per video - 1
```

Sample size:

- Collaboration group sample size from the available count column.

Confidence thresholds:

```text
min_strong_n = 5
min_moderate_n = 3
```

The resulting rule id is:

```text
content_collaboration_signal
```

Current limitation:

- The collaboration recommendation currently emphasizes
  `AverageViewsPerVideo`. Revenue fields may be plotted elsewhere, but this
  rule does not currently produce a separate revenue-based collaboration
  recommendation.

Fallback:

- `content_strategy_fallback`: emitted when topic, tag, and collaboration
  rules all fail to produce a supported signal.

### Audience Recommendations

Function:

```r
dashboard_recommendations_audience(dashboard_data)
```

Primary inputs:

- `dashboard_data$audience_summary`
- `dashboard_data$audience_geography`

#### Largest age/gender segment

Required columns:

- `.age`
- `.gender`
- `share`

The rule selects the latest `.period`, then finds the age/gender segment with
the largest `share`.

Sample size:

- Uses the `videos` column from the latest period.

Confidence:

- Uses `dashboard_recommendation_confidence()` with default thresholds.
- Uses audience share as the lift-like value for thresholding.

The resulting rule id is:

```text
audience_top_segment
```

Caution:

- Audience demographic values are shares/percentages, not raw viewer counts.

#### Largest geography signal

Required columns:

- `snapshot_date`
- `country_code`
- `metric_value`

The rule selects the latest geography snapshot and takes the highest
`metric_value`.

Labeling:

- Uses `country_name` if available.
- Falls back to `country_code`.

Confidence:

- Hard-coded as `moderate`.

Priority:

- Hard-coded as `medium`.

The resulting rule id is:

```text
audience_top_geography
```

Caution:

- Geography exports may be share-based or estimated.
- Unmatched country codes are omitted from the map.

Fallback:

- `audience_fallback`: emitted when no audience demographic or geography rule
  produces a supported signal.

### Performance Recommendations

Function:

```r
dashboard_recommendations_performance(dashboard_data)
```

Primary inputs:

- `dashboard_data$top_videos`
- `dashboard_data$source_data$analytics`
- `dashboard_data$content_summary$average_views`

#### Top-video concentration

Required fields:

- `top_videos$views`
- `source_data$analytics$views`

The rule computes the share of total observed views accounted for by the top
five videos, or fewer if fewer are available:

```text
sum(top N video views) / sum(all analytics views)
```

Priority:

- `high` if top share is at least `50%`.
- `medium` otherwise.

Confidence:

- Uses `dashboard_recommendation_confidence(top_n, top_share,
  min_strong_n = 5, min_moderate_n = 3)`.

The resulting rule id is:

```text
performance_top_video_concentration
```

#### Best content type by average views per video

Uses:

```r
dashboard_data$content_summary$average_views
```

Required columns:

- `Content Type`
- `Average_Views`
- `VideoCount`

Eligibility:

- Content type must have at least `3` videos.

Baseline:

- Weighted mean of `Average_Views`, weighted by `VideoCount`.

Lift:

```text
best content-type average / weighted format baseline - 1
```

The resulting rule id is:

```text
performance_content_type_average_views
```

Fallback:

- `performance_fallback`: emitted when top-video concentration or content-type
  average summaries are unavailable or too sparse.

## 6. Ranking And Grouping

`dashboard_build_recommendations()` combines all domains:

```r
all <- dashboard_recommendation_bind(
  dashboard_recommendations_publishing(dashboard_data),
  dashboard_recommendations_content_strategy(dashboard_data),
  dashboard_recommendations_audience(dashboard_data),
  dashboard_recommendations_performance(dashboard_data)
)
```

It then coerces `priority` and `confidence` to ordered factors:

```text
priority: high -> medium -> low
confidence: strong -> moderate -> weak -> insufficient
```

Sorting order:

1. `data_limit_flag`
2. `priority`
3. `confidence`
4. `sort_order`

Because `data_limit_flag` sorts first, ordinary actionable recommendations
appear before data-limit fallback rows.

The returned object is a list:

- `all`: every generated recommendation row.
- `top`: first five actionable recommendations.
- `publishing`: `all` filtered to `domain == "publishing"`.
- `content`: `all` filtered to `domain == "content"`.
- `audience`: `all` filtered to `domain == "audience"`.
- `performance`: `all` filtered to `domain == "performance"`.
- `caveats`: rows where `data_limit_flag` is true or confidence is `weak` or
  `insufficient`, reduced to audit fields.

Top recommendations are selected from:

```r
actionable <- all %>%
  dplyr::filter(!.data$data_limit_flag, .data$confidence != "insufficient")

top <- actionable %>% dplyr::slice_head(n = 5)
```

## 7. Rendering On The Dashboard

`dashboard_recommendation_cards()` accepts either:

- a recommendation data frame; or
- the recommendation story list created by `dashboard_build_recommendations()`.

If a story list is passed, the function selects:

```r
recommendations$all
```

or, when a domain is supplied:

```r
recommendations[[domain]]
```

Each card renders:

- `title`
- confidence badge from `confidence`
- priority/domain line from `priority` and `domain`
- `finding`
- `recommendation`
- `evidence`
- `caveat`

Badge colors are local to the card renderer:

- `strong`: green
- `moderate`: blue
- `weak`: brown/yellow
- `insufficient`: gray

If no recommendations are available, the card renderer returns:

```text
Not enough evidence for a recommendation in this area for the active filters.
```

`dashboard_recommendation_caveat_cards()` renders up to six caveats from:

```r
recommendation_story$caveats
```

Each caveat is shown as:

```text
<title>: <caveat>
```

If no caveats are available, it returns an empty-state message.

## 8. Design Principles And Safeguards

Current safeguards:

- Recommendations are descriptive, not causal.
- Per-video metrics are preferred over total-volume metrics when recommending
  timing, topics, tags, formats, or collaboration.
- Strong recommendations require both sample size and lift.
- Small samples produce weak or insufficient recommendations.
- Missing or incomplete data produces fallback rows instead of forced advice.
- Views and revenue are conceptually separate, even where both appear in source
  summaries.
- Rules should not hard-code talent names or date ranges.
- Rules operate on `dashboard_data`, which has already been filtered by the
  dashboard data pipeline, including `start_date` and `end_date`.
- Recommendation cards should cite a supporting plot/table via
  `supporting_plot_or_table`.
- Generic advice without a supporting metric should be avoided.

## 9. Known Limitations

Current limitations implied by the code:

- Cross-tab insights are only partially encoded. For example, a topic can look
  strong in lifecycle/longevity but not become a content recommendation if it
  does not meet the Content Strategy summary rule criteria.
- Topic and tag recommendations use a first-available metric order:
  `AverageViewsPerVideo`, then `MedianViewsPerVideo`, then
  `AverageRevenuePerVideo`. This means revenue may not drive the recommendation
  when a views metric is present.
- Collaboration recommendations currently emphasize average views per video.
  A separate revenue-based collaboration rule may be useful later.
- Recommendation thresholds are heuristic.
- Causal claims are not supported.
- Sparse data can suppress recommendations or produce fallback/data-limit
  cards.
- Some audience geography recommendations use a fixed `moderate` confidence
  rather than sample-size/lift confidence.
- The card renderer does not currently expose `supporting_plot_or_table`,
  `rule_id`, or `source_table` on the visible card, though those fields are
  available in the underlying tibble.

## 10. How To Add A New Recommendation Rule

Use this pattern when extending the system:

1. Identify the source summary table in `dashboard_data`.
2. Confirm the table is already filtered by the active talent/date/content-type
   settings.
3. Define the metric and comparison baseline.
4. Set minimum sample-size requirements.
5. Calculate lift or another comparison metric.
6. Assign confidence with `dashboard_recommendation_confidence()`.
7. Assign priority with `dashboard_recommendation_priority()` unless the rule
   has an explicit reason to override priority.
8. Return a row with `dashboard_recommendation()`.
9. Add the rule to the correct domain function.
10. Add a fallback if missing or sparse data is possible.
11. Confirm the card renders correctly.
12. Test with at least one normal dataset and one sparse/missing-data dataset.

Pseudo-code:

```r
new_rule <- NULL
df <- dashboard_data$some_summary

if (!is.null(df) && nrow(df) > 0 && all(c("group", "metric", "n") %in% names(df))) {
  dat <- df %>%
    dplyr::mutate(
      .metric = suppressWarnings(as.numeric(.data$metric)),
      .n = suppressWarnings(as.numeric(.data$n))
    ) %>%
    dplyr::filter(is.finite(.data$.metric), is.finite(.data$.n), .data$.n >= 3)

  if (nrow(dat) > 0) {
    best <- dat %>%
      dplyr::arrange(dplyr::desc(.data$.metric), dplyr::desc(.data$.n)) %>%
      dplyr::slice_head(n = 1)

    baseline <- stats::weighted.mean(dat$.metric, w = pmax(dat$.n, 1), na.rm = TRUE)
    lift <- if (is.finite(baseline) && baseline > 0) best$.metric[[1]] / baseline - 1 else NA_real_
    confidence <- dashboard_recommendation_confidence(best$.n[[1]], lift)

    new_rule <- dashboard_recommendation(
      domain = "content",
      priority = dashboard_recommendation_priority(confidence, lift),
      title = "Short title for the card",
      finding = "What the rule observed.",
      recommendation = "What to consider doing next.",
      evidence = paste0("Metric: ", dashboard_format_metric(best$.metric[[1]], "views")),
      caveat = "Why this should not be overread.",
      metric_name = "average views per video",
      metric_value = best$.metric[[1]],
      comparison_value = baseline,
      sample_size = best$.n[[1]],
      confidence = confidence,
      supporting_plot_or_table = "Relevant Dashboard Card",
      rule_id = "content_new_rule_id",
      source_table = "some_summary",
      sort_order = 230
    )
  }
}
```

Then include the new rule in the relevant domain function:

```r
dashboard_recommendation_bind(existing_rule, new_rule, fallback)
```

## 11. Testing And Validation Checklist

Before merging a new recommendation rule, check:

- Does the rule respect the active talent/date/content-type filters by using
  `dashboard_data` rather than reloading raw data independently?
- Does it avoid hard-coded talent names, topic labels, date windows, or client
  assumptions?
- Does it use per-video metrics when appropriate?
- Does it avoid recommending from total volume alone?
- Does it have a sample-size threshold?
- Does it compute a clear baseline, lift, share, or comparison metric?
- Does it assign confidence consistently?
- Does it produce fallback text when data is missing or too sparse?
- Does it avoid causal language?
- Does it include a useful `supporting_plot_or_table` value?
- Does it include a stable `rule_id`?
- Does it include a real `source_table` pointer?
- Does it produce a valid recommendation tibble with the standard schema?
- Does it appear in `dashboard_data$recommendations$story$all`?
- Does it route to the correct domain subset?
- Does it render correctly in the Recommendations tab?
- Does it behave sensibly for both normal and sparse datasets?

