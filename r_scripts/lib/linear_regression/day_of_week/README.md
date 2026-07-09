# Day-of-Week Linear Regression Helpers

This folder contains reusable helpers for testing whether livestream publish day is associated with viewership.

The core goal is interpretability. The helpers explicitly convert `publish_wday` to an unordered factor so `lm()` produces coefficients such as `publish_wdayTuesday`, `publish_wdayWednesday`, and so on, rather than ordered-factor polynomial terms such as `publish_wday.L`, `publish_wday.Q`, or `publish_wday.C`.

## Expected input

Use a stream-level data frame with at least:

- `views`: numeric or numeric-like total views.
- `publish_wday`: weekday labels. Abbreviated labels such as `Mon`, `Tue`, and full labels such as `Monday`, `Tuesday` are supported.

Optional columns are used only if present and non-constant:

- `stream_duration`
- `content_type`
- `topic`
- `talent_name`

The workflow does not overwrite the original input data. It creates a cleaned modeling data frame in the returned result list.

## Main workflow

Source the helper file or use `source_dir()` on this folder:

```r
source(here::here("r_scripts", "lib", "linear_regression", "day_of_week", "day_of_week_model.R"))
```

For a transparent notebook analysis where you only want the weekday recoding helpers, source:

```r
source(here::here("r_scripts", "lib", "linear_regression", "day_of_week", "weekday_transform.R"))
```

The compact notebook API is:

```r
live <- dow_prep(content$live)
videos <- dow_prep(content$videos)

live_summary <- dow_summary(live)
video_summary <- dow_summary(videos)

live_models <- dow_fit(live)
video_models <- dow_fit(videos)

live_plots <- dow_plot(live, live_summary)
```

These functions are content-agnostic. They work with any content subset that has `views` and `publish_wday`.

Then run:

```r
results <- dow_run_weekday_workflow(
  df = live_df,
  reference_day = "Monday"
)
```

This keeps all tables, models, plots, diagnostics, and recommendations in the returned `results` object. Nothing is written to disk unless you pass `output_dir`.

To save CSV and PNG artifacts, use:

```r
results <- dow_run_weekday_workflow(
  df = live_df,
  output_dir = "/mnt/datalake/DataLake/Sun_Data_Analytics/Processed/weekday_model/ava",
  reference_day = "Monday"
)
```

The returned `results` list contains:

- `weekday_df`: cleaned model-ready data.
- `weekday_summary`: weekday summary table in weekday order.
- `weekday_summary_by_median`: weekday summary table sorted by median views.
- `plots`: ggplot objects for raw and log-scale plots.
- `models`: fitted `lm()` objects.
- `reports`: model metrics and coefficient tables.
- `pairwise`: post-hoc weekday comparisons when the weekday term is statistically meaningful.
- `diagnostics`: residual tables, outlier checks, and residual plots.
- `recommendation`: final plain-English recommendation inputs.
- `output_dir`: the folder where CSV and PNG outputs were saved, or `NULL` when running notebook-only.

## Function reference

### `dow_recode_weekday_labels(x)`

Maps weekday abbreviations and labels to full weekday names. For example:

- `Mon` -> `Monday`
- `Tue` or `Tues` -> `Tuesday`
- `Thu`, `Thur`, or `Thurs` -> `Thursday`

Use this before modeling when your weekday column comes from an ordered lubridate weekday factor.

### `dow_factor_weekday(x, reference_day = "Monday")`

Converts full weekday labels into an unordered factor with levels:

`Monday`, `Tuesday`, `Wednesday`, `Thursday`, `Friday`, `Saturday`, `Sunday`

The `reference_day` becomes the intercept/reference category in `lm()`. If the requested reference day is not present in the data, the first available weekday is used.

### `dow_add_model_weekday(df, weekday_col = "publish_wday", reference_day = "Monday")`

Adds two modeling columns:

- `publish_wday_label`: full weekday text from `dow_recode_weekday_labels()`
- `publish_wday`: unordered model factor from `dow_factor_weekday()`

This is useful in notebooks because it keeps the label-mapping and factor-coding steps visible without repeating all the recode details inline.

### `dow_prep(df, views_col = "views", weekday_col = "publish_wday", reference_day = "Monday")`

Compact preparation helper for content subsets such as `content$live` or `content$videos`. It filters missing views/weekdays, converts views to numeric, adds weekday labels/factors, and creates `log_views`.

### `dow_summary(df, views_col = "views")`

Compact weekday summary helper. Returns counts, mean, median, standard deviation, standard error, min/max, and 95% confidence intervals.

### `dow_fit(df)`

Fits the two simple weekday models:

- `views ~ publish_wday`
- `log_views ~ publish_wday`

Returns a list with `raw`, `log`, and `log_effects`.

### `dow_plot(df, summary_df = dow_summary(df))`

Returns notebook-ready ggplot objects for raw views, mean views with CI, and log views. It does not save files.

### `dow_inspect_weekday(df, weekday_col = "publish_wday")`

Prints the class, levels, and ordered-factor status of the weekday column. Use this before modeling to confirm whether `publish_wday` would trigger polynomial contrasts.

### `dow_prepare_weekday_df(df, views_col = "views", weekday_col = "publish_wday", reference_day = "Monday")`

Creates a model-ready copy of the data. It:

- converts `views` to numeric
- maps weekday abbreviations to full weekday labels
- creates an unordered factor in Monday-Sunday order
- uses Monday as the reference day when present
- falls back to the first available weekday when Monday is absent
- adds `log1p_views`
- filters rows missing views or weekday

### `dow_summarise_weekdays(df, value_col = "views")`

Calculates per-weekday descriptives:

- `n`
- mean
- median
- standard deviation
- min
- max
- standard error
- 95% confidence interval around the mean

### `dow_save_weekday_plots(df, summary_df, output_dir = NULL)`

Builds four plots and returns them as ggplot objects:

- `boxplot_views_by_weekday.png`
- `mean_views_ci_by_weekday.png`
- `boxplot_log1p_views_by_weekday.png`
- `mean_log1p_views_ci_by_weekday.png`

When `output_dir` is supplied, it also saves the plots as PNG files.

### `dow_report_model(model, model_name, log_response = FALSE, output_dir = NULL)`

Prints and optionally saves:

- model formula
- R-squared
- adjusted R-squared
- overall F-test p-value
- coefficient table

For log-response models, weekday coefficients also get `approx_percent_change = exp(beta) - 1`.

### `dow_explain_weekday_coefficients(model, log_response = FALSE)`

Prints how to interpret the intercept and weekday terms. The intercept is the estimated outcome for the reference day. Each weekday coefficient is the difference from that reference day.

### `dow_select_optional_predictors(df, candidates = c("stream_duration", "content_type", "topic", "talent_name"))`

Keeps only optional predictors that exist in the data and have more than one observed value. This prevents richer models from failing when a one-talent or live-only slice has constant columns.

### `dow_run_pairwise_weekday_tests(model, model_name, output_dir = NULL)`

Runs weekday pairwise comparisons only if the overall weekday term is statistically meaningful. It uses `emmeans` with Tukey adjustment when available. If `emmeans` is not installed, it falls back to `pairwise.t.test()` with Holm correction.

### `dow_residual_diagnostics(model, model_name, source_df, output_dir = NULL)`

Creates residual diagnostics for a fitted model. It:

- saves residuals by modeled row
- prints residual summaries
- identifies largest positive and negative residuals
- counts streams with absolute standardized residual greater than 3
- saves a residual-vs-fitted plot when `output_dir` is supplied

### `dow_print_final_recommendation(weekday_summary, model_b, diagnostics_a, diagnostics_b)`

Prints a concise interpretation:

- whether weekday appears predictive in the log model
- highest average-view weekday
- highest median-view weekday
- whether raw views or log views look more appropriate
- why this should be treated as directional evidence before schedule changes

### `dow_run_weekday_workflow(df, output_dir = NULL, reference_day = "Monday", optional_predictors = ...)`

Runs the complete analysis end to end. This is the recommended entry point for notebooks.

It fits:

- Model A: `views ~ publish_wday`
- Model B: `log1p_views ~ publish_wday`
- Model C: `log1p_views ~ publish_wday + optional predictors`

Model C is only fit when optional predictors exist and vary.

## Optional Output Files

By default, the workflow writes nothing and keeps results in memory. If `output_dir` is supplied, the workflow writes:

- `weekday_summary_by_order.csv`
- `weekday_summary_by_median.csv`
- `<model_name>_metrics.csv`
- `<model_name>_coefficients.csv`
- `<model_name>_residuals.csv`
- `<model_name>_residuals_vs_fitted.png`
- weekday boxplots and mean/CI plots
- pairwise comparison CSVs when pairwise tests run

## Modeling note

Day-of-week effects can be confounded by stream duration, topic, event type, collaboration, start time, seasonality, and the streamer’s audience habits. The weekday model is useful for exploration, but it should not be the only basis for schedule decisions.
