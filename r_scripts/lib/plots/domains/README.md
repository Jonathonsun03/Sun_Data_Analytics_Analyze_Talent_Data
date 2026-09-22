# Domain Plot Modules

Reusable plot preparation and rendering functions are organized by analytical
domain rather than by report or dashboard. Source `load.R` after the shared
plot themes and Bundle A formatting helpers are available.

## Layout

- `common/`: shared metric preparation and plotting primitives
- `content_strategy/`: collaboration, topic, and tag analysis
- `publishing/`: day-of-week and topic-weekday analysis
- `audience/`: demographic and geography visualizations

`audience/viewer_activity.R` provides `viewer_activity_bipartite_prep()` for
the user--video engagement node and edge tables, and
`viewer_activity_bipartite_ggplot()` for its reusable static graph.

Functions should identify their output contract explicitly:

- `_prep`: prepared data frame or list
- `_ggplot`: ggplot object
- `_plotly`: Plotly/htmlwidget output
- `_table`: table data or table widget

Dashboard adapters may select data, provide empty states, and apply
dashboard-specific layout, but reusable plot construction belongs here.
