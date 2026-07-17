# Creator Dashboard Library

The creator dashboard library is organized by responsibility. Dashboard
entrypoints should source only `load.R`, which loads shared dependencies and
dashboard modules in an explicit order.

## Layout

- `data/`: source access, filtering, and final dashboard-data assembly
- `metrics/`: domain-specific summary and metric preparation
- `recommendations/`: recommendation schema, domain rules, and story assembly
- `ui/`: reusable tables, cards, empty states, and other HTML components
- `adapters/`: dashboard-specific adapters around reusable report plots
- `load.R`: the composition root and public loading entrypoint

Reusable plot implementations live under `r_scripts/lib/plots/domains/`.
Adapters may select data, handle dashboard empty states, and apply
dashboard-specific Plotly or card formatting, but should not become a second
plot library.

## Naming

Filenames use their layer and domain rather than repeating `dashboard_`; the
parent directory already provides that context. Existing function names are
temporarily retained as the public API during the structural refactor.

New or renamed functions should put responsibility immediately after the
dashboard namespace, for example `dash_data_*`, `dash_metric_*`, `dash_rec_*`,
`dash_ui_*`, or `dash_plotly_*`. General plotting helpers should identify their
return type with suffixes such as `_prep`, `_ggplot`, `_plotly`, `_table`, or
`_card`.
