# Chatter Community Dashboard

`dashboard.qmd` is the authenticated Quarto/Shiny application for exploring
observed live-chat communities. It remains separate from the creator analytics
dashboard because its filters use stream dates and its interactive D3 payloads
have a different performance profile from analytics-snapshot reporting.

The application reuses the shared viewer-activity loaders and visualization
components. Its Overview, Chatter Flows, Participation, Audience Overlap, and
Methodology pages are ordinary top-level dashboard tabs that all use one
applied talent, stream-date window, and minimum message threshold. The visual
system uses a compact 260-pixel filter rail, a single horizontal summary bar, navy
typography, selective blue accents, and flat analytical sections. The page is
the primary vertical scroll container; chart frames retain horizontal overflow
only when preserving readable labels requires it at narrow widths.

The summary combines the applied talent, selected period, message total, and
three audience metrics in one reactive output. It opts out of Quarto's fill
layout so its content reserves space before Community Layers. The stylesheet
is registered as an HTML dependency, allowing Shiny to serve it from its
resource directory (a plain relative Quarto CSS link returned 404 in previews).

## Authorization

Production authorization fails closed. The trusted Cloudflare permissions
proxy must provide both headers on every request:

- `X-SDA-Verified-Email`
- `X-SDA-Allowed-Talent-Codes`

The application intersects the exact allowed codes with active talents before
populating the selector and validates the selected talent again before every
community-data load. A browser-supplied talent code cannot broaden access.

For an explicit local-only preview, use the same development settings as the
creator dashboard:

```bash
export DASHBOARD_AUTH_MODE=development
export DASHBOARD_DEV_ALLOWED_TALENT_CODES=LEI3,AVA1
export DASHBOARD_DEV_EMAIL=local@example.com
```

## Run locally

From the repository root:

```bash
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
  quarto serve \
  r_scripts/notebooks/dashboards/community_dashboard/dashboard.qmd \
  --host 127.0.0.1 \
  --port 3841
```

Do not expose the application port directly. Put it behind the same
authenticated permissions proxy used by the creator dashboard.
