# Shared JavaScript

This directory contains browser-side components shared by R notebooks,
Python-generated reports, and direct HTML pages.

## Layout

- `vendor/`: pinned third-party browser libraries and their licenses.
- `lib/`: reusable project-owned JavaScript components.
- `styles/`: styles for those components.

`lib/viewer_activity_network.js` exposes
`SunDataNetwork.renderViewerActivityNetwork(element, data, options)`. The
renderer accepts ordinary node and link objects, so callers do not need to be
written in R. See the function's JSDoc block for the data contract.

`lib/community_shape.js` adds reusable community-layer, engagement-landscape,
and audience-overlap renderers. These complement the node-link graph with
full-population and adjacency-matrix views that remain readable for large
chatter communities. The node-link and overlap components provide searchable
multi-video selectors without requiring a server-side application.

`lib/video_explorer.js` uses a compact, dictionary-encoded activity payload to
offer every video in the selected data scope. It materializes only the checked
videos into the network and overlap matrix, keeping large catalogs usable.

D3 is vendored locally so generated reports remain reproducible and usable
without a CDN. Update the filename and the matching dependency version in
`r_scripts/lib/plots/domains/audience/viewer_activity.R` when upgrading D3.
