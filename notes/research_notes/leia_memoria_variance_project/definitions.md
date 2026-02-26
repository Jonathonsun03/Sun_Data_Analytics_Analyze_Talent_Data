# Label Definitions Sheet

## Purpose
This sheet defines analytic labels used in the findings and references reports so interpretation is consistent across channels.

## Interaction Labels

| Label | Definition | Typical Values |
|---|---|---|
| Reciprocity | Degree of two-way interaction between streamer and chat (how often chat input is acknowledged and integrated). | High, Moderate, Low |
| Reciprocity: High | Frequent direct acknowledgments, visible back-and-forth, chat input regularly changes or steers in-stream talk/action. | High |
| Reciprocity: Moderate | Regular acknowledgment exists, but streamer remains primarily agenda-led and chat influence is intermittent. | Moderate |
| Reciprocity: Low | Stream is mostly host-driven with lighter or less frequent chat integration. | Low |
| Pacing | Interaction tempo, inferred from turn-taking speed and chat throughput described in summaries. | Rapid, Moderate, Slow, Unknown |
| Pacing: Rapid | Fast conversational turnover, dense chat activity, frequent quick acknowledgments. | Rapid |
| Pacing: Moderate | Balanced conversational tempo with periodic bursts and pauses. | Moderate |
| Pacing: Slow | Longer host turns, lower chat velocity, fewer rapid exchanges. | Slow |
| Pacing: Unknown | Summary text does not provide enough signal to assign pacing reliably. | Unknown |
| Chat momentum | Local pacing descriptor around paid-message windows or key moments. | Rapid overall, Moderate overall, Slow overall, Unknown |

## Monetization Labels

| Label | Definition | Typical Values |
|---|---|---|
| Paid-message presence | Whether a stream summary contains a visible paid-message inventory in sectioned analysis. | With visible paid-message inventory / Without visible paid-message inventory |
| Paid-message inventory | Listed paid-message events (single moments or clusters) captured in the summary text. | Count of visible moments |
| Paid-message count profile | Distribution of visible paid-message counts per stream across a corpus. | 0, 1, 2, ... n |
| Visible paid moment | One paid-message entry/cluster explicitly described in a summary. | Integer count unit |

## Functional Roles of Money

| Label | Definition |
|---|---|
| Support | Payment primarily framed as financial support/appreciation for streamer or channel. |
| Bonding | Payment used to express care, affiliation, closeness, or relational solidarity. |
| Influence attempts | Payment used to request, steer, or negotiate stream behavior/goals/challenges. |
| Ritual participation | Payment used as part of recurring community ritual (milestones, traditions, repeated in-jokes). |
| Joke amplification | Payment used to heighten humor bits, banter, or playful escalation. |
| Attention-seeking | Payment used to increase visibility and secure explicit acknowledgment. |

## Relationship Model Labels

| Label | Definition |
|---|---|
| Performative hosting | Streamer-centered host model with structured audience participation and ritual acknowledgment. |
| Peer-like collaboration | Shared-activity model where streamer and chat co-construct moment-to-moment direction. |
| Performative intimacy | Host uses warmth/self-disclosure/affection language to create closeness at scale. |
| Other/unclear | Relationship framing present but not cleanly classifiable to the main categories. |
| Dominant relationship model phrasing | Most frequent relationship-model wording across summaries in a corpus. |

## Content/Structure Labels

| Label | Definition |
|---|---|
| Content pillar (title-tag cluster) | Recurring stream theme inferred from repeated bracketed tags in filenames (for example, a game, format, or series). |
| Serialized arc | Multi-part continuity content where progress carries across episodes. |
| Eventized stream | Stream designed around a milestone/event/hype peak rather than baseline progression. |

## Method Labels

| Label | Definition |
|---|---|
| Corpus reviewed | Total number of summary files included in a report. |
| Unit of analysis | Single `*_summary.md` document treated as one analyzed stream record. |
| Normalized | Labels harmonized across template variants/capitalization differences before counting. |
| Unknown | Used when a label cannot be assigned from available summary text without over-inference. |
| Visible in summaries | Means present in summary text; does not guarantee full platform-complete logs. |
