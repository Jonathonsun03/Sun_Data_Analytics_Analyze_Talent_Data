---
title: Report Pricing Model
aliases:
  - Bundle Pricing
  - Report Pricing
tags:
  - product
  - pricing
  - reports
created: 2026-03-26
updated: 2026-03-26
status: active
---

# Report Pricing Model

## Current pricing decision

- Bundle A: `$35/month`
- Bundle B: `TBD`, but current recommendation is `$65/month`
- Possible A + B combined offer: `$85-$89/month`

## Why Bundle A at $35/month

- `$35/month` feels affordable for smaller creators and smaller clients.
- It is still comfortably above the estimated AI generation cost.
- It better reflects the value of the report system, prompt design, maintenance, and delivery than `$25/month`.
- It is still low enough to work as a recurring offer rather than a high-friction consulting sale.

## Estimated AI run cost

These estimates are based on current GPT-5.4 pricing assumptions discussed during pricing review:

- Input tokens: `$2.50 / 1M`
- Output tokens: `$15.00 / 1M`

Important note:

- The logs currently store total tokens used, not separate input and output token counts.
- Because of that, exact dollar cost cannot be calculated from the logs alone.
- The practical estimates below assume these report runs are input-heavy, which matches how the prompts are structured.

## Bundle A run cost

Historical full-run average from completed logs:

- Average total tokens per full Bundle A run: `342,497`
- Number of complete runs used for average: `6`

Estimated dollar cost per full Bundle A run:

- Practical estimate: `~$1.40`
- Reasonable estimate range: `$1.30-$1.50`
- Hard floor if all tokens were input: `~$0.86`
- Hard ceiling if all tokens were output: `~$5.14`

## Bundle B run cost

Historical full-run average from completed logs:

- Average total tokens per full Bundle B run: `474,928`
- Number of complete runs used for average: `2`

Estimated dollar cost per full Bundle B run:

- Practical estimate: `~$1.95`
- Reasonable estimate range: `$1.80-$2.10`
- Hard floor if all tokens were input: `~$1.19`
- Hard ceiling if all tokens were output: `~$7.12`

## Pricing rationale

### Bundle A

- Bundle A is the lower-friction entry product.
- It works well as a recurring monthly report.
- At `$35/month`, the price still feels accessible without making the product feel disposable or low-value.
- This price leaves room for maintenance, reruns, product improvement, support time, and future margin.

### Bundle B

- Bundle B is a deeper strategic and diagnostic product than Bundle A.
- It takes more prompts, more tokens, and more interpretation work.
- It is more likely to be perceived as decision-support rather than just a performance snapshot.
- Because of that, it is reasonable for Bundle B to be priced above Bundle A.

## Current Bundle B pricing recommendation

- Recommended default: `$65/month`
- Conservative lower option: `$55/month`
- Higher but still small-client-friendly option: `$75/month`

Why `$65/month` currently feels strongest:

- It clearly separates Bundle B from Bundle A.
- It better matches the added analytical depth.
- It is still far below large agency pricing.
- It leaves room for a discounted combined package.

## Alternative simple pricing model

If pricing simplicity matters more than value separation, an alternative model is:

- Bundle A: `$35/month`
- Bundle B: `$35/month`
- A + B together: `$60/month`

Why this could work:

- simpler to explain
- easier buying decision
- reduces buyer hesitation about choosing between bundles

Why it may be weaker:

- Bundle B is objectively heavier to produce
- it may underprice the more strategic report
- it reduces the perceived difference between products

## Recommended pricing position right now

Current preferred pricing structure:

- Bundle A: `$35/month`
- Bundle B: `$65/month`
- A + B together: `$85-$89/month`

## Positioning notes

- `$25/month` for Bundle A is viable only as a very low-friction intro offer.
- `$35/month` is a stronger small-creator price point.
- Pricing should reflect more than token cost.
- The product value comes from the system, insight structure, report quality, and continued upkeep, not just model spend.

## Future improvements

- Add automatic logging of estimated dollar cost per run alongside token logs.
- Track input and output tokens separately if possible.
- Revisit Bundle B pricing after more full-run history is available.
- Reassess whether A + B should be discounted more aggressively for conversion.
