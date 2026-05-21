# Internal ENA Precoding Tools

These scripts are implementation details for:

```bash
bin/linux/ena_precoding/run_qualitative_coding_batch.sh
```

Use the top-level runner for normal qualitative-coding work. It prepares the
CSV inputs, compiles the codebook JSON, runs coding, and organizes the run
folder under `Processed/Talent_Data/qualitative_batch_runs`.

The internal scripts remain available for debugging, legacy Batch API payload
experiments, sync tests, export/retry utilities, and low-level preparation.
