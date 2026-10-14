# Utilities

- This directory is for small, domain-independent helpers used across subsystems.
- Do not add API clients, machine management, pipeline orchestration, or domain-specific collections of functions here. Use a named library under `r_scripts/lib/`.
- NLP API and machine lifecycle work belongs in `../nlp_inference/`; read its README and AGENTS.md first.
- Keep compatibility loaders limited to sourcing the maintained library. Do not grow new logic in `inference_machine.R`.
- Follow the repository's module/function size review thresholds and extract cohesive responsibilities rather than expanding catch-all utilities.
