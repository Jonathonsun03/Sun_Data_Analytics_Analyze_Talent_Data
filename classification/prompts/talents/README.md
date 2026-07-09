# Talent Prompt Organization

Each talent folder can now hold all prompt-related assets in one place:

- `overlay.txt`: talent overlay rules
- `definitions/*.txt` (optional): talent-specific definition overrides/additions
- `content_type_rules.txt` (optional): talent-specific content-type gating override
- `full_prompt/*.txt`: compiled prompt dumps written at classification runtime

Resolution order:
1. Base definitions from `classification/prompts/definitions/*.txt`
2. Talent-specific `definitions/*.txt` (override by PRIMARY CODE field name)
3. Talent-specific `content_type_rules.txt` if present, else base content type rules
