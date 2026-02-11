ChatGPT R Module (Skeleton)

Structure
- auth/ handles .env loading and API key lookup
- prompts/ loads prompt text files
- models/ resolves model selection
- data/ builds message payloads
- send_packet/ sends requests
- utils/ common helpers
- examples/ runnable usage

Typical flow
1. Ensure repo root has a `.env` with `OPENAI_API_KEY=...`.
2. Source all R files in `scripts/lib/ChatGPT`.
3. Load prompts from `scripts/lib/ChatGPT/prompts/text`.
4. Build messages and call `chatgpt_send_chat`.

Prompt templates
- Use `{{placeholder}}` in prompt files.
- Use `chatgpt_render_prompt(prompt, list(placeholder = "value"))` to substitute.

Section-based prompts (writer-friendly)
- Store per-section content under `Templates/<TemplateName>/sections/<NN_section_name>/`.
- Expected files per section: `prompt.txt`, `input.md`, `output.md`.
- Helpers: `chatgpt_load_section_prompt()`, `chatgpt_read_section_input()`, `chatgpt_write_section_output()`.
 - If `prompt.txt` contains `---SYSTEM---` and `---USER---`, use `chatgpt_load_section_prompt_parts()` to split them.
 - For report outputs, use `chatgpt_write_report_output()` to write to `Reports/<TemplateName>/sections/<SectionName>/output.md`.

Consistency pass (cross-section editing)
- Shared prompts live under `Templates/Global/sections/`:
  - `consistency_edit/prompt.txt`
  - `consistency_summary/prompt.txt`
  - `introduction/prompt.txt`
  - `conclusion/prompt.txt`
- New helpers in `utils/consistency_pass.R`:
  - `chatgpt_list_report_sections(template, ...)` lists numbered `Reports/<Template>/sections/<NN_...>` in order.
  - `chatgpt_read_report_output(template, section, ...)` reads `Reports/<Template>/sections/<Section>/output.md`.
  - `chatgpt_run_consistency_pass(...)` runs an edit pass and writes to `Reports/<Template>/<sections_dir>/`.
    - Default output dir is `sections_v2` to preserve originals.
    - Creates `0_introduction` and `5_conclusion` using Global prompts.
