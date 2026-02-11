library(here)
library(purrr)
library(jsonlite)

here::i_am("scripts/lib/ChatGPT/examples/basic_flow.R")

list.files(
  here("scripts", "lib", "ChatGPT"),
  recursive = TRUE,
  full.names = TRUE,
  pattern = "\\.R$"
) %>%
  walk(source)

system_prompt <- chatgpt_load_prompt("system_default")
user_prompt <- chatgpt_render_prompt(
  chatgpt_load_prompt("user_default"),
  list(input_text = "The quick brown fox jumped over the lazy dog.")
)

messages <- chatgpt_make_messages(
  system_prompt = system_prompt,
  user_prompt = user_prompt
)

response <- chatgpt_send_chat(
  messages = messages,
  model = "YOUR_MODEL_HERE",
  temperature = 0.2
)

cat(chatgpt_extract_text(response))
