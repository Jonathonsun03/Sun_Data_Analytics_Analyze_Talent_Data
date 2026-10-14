# Load into the caller's environment so scoped users and test doubles remain local.
for (.nlp_inference_module in c(
  "config.R", "transport.R", "api.R", "locks.R", "machine.R",
  "cleanup.R", "lifecycle.R", "punctuation.R"
)) {
  source(here::here("r_scripts", "lib", "nlp_inference", .nlp_inference_module), local = TRUE)
}
rm(.nlp_inference_module)
