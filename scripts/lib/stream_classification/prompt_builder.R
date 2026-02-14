library(jsonlite)

load_prompt_bundle <- function(talent_name, classification_root = "classification") {
  config_path <- file.path(classification_root, "config", "talent_profiles.json")
  cfg <- load_talent_profiles(config_path = config_path)
  selected <- select_talent_profile(
    talent_name = talent_name,
    cfg = cfg,
    classification_root = classification_root
  )
  profile_name <- selected$profile_name
  profile <- selected$profile

  system_path <- resolve_classification_path(classification_root, cfg$paths$system_prompt)
  user_template_path <- resolve_classification_path(classification_root, cfg$paths$user_template)
  schema_path <- resolve_classification_path(classification_root, cfg$paths$schema)
  overlay_path <- resolve_classification_path(classification_root, profile$overlay)

  required_paths <- c(system_path, user_template_path, schema_path, overlay_path)
  missing_paths <- required_paths[!file.exists(required_paths)]
  if (length(missing_paths) > 0) {
    stop("Missing prompt assets: ", paste(missing_paths, collapse = ", "))
  }

  list(
    taxonomy_version = cfg$taxonomy_version,
    prompt_version = cfg$prompt_version,
    profile_name = profile_name,
    system_prompt = read_text_file(system_path),
    user_prompt_template = read_text_file(user_template_path),
    talent_rules_text = read_text_file(overlay_path),
    schema_path = schema_path,
    schema_text = read_text_file(schema_path),
    schema = jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
  )
}
