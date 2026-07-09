import_modules <- function(path, recursive = TRUE) {
  list.files(
    here::here(path),
    full.names = TRUE,
    pattern = "\\.R$",
    recursive = recursive
  ) %>%
    purrr::walk(source)
}
