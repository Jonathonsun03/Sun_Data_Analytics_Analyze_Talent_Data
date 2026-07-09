if (!exists("ena_precoding_codebook_path", mode = "function")) {
  source(file.path("r_scripts", "lib", "ENA_prep", "shared", "ena_precoding_paths.R"))
}
if (!exists("talent_slugify", mode = "function")) {
  ena_precoding_source_repo_file("r_scripts", "lib", "utils", "staging_root.R")
  ena_precoding_source_repo_file("r_scripts", "lib", "utils", "talent_select.R")
}

ena_precoding_squish <- function(x) {
  x <- enc2utf8(as.character(x))
  x[is.na(x)] <- ""
  gsub("\\s+", " ", trimws(x), perl = TRUE)
}

ena_precoding_make_code_column <- function(code_id) {
  out <- tolower(gsub("[^A-Za-z0-9]+", "_", code_id))
  out <- gsub("^_+|_+$", "", out)
  out <- paste0("code_", out)
  make.names(out, unique = FALSE)
}

ena_precoding_normalize_codebook <- function(codebook_path = NULL, talent_data_root = NULL) {
  path <- ena_precoding_codebook_path(codebook_path, talent_data_root)
  if (!file.exists(path)) {
    stop("Qualitative codebook not found: ", path)
  }
  raw <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  names(raw) <- trimws(names(raw))
  required <- c(
    "Primary Code ID", "Primary Code", "Secondary Code ID", "Secondary Code",
    "Definition", "Date added", "Examples from text"
  )
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0L) {
    stop("Qualitative codebook is missing required columns: ", paste(missing, collapse = ", "))
  }

  primary_id <- ena_precoding_squish(raw[["Primary Code ID"]])
  primary_code <- ena_precoding_squish(raw[["Primary Code"]])
  secondary_id <- ena_precoding_squish(raw[["Secondary Code ID"]])
  secondary_code <- ena_precoding_squish(raw[["Secondary Code"]])
  definition <- ena_precoding_squish(raw[["Definition"]])

  code_stub <- ifelse(
    nzchar(secondary_id),
    paste("personality", primary_id, secondary_id, secondary_code, sep = "_"),
    paste("personality", primary_id, primary_code, sep = "_")
  )
  code_id <- tolower(gsub("[^A-Za-z0-9]+", "_", code_stub))
  code_id <- gsub("^_+|_+$", "", code_id)

  out <- data.frame(
    code_id = code_id,
    code_column = ena_precoding_make_code_column(code_id),
    primary_code_id = primary_id,
    primary_code = primary_code,
    secondary_code_id = secondary_id,
    secondary_code = secondary_code,
    definition = definition,
    examples_from_text = ena_precoding_squish(raw[["Examples from text"]]),
    date_added = ena_precoding_squish(raw[["Date added"]]),
    stringsAsFactors = FALSE
  )
  out
}

ena_precoding_codebook_diagnostics <- function(codebook) {
  if (!is.data.frame(codebook) || nrow(codebook) == 0L) {
    return(data.frame(
      row_number = integer(),
      field = character(),
      status = character(),
      value = character(),
      detail = character(),
      stringsAsFactors = FALSE
    ))
  }
  diagnostics <- list()
  add_diag <- function(rows, field, status, value, detail) {
    if (length(rows) == 0L) return(invisible(NULL))
    diagnostics[[length(diagnostics) + 1L]] <<- data.frame(
      row_number = rows,
      field = field,
      status = status,
      value = value,
      detail = detail,
      stringsAsFactors = FALSE
    )
  }
  add_diag(which(!nzchar(codebook$code_id)), "code_id", "error", "", "missing code_id")
  add_diag(which(!nzchar(codebook$code_column)), "code_column", "error", "", "missing code_column")
  dup_id <- which(duplicated(codebook$code_id) | duplicated(codebook$code_id, fromLast = TRUE))
  if (length(dup_id) > 0L) {
    add_diag(dup_id, "code_id", "warning", codebook$code_id[dup_id], "duplicate code_id")
  }
  dup_col <- which(duplicated(codebook$code_column) | duplicated(codebook$code_column, fromLast = TRUE))
  if (length(dup_col) > 0L) {
    add_diag(dup_col, "code_column", "warning", codebook$code_column[dup_col], "duplicate code_column")
  }
  if (length(diagnostics) == 0L) {
    return(data.frame(
      row_number = NA_integer_,
      field = "codebook",
      status = "ok",
      value = "",
      detail = "no duplicate or missing code IDs",
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, diagnostics)
}
