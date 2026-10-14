# Isolated checkpoint and deadline tests; no production API or lakehouse access.
source("r_scripts/lib/subtitle_backfill/subtitle_backfill_runtime.R")
source("r_scripts/lib/subtitle_backfill/subtitle_backfill_database.R")
source("r_scripts/lib/subtitle_backfill/subtitle_backfill_checkpoints.R")
source("r_scripts/lib/subtitle_backfill/subtitle_backfill_speakers.R")
source("r_scripts/lib/subtitle_backfill/subtitle_speaker_checkpoints.R")

local({
  root <- tempfile("subtitle-resume-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE); subtitle_backfill_start_budget(0) })
  path <- file.path(root, "speaker.rds")
  sentences <- data.frame(
    text = c("A", "B", "C", "D"),
    speaker_turn_id = rep(1L, 4), speaker_change = rep(FALSE, 4)
  )
  identity <- list(source = "checksum", model = "model", prompt = "prompt", sentences = sentences)
  calls <- 0L
  classifier <- function(...) {
    calls <<- calls + 1L
    "same_speaker"
  }
  # Reassigning the caller's function must retain the original classifier.
  wrapped <- function(...) "uncertain"
  wrapped <- subtitle_speaker_cached_classifier(wrapped, file.path(root, "wrapped.rds"), identity)
  stopifnot(identical(wrapped("", "A", "B", "C"), "uncertain"))
  stopped <- tryCatch({
    infer_subtitle_speaker_turns(
      sentences, subtitle_speaker_cached_classifier(classifier, path, identity),
      stop_requested_fn = function() calls == 2L
    )
    FALSE
  }, subtitle_backfill_stop_requested = function(error) TRUE)
  stopifnot(stopped, calls == 2L, length(readRDS(path)$results) == 2L)
  resumed <- infer_subtitle_speaker_turns(
    sentences, subtitle_speaker_cached_classifier(classifier, path, identity)
  )
  stopifnot(calls == 3L, identical(resumed$inferred_speaker_turn_id, rep(1L, 4)))
  # Changing source/model/prompt/text invalidates the saved comparisons.
  for (field in names(identity)) {
    changed <- identity
    changed[[field]] <- paste("changed", field)
    cached <- subtitle_speaker_cached_classifier(classifier, path, changed)
    before <- calls
    cached("", "A", "B", "C")
    stopifnot(calls == before + 1L)
  }
  cached <- subtitle_speaker_cached_classifier(classifier, path, identity, force = TRUE)
  before <- calls
  cached("", "A", "B", "C")
  stopifnot(calls == before + 1L)
  # An unsuccessful call must not become a reusable checkpoint.
  before <- readRDS(path)
  failed <- tryCatch({
    cached <- subtitle_speaker_cached_classifier(function(...) stop("API failure"), path, identity)
    cached("new", "request", "fails", "")
    FALSE
  }, error = function(error) TRUE)
  stopifnot(failed, identical(before, readRDS(path)))

  subtitle_backfill_start_budget(14400)
  remaining <- subtitle_backfill_budget$deadline - proc.time()[["elapsed"]]
  stopifnot(remaining > 14399, remaining <= 14400)
  attempts <- 0L
  deadline_stop <- tryCatch({
    subtitle_speaker_with_retries(
      function() {
        attempts <<- attempts + 1L
        subtitle_backfill_budget$deadline <- 0
        stop(subtitle_speaker_transient_error("temporary failure"))
      },
      sleep_fn = function(...) stop("Should stop before sleeping"),
      stop_requested_fn = function() subtitle_backfill_stop_requested(file.path(root, "stop"))
    )
    FALSE
  }, subtitle_backfill_stop_requested = function(error) {
    grepl("runtime budget", conditionMessage(error))
  })
  stopifnot(deadline_stop, attempts == 1L)

  # A waiting reader honors the same deadline without opening DuckDB.
  isolated <- new.env(parent = environment())
  sys.source("r_scripts/lib/subtitle_backfill/subtitle_backfill_access.R", isolated)
  connects <- 0L
  isolated$duckdb_connect <- function(...) {
    connects <<- connects + 1L
    stop("Conflicting lock")
  }
  isolated$subtitle_backfill_budget$deadline <- 0
  stopped <- tryCatch({
    isolated$subtitle_backfill_with_connection(file.path(root, "db"), identity, TRUE)
    FALSE
  }, subtitle_backfill_stop_requested = function(error) TRUE)
  stopifnot(stopped, connects == 0L)
  isolated$subtitle_backfill_start_budget(0)
  failed <- tryCatch({
    isolated$subtitle_backfill_with_connection(
      file.path(root, "db"), identity, TRUE, connection_attempts = 3L, retry_delay_sec = 0
    )
    FALSE
  }, error = function(error) TRUE)
  stopifnot(failed, connects == 3L)
  # Terminal bookkeeping waits for collection but cannot wait indefinitely.
  marker <- file.path(root, "collection-active")
  file.create(marker)
  failed <- tryCatch({
    subtitle_backfill_access_wait(marker, file.path(root, "stop"), FALSE, 0)
    FALSE
  }, error = function(error) TRUE)
  stopifnot(failed)
})
cat("Subtitle resume and runtime-budget tests passed\n")
