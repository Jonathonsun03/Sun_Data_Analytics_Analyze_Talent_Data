source(file.path("r_scripts", "lib", "dashboard", "auth", "access.R"))

assert_true <- function(x, message) {
  if (!isTRUE(x)) {
    stop(message, call. = FALSE)
  }
}

assert_equal <- function(x, y, message) {
  if (!identical(x, y)) {
    stop(message, call. = FALSE)
  }
}

request <- list(
  HTTP_X_SDA_VERIFIED_EMAIL = "client@example.com",
  HTTP_X_SDA_ALLOWED_TALENT_CODES = "LEI3, AVA1,LEI3"
)
context <- dashboard_access_context(request, c("AVA1", "KAT2", "LEI3"))

assert_true(context$authorized, "A verified user with an allowed code should be authorized.")
assert_equal(
  context$allowed_talent_codes,
  c("LEI3", "AVA1"),
  "Talent codes should be trimmed, deduplicated, and intersected exactly."
)
assert_true(
  dashboard_talent_is_authorized("LEI3", context),
  "An allowed code should pass the server-side authorization check."
)
assert_true(
  !dashboard_talent_is_authorized("KAT2", context),
  "A browser-supplied but unassigned code should fail authorization."
)

missing_context <- dashboard_access_context(list(), c("AVA1"))
assert_true(
  !missing_context$authorized,
  "Production authorization must fail closed when trusted headers are absent."
)

invalid_request <- list(
  HTTP_X_SDA_VERIFIED_EMAIL = "client@example.com",
  HTTP_X_SDA_ALLOWED_TALENT_CODES = "AVA1,not valid"
)
assert_true(
  inherits(
    try(dashboard_access_context(invalid_request, c("AVA1")), silent = TRUE),
    "try-error"
  ),
  "Malformed talent codes must fail closed."
)

cat("dashboard access tests passed\n")
