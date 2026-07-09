detect_decimals <- function(df, tol = 1e-12) {
  if (is.null(df)) {
    return(character())
  }

  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]

  if (!length(numeric_cols)) {
    return(character())
  }

  decimal_cols <- numeric_cols[
    vapply(df[numeric_cols], function(x) {
      x <- x[!is.na(x)]
      if (!length(x)) return(FALSE)
      any(abs(x - round(x)) > tol)
    }, logical(1))
  ]

  decimal_cols
}
