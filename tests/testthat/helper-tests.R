expect_error_free <- function(...) {
  testthat::expect_error(..., regexp = NA)
}

