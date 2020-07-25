#' Retrieve the indices of categorical (factor) columns
#'
#' Utility function to help identify factors in data.frame.
#' Does only identify the columns, nothing else.
#' @noRd
categorical_columns <- function(x){
  categorical_cols <- NULL
  for (i in seq_along(x)) {
    if (is.factor(x[[i]])) {
      categorical_cols <- c(categorical_cols, i)
    }
  }
  categorical_cols
}

#' Replace categorical features with integers
#'
#' Utility function to replace categorical features with integer
#' representation.
#' @noRd
categorical_features_to_int <- function(x, cat_indices){
  for (i in cat_indices){
    x[[i]] <- as.integer(x[[i]]) -1
  }
  x
}
