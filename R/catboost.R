#' @export
train_catboost_class <- function(...) {
  train_catboost(..., mode = "classification")
}

#' @export
train_catboost_reg <- function(...) {
  train_catboost(..., mode = "regression")
}

#' @export
train_catboost <- function(..., mode) {
  args <- list(...)

  args$rsm <- args$rsm/ncol(args$x)

  d <- catboost::catboost.load_pool(data = args$x, label = args$y)

  args$x <- NULL
  args$y <- NULL

  mod <- catboost::catboost.train(learn_pool = d, params = args)
  class(mod) <- c("catboost")
  mod
}









