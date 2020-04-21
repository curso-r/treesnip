

# devtools::install_url("https://github.com/catboost/catboost/releases/download/v0.22/catboost-R-Darwin-0.22.tgz",
#                       INSTALL_opts = c("--no-multiarch"))

#' @export
train_catboost <- function(...) {
  args <- list(...)

  args$rsm <- args$rsm/ncol(args$x)

  d <- catboost::catboost.load_pool(data = args$x, label = args$y)

  args$x <- NULL
  args$y <- NULL

  mod <- catboost::catboost.train(learn_pool = d, params = args)
  class(mod) <- c("catboost")
  mod
}









