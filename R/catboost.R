#' Wrapper to add `catboost` engine to the parsnip `boost_tree` model
#' specification
#'
#' @return NULL
#' @export
add_boost_tree_catboost <- function() {

  parsnip::set_model_engine("boost_tree", mode = "regression", eng = "catboost")
  parsnip::set_model_engine("boost_tree", mode = "classification", eng = "catboost")
  parsnip::set_dependency("boost_tree", eng = "catboost", pkg = "catboost")

  parsnip::set_fit(
    model = "boost_tree",
    eng = "catboost",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "treesnip", fun = "train_catboost_reg"),
      defaults = list()
    )
  )

  parsnip::set_fit(
    model = "boost_tree",
    eng = "catboost",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "treesnip", fun = "train_catboost_class"),
      defaults = list()
    )
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "catboost",
    parsnip = "tree_depth",
    original = "max_depth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "catboost",
    parsnip = "trees",
    original = "iterations",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "catboost",
    parsnip = "learn_rate",
    original = "eta",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "catboost",
    parsnip = "mtry",
    original = "rsm",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "catboost",
    parsnip = "min_n",
    original = "min_child_weight",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "catboost",
    parsnip = "loss_reduction",
    original = "gamma",
    func = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "catboost",
    parsnip = "sample_size",
    original = "subsample",
    func = list(pkg = "dials", fun = "sample_size"),
    has_submodel = FALSE
  )


}

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









