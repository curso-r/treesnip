#' Simple wraper around `tree::tree`
#'
#' Simple wraper aroound `tree::tree` that adds a simple
#' class and sets the `mincut` argument to 0.
#'
#' @param ... arguments passed directly to `[tree::tree()]`.
#'
#' @export
train_tree <- function(...) {
  mod <- tree::tree(..., mincut = 0)
  class(mod) <- c("treesnip", "tree")
  mod
}

#' @export
print.treesnip <- tree:::print.tree


# tree engine definitions ---------------------------------------

.onLoad <- function(libname, pkgname){

  object <- NULL
  new_data <- NULL

  parsnip::set_model_engine("decision_tree", mode = "regression", eng = "tree")

  parsnip::set_dependency("decision_tree", eng = "tree", pkg = "tree")

  parsnip::set_fit(
    model = "decision_tree",
    eng = "tree",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "treesnip", fun = "train_tree"),
      defaults = list()
    )
  )

  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "tree",
    parsnip = "min_n",
    original = "minsize",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "tree",
    parsnip = "cost_complexity",
    original = "mindev",
    func = list(pkg = "dials", fun = "cost_complexity"),
    has_submodel = FALSE
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "tree",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data)
        )
    )
  )


  # -------------- catboost


  parsnip::set_model_engine("boost_tree", mode = "regression", eng = "catboost")

  parsnip::set_dependency("boost_tree", eng = "catboost", pkg = "catboost")

  parsnip::set_fit(
    model = "boost_tree",
    eng = "catboost",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "treesnip", fun = "train_catboost"),
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




