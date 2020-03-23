train_tree <- function(...) {
  mod <- tree::tree(..., mincut = 0)
  class(mod) <- c("treesnip", "tree")
  mod
}

#' @export
print.treesnip <- tree:::print.tree


# tree engine definitions ---------------------------------------

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


