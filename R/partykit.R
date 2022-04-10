add_decision_tree_partykit <- function() {
  parsnip::set_model_engine("decision_tree", mode = "regression", eng = "partykit")

  parsnip::set_dependency("decision_tree", eng = "partykit", pkg = "partykit")

  parsnip::set_encoding(
    model = "decision_tree",
    eng = "partykit",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "decision_tree",
    eng = "partykit",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "parsnip", fun = "ctree_train"),
      defaults = list()
    )
  )

  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "partykit",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "partykit",
    parsnip = "tree_depth",
    original = "maxdepth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "partykit",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          type = "response"
        )
    )
  )

  # ----------------------------------------------------------------------------

  parsnip::set_model_engine("decision_tree", mode = "classification", eng = "partykit")

  parsnip::set_dependency("decision_tree", eng = "partykit", pkg = "partykit")

  parsnip::set_encoding(
    model = "decision_tree",
    eng = "partykit",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "decision_tree",
    eng = "partykit",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "parsnip", fun = "ctree_train"),
      defaults = list()
    )
  )

  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "partykit",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "partykit",
    parsnip = "tree_depth",
    original = "maxdepth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "partykit",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          type = "response"
        )
    )
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "partykit",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(result, object) tibble::as_tibble(result),
      func = c(fun = "predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          type = "prob"
        )
    )
  )

}

add_rand_forest_partykit <- function() {
  parsnip::set_model_engine("rand_forest", mode = "regression", eng = "partykit")

  parsnip::set_dependency("rand_forest", eng = "partykit", pkg = "partykit")

  parsnip::set_encoding(
    model = "rand_forest",
    eng = "partykit",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "rand_forest",
    eng = "partykit",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "parsnip", fun = "cforest_train"),
      defaults = list()
    )
  )

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "partykit",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "partykit",
    parsnip = "tree_depth",
    original = "maxdepth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )


  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "partykit",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "partykit",
    parsnip = "trees",
    original = "ntree",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "partykit",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          type = "response"
        )
    )
  )

  # ----------------------------------------------------------------------------

  parsnip::set_model_engine("rand_forest", mode = "classification", eng = "partykit")

  parsnip::set_dependency("rand_forest", eng = "partykit", pkg = "partykit")

  parsnip::set_encoding(
    model = "rand_forest",
    eng = "partykit",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "rand_forest",
    eng = "partykit",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "parsnip", fun = "cforest_train"),
      defaults = list()
    )
  )

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "partykit",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "partykit",
    parsnip = "tree_depth",
    original = "maxdepth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "partykit",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          type = "response"
        )
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "partykit",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(result, object) tibble::as_tibble(result),
      func = c(fun = "predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          type = "prob"
        )
    )
  )

}


