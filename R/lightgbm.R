#' Wrapper to add `lightgbm` engine to the parsnip `boost_tree` model
#' specification
#'
#' @return NULL
#' @export
add_boost_tree_lightgbm <- function() {
  parsnip::set_model_engine("boost_tree", mode = "regression", eng = "lightgbm")
  parsnip::set_model_engine("boost_tree", mode = "classification", eng = "lightgbm")
  parsnip::set_dependency("boost_tree", eng = "lightgbm", pkg = "lightgbm")

  parsnip::set_fit(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(pkg = "treesnip", fun = "train_lightgbm"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "boost_tree",
    mode = "regression",
    eng = "lightgbm",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "treesnip", fun = "predict_lightgbm_regression_numeric"),
      args = list(
        object = quote(object),
        new_data = quote(new_data)
      )
    )
  )

  parsnip::set_fit(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(pkg = "treesnip", fun = "train_lightgbm"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "boost_tree",
    mode = "classification",
    eng = "lightgbm",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    type = "class",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = NULL,
      func = c(pkg = "treesnip", fun = "predict_lightgbm_classification_class"),
      object = quote(object),
      new_data = quote(new_data)
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    type = "prob",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = NULL,
      func = c(pkg = "treesnip", fun = "predict_lightgbm_classification_prob"),
      object = quote(object),
      new_data = quote(new_data)
    )
  )

  # model args ----------------------------------------------------
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "tree_depth",
    original = "max_depth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "trees",
    original = "num_iterations",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "learn_rate",
    original = "learning_rate",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "mtry",
    original = "feature_fraction",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "min_n",
    original = "min_data_in_leaf",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "loss_reduction",
    original = "min_gain_to_split",
    func = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "sample_size",
    original = "bagging_fraction",
    func = list(pkg = "dials", fun = "sample_size"),
    has_submodel = FALSE
  )
}

#' Boosted trees via lightgbm
#'
#' `lightgbm_train` is a wrapper for `lightgbm` tree-based models
#'  where all of the model arguments are in the main function.
#'
#' @param x A data frame or matrix of predictors
#' @param y A vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param max_depth An integer for the maximum depth of the tree.
#' @param num_iterations An integer for the number of boosting iterations.
#' @param learning_rate A numeric value between zero and one to control the learning rate.
#' @param feature_fraction Subsampling proportion of columns.
#' @param min_data_in_leaf A numeric value for the minimum sum of instances needed
#'  in a child to continue to split.
#' @param min_gain_to_split A number for the minimum loss reduction required to make a
#'  further partition on a leaf node of the tree.
#' @param bagging_fraction Subsampling proportion of rows.
#' @param ... Other options to pass to `lightgbm.train`.
#' @return A fitted `lightgbm.Model` object.
#' @keywords internal
#' @export
train_lightgbm <- function(x, y, max_depth = 6, num_iterations = 100, learning_rate = 0.1,
                           feature_fraction = 1, min_data_in_leaf = 1, min_gain_to_split = 0, bagging_fraction = 1, ...) {

  force(x)
  force(y)

  # feature_fraction ------------------------------
  if(!is.null(feature_fraction)) {
    feature_fraction <- feature_fraction/ncol(x)
  }
  if(feature_fraction > 1) {
    feature_fraction <- 1
  }

  # subsample -----------------------
  if (bagging_fraction > 1) {
    bagging_fraction <- 1
  }

  # loss and num_class -------------------------
  if (is.numeric(y)) {
    num_class <- 1
    objective <- "regression"
  } else {
    lvl <- levels(y)
    lvls <- length(lvl)
    y <- as.numeric(y) - 1
    if (lvls == 2) {
      num_class <- 1
      objective <- "binary"
    } else {
      num_class <- lvls
      objective <- "multiclass"
    }
  }

  arg_list <- list(
    num_class = num_class,
    objective = objective,
    num_iterations = num_iterations,
    learning_rate = learning_rate,
    max_depth = max_depth,
    feature_fraction = feature_fraction,
    min_data_in_leaf = min_data_in_leaf,
    min_gain_to_split = min_gain_to_split,
    bagging_fraction = bagging_fraction
  )

  # override or add some other args
  others <- list(...)
  others <- others[!(names(others) %in% c("data", names(arg_list)))]

  if(is.null(others$num_leaves)) others$num_leaves = 2^max_depth - 1

  arg_list <- purrr::compact(c(arg_list, others))


  # train ------------------------
  d <- lightgbm::lgb.Dataset(data = x, label = y, feature_pre_filter = FALSE)

  main_args <- list(
    data = quote(d),
    params = arg_list
  )

  call <- parsnip:::make_call(fun = "lgb.train", ns = "lightgbm", main_args)
  rlang::eval_tidy(call, env = rlang::current_env())
}

#' predict_lightgbm_classification_prob
#'
#' Not intended for direct use.
#'
#' @param object a fitted object.
#'
#' @param new_data data frame in which to look for variables with which to predict.
#'
#' @export
predict_lightgbm_classification_prob <- function(object, new_data) {
  p <- stats::predict(object$fit, new_data, reshape = TRUE)
  if(is.vector(p)) {
    p <- tibble::tibble(p1 = 1 - p, p2 = p)
  }
  colnames(p) <- object$lvl
  tibble::as_tibble(p)
}

#' predict_lightgbm_classification_class
#'
#' Not intended for direct use.
#'
#' @param object a fitted object.
#'
#' @param new_data data frame in which to look for variables with which to predict.
#'
#' @export
predict_lightgbm_classification_class <- function(object, new_data) {
  p <- predict_lightgbm_classification_prob(object, new_data)
  q <- apply(p, 1, function(x) which.max(x))
  names(p)[q]
}


#' predict_lightgbm_regression_numeric
#'
#' Not intended for direct use.
#'
#' @param object a fitted object.
#'
#' @param new_data data frame in which to look for variables with which to predict.
#'
#' @export
predict_lightgbm_regression_numeric <- function(object, new_data) {
  p <- stats::predict(object$fit, new_data, reshape = TRUE)
  p
}







