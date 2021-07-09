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
      interface = "data.frame",
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
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
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
      interface = "data.frame",
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
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
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

  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    type = "raw",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = NULL,
      func = c(pkg = "treesnip", fun = "predict_lightgbm_classification_raw"),
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
    has_submodel = TRUE
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
    func = list(pkg = "dials", fun = "sample_prop"),
    has_submodel = FALSE
  )
}

prepare_df_lgbm <- function(x, y = NULL) {
  categorical_cols <- categorical_columns(x)
  x <- categorical_features_to_int(x, categorical_cols)
  x <- as.matrix(x)
  return(x)
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
train_lightgbm <- function(x, y, max_depth = 17, num_iterations = 10, learning_rate = 0.1,
                           feature_fraction = 1, min_data_in_leaf = 20, min_gain_to_split = 0, bagging_fraction = 1, ...) {

  force(x)
  force(y)
  others <- list(...)

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
  if (!any(names(others) %in% c("objective"))) {
    if (is.numeric(y)) {
      others$num_class <- 1
      others$objective <- "regression"
    } else {
      lvl <- levels(y)
      lvls <- length(lvl)
      y <- as.numeric(y) - 1
      if (lvls == 2) {
        others$num_class <- 1
        others$objective <- "binary"
      } else {
        others$num_class <- lvls
        others$objective <- "multiclass"
      }
    }
  }

  arg_list <- list(
    num_iterations = num_iterations,
    learning_rate = learning_rate,
    max_depth = max_depth,
    feature_fraction = feature_fraction,
    min_data_in_leaf = min_data_in_leaf,
    min_gain_to_split = min_gain_to_split,
    bagging_fraction = bagging_fraction
  )

  # override or add some other args
  others <- others[!(names(others) %in% c("data", names(arg_list)))]

  # parallelism should be explicitly specified by the user
  if(all(sapply(others[c("num_threads", "num_thread", "nthread", "nthreads", "n_jobs")], is.null))) others$num_threads <- 1L

  if(max_depth > 17) {
    warning("max_depth > 17, num_leaves truncated to 2^17 - 1")
    max_depth <- 17
  }

  if(is.null(others$num_leaves)) {
    others$num_leaves = max(2^max_depth - 1, 2)
  }

  arg_list <- purrr::compact(c(arg_list, others))


  # train ------------------------
  d <- lightgbm::lgb.Dataset(
    data = prepare_df_lgbm(x),
    label = y,
    categorical_feature = categorical_columns(x),
    feature_pre_filter = FALSE
  )

  main_args <- list(
    data = quote(d),
    params = arg_list
  )

  call <- parsnip::make_call(fun = "lgb.train", ns = "lightgbm", main_args)
  rlang::eval_tidy(call, env = rlang::current_env())
}

#' predict_lightgbm_classification_prob
#'
#' Not intended for direct use.
#'
#' @param object a fitted object.
#'
#' @param new_data data frame in which to look for variables with which to predict.
#' @param ... Additional named arguments passed to the predict() method of the lgb.Booster object passed to object.
#'
#' @export
predict_lightgbm_classification_prob <- function(object, new_data, ...) {
  p <- stats::predict(object$fit, prepare_df_lgbm(new_data), reshape = TRUE, ...)
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
#' @param ... Additional named arguments passed to the predict() method of the lgb.Booster object passed to object.
#'
#' @export
predict_lightgbm_classification_class <- function(object, new_data, ...) {
  p <- predict_lightgbm_classification_prob(object, prepare_df_lgbm(new_data), ...)
  q <- apply(p, 1, function(x) which.max(x))
  names(p)[q]
}

#' predict_lightgbm_classification_raw
#'
#' Not intended for direct use.
#'
#' @param object a fitted object.
#'
#' @param new_data data frame in which to look for variables with which to predict.
#' @param ... Additional named arguments passed to the predict() method of the lgb.Booster object passed to object.
#'
#' @export
predict_lightgbm_classification_raw <- function(object, new_data, ...) {
  stats::predict(object$fit, prepare_df_lgbm(new_data), reshape = TRUE, rawscore = TRUE, ...)
}

#' predict_lightgbm_regression_numeric
#'
#' Not intended for direct use.
#'
#' @param object a fitted object.
#'
#' @param new_data data frame in which to look for variables with which to predict.
#' @param ... Additional named arguments passed to the predict() method of the lgb.Booster object passed to object.
#'
#' @export
predict_lightgbm_regression_numeric <- function(object, new_data, ...) {
  # train_colnames <- object$fit$.__enclos_env__$private$train_set$get_colnames()
  p <- stats::predict(object$fit, prepare_df_lgbm(new_data), reshape = TRUE, predict_disable_shape_check=TRUE, ...)
  p
}



#' Model predictions across many sub-models
#'
#' For some models, predictions can be made on sub-models in the model object.
#'
#' @param object A model_fit object.
#' @param ... Optional arguments to pass to predict.model_fit(type = "raw") such as type.
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or NULL. Possible values are "numeric", "class", "prob", "conf_int", "pred_int", "quantile", or "raw". When NULL, predict() will choose an appropriate value based on the model's mode.
#' @param trees An integer vector for the number of trees in the ensemble.
#'
#' @export
#' @importFrom purrr map_df
#' @importFrom parsnip multi_predict
multi_predict._lgb.Booster <- function(object, new_data, type = NULL, trees = NULL, ...) {
  if (any(names(rlang::enquos(...)) == "newdata")) {
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
  }

  trees <- sort(trees)

  res <- map_df(trees, lightgbm_by_tree, object = object, new_data = new_data, type = type)
  res <- dplyr::arrange(res, .row, trees)
  res <- split(res[, -1], res$.row)
  names(res) <- NULL

  tibble::tibble(.pred = res)

}

lightgbm_by_tree <- function(tree, object, new_data, type = NULL) {

  # switch based on prediction type
  if (object$spec$mode == "regression") {
    pred <- predict_lightgbm_regression_numeric(object, new_data, num_iteration = tree)
    pred <- tibble::tibble(.pred = pred)
    nms <- names(pred)
  } else {
    if (type == "class") {
      pred <- predict_lightgbm_classification_class(object, new_data, num_iteration = tree)
      pred <- tibble::tibble(.pred_class = factor(pred, levels = object$lvl))
    } else {
      pred <- predict_lightgbm_classification_prob(object, new_data, num_iteration = tree)
      names(pred) <- paste0(".pred_", names(pred))
    }
    nms <- names(pred)
  }
  pred[["trees"]] <- tree
  pred[[".row"]] <- 1:nrow(new_data)
  pred[, c(".row", "trees", nms)]
}




