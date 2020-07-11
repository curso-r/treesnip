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
      remove_intercept = FALSE
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
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        data = quote(model.matrix(~., data = new_data)),
        reshape = TRUE,
        rawscore = TRUE
      )
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        data = quote(as.matrix(new_data)),
        reshape = TRUE,
        rawscore = TRUE
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

  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = function(x, object) {
        if (is.vector(x)) {
          x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
        } else {
          x <- object$lvl[apply(x, 1, which.max)]
        }
        x
      },
      func = c(pkg = NULL, fun = "predict"),
      args = list(object = quote(object$fit), new_data = quote(new_data))
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(x, object) {
        if (is.vector(x)) {
          x <- tibble::tibble(v1 = 1 - x, v2 = x)
        } else {
          x <- tibble::as_tibble(x)
        }
        colnames(x) <- object$lvl
        x
      },
      func = c(pkg = NULL, fun = "predict"),
      args = list(object = quote(object$fit), new_data = quote(new_data))
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), new_data = quote(new_data))
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


  browser()
  if (is.data.frame(x)) {
    x <- model.matrix(~., data = x)
  }

  # override or add some other args
  others <- list(...)
  others <- others[!(names(others) %in% c("data", names(arg_list)))]

  if(is.null(others$num_leaves)) others$num_leaves = 2^max_depth - 1

  arg_list <- purrr::compact(c(arg_list, others))


  # train ------------------------
  d_a <- lightgbm::lgb.Dataset(data = x, label = y, feature_pre_filter = FALSE)
  d_b <- lightgbm::lgb.Dataset(data = x, label = iris$Sepal.Length, feature_pre_filter = FALSE)

  main_args_a <- list(
    data = quote(d_a),
    params = arg_list
  )
  main_args_b <- list(
    data = quote(d_b),
    params = arg_list
  )
  call_a <- parsnip:::make_call(fun = "lgb.train", ns = "lightgbm", main_args_a)
  a <- rlang::eval_tidy(call_a, env = rlang::current_env())
  call_b <- parsnip:::make_call(fun = "lgb.train", ns = "lightgbm", main_args_b)
  b <- rlang::eval_tidy(call_b, env = rlang::current_env())

  # predict(a, model.matrix(Sepal.Length ~., data = iris), predict_disable_shape_check = FALSE)
  lightgbm:::predict.lgb.Booster(a, data = x)
  lightgbm:::predict.lgb.Booster(b, data = x)
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
multi_predict._lightgbm.Model <- function(object, new_data, type = NULL, trees = NULL, ...) {
  if (any(names(rlang::enquos(...)) == "newdata")) {
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
  }

  if (is.null(trees)) {
    trees <- object$fit$tree_count
  }
  trees <- sort(trees)

  if (is.null(type)) {
    if (object$spec$mode == "classification")
      type <- "Class"
    else
      type <- "RawFormulaVal"
  } else {
    type <- switch (
      type,
      "raw" = "RawFormulaVal",
      "numeric" = "RawFormulaVal",
      "class" = "Class",
      "prob" = "Probability"
    )
  }

  res <- map_df(trees, lightgbm_by_tree, object = object, new_data = new_data, type = type, ...)
  res <- dplyr::arrange(res, .row, trees)
  res <- split(res[, -1], res$.row)
  names(res) <- NULL

  tibble::tibble(.pred = res)

}

lightgbm_by_tree <- function(tree, object, new_data, type, ...) {

  pred <- predict.lightgbm.Model(object$fit, new_data, ntree_end = tree, type = type, ...)

  # switch based on prediction type
  if (object$spec$mode == "regression") {
    pred <- tibble::tibble(.pred = pred)
    nms <- names(pred)
  } else {
    if (type == "class") {
      pred <- object$spec$method$pred$class$post(pred, object)
      pred <- tibble::tibble(.pred_class = factor(pred, levels = object$lvl))
    } else {
      pred <- object$spec$method$pred$prob$post(pred, object)
      pred <- tibble::as_tibble(pred)
      names(pred) <- paste0(".pred_", names(pred))
    }
    nms <- names(pred)
  }
  pred[["trees"]] <- tree
  pred[[".row"]] <- 1:nrow(new_data)
  pred[, c(".row", "trees", nms)]
}

#' @export
predict.lightgbm.Model <- function(object, new_data, type = "RawFormulaVal", ...) {
  browser()
  if (!inherits(new_data, "lightgbm.Pool")) {
    new_data <- lightgbm::lightgbm.load_pool(new_data)
  }

  type <- switch (
    type,
    "raw" = "RawFormulaVal",
    "numeric" = "RawFormulaVal",
    "class" = "Class",
    "prob" = "Probability",
    "RawFormulaVal"
  )

  lightgbm::lightgbm.predict(object, new_data, prediction_type = type, ...)
}




