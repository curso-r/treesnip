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
      protect = c("x", "y"),
      func = c(pkg = "treesnip", fun = "train_catboost"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "boost_tree",
    mode = "regression",
    eng = "catboost",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE
    )
  )

  parsnip::set_encoding(
    model = "boost_tree",
    mode = "classification",
    eng = "catboost",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "catboost",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), new_data = quote(new_data))
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "catboost",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), new_data = quote(new_data))
    )
  )

  parsnip::set_fit(
    model = "boost_tree",
    eng = "catboost",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "treesnip", fun = "train_catboost"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "catboost",
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
    eng = "catboost",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(x, object) {
        if (is.vector(x)) {
          x <- tibble::tibble(v1 = 1 - x, v2 = x)
        } else {
          x <- tibble::as_tibble(x, .name_repair = make.names)
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
    eng = "catboost",
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
    eng = "catboost",
    parsnip = "tree_depth",
    original = "depth",
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
    original = "learning_rate",
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
    original = "min_data_in_leaf",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  # parsnip::set_model_arg(
  #   model = "boost_tree",
  #   eng = "catboost",
  #   parsnip = "loss_reduction",
  #   original = "gamma", # There is no such parameter in catboost
  #   func = list(pkg = "dials", fun = "loss_reduction"),
  #   has_submodel = FALSE
  # )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "catboost",
    parsnip = "sample_size",
    original = "subsample",
    func = list(pkg = "dials", fun = "sample_size"),
    has_submodel = FALSE
  )
}

prepare_df_catboost <- function(x, y = NULL, categorical_cols= NULL) {
  if(is.null(categorical_cols)){
    # auto detect the categorical columns from data.frame
    # Not strictly necessary but good form.
    categorical_cols <- categorical_columns(x)
  }

  # catboost uses 0-indexed feature cols
  if(!is.null(categorical_cols)){categorical_cols <- categorical_cols-1}

  if (is.null(y))
    return(x)

  catboost::catboost.load_pool(
    data = x,
    label = y,
    cat_features = categorical_cols
  )
}


#' Boosted trees via catboost
#'
#' `catboost_train` is a wrapper for `catboost` tree-based models
#'  where all of the model arguments are in the main function.
#'
#' @param x A data frame or matrix of predictors
#' @param y A vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param depth An integer for the maximum depth of the tree.
#' @param iterations An integer for the number of boosting iterations.
#' @param learning_rate A numeric value between zero and one to control the learning rate.
#' @param rsm Subsampling proportion of columns.
#' @param min_data_in_leaf A numeric value for the minimum sum of instances needed
#'  in a child to continue to split.
#' @param subsample Subsampling proportion of rows.
#' @param categorical_cols indices of categorical columns, when NULL (default) factor columns are automatically detected
#' @param ... Other options to pass to `catboost.train`.
#' @return A fitted `catboost.Model` object.
#' @keywords internal
#' @export
train_catboost <- function(x, y, depth = 6, iterations = 1000, learning_rate = NULL,
                           rsm = 1, min_data_in_leaf = 1, subsample = 1,
                           categorical_cols = NULL, ...) {

  # rsm ------------------------------
  if(!is.null(rsm)) {
    rsm <- rsm/ncol(x)
  }
  if(rsm > 1) {
    rsm <- 1
  }

  # subsample -----------------------
  if (subsample > 1) {
    subsample <- 1
  }

  # loss -------------------------
  if (is.numeric(y)) {
    loss_function <- "RMSE"
  } else {
    lvl <- levels(y)
    y <- as.numeric(y) - 1
    if (length(lvl) == 2) {
      loss_function <- "Logloss"
    } else {
      loss_function <- "MultiClass"
    }
  }

  arg_list <- list(
    loss_function = loss_function,
    iterations = iterations,
    learning_rate = learning_rate,
    depth = depth,
    rsm = rsm,
    min_data_in_leaf = min_data_in_leaf,
    subsample = subsample
  )

  # train ------------------------
  d <- prepare_df_catboost(x, y = y, categorical_cols = categorical_cols)

  # override or add some other args
  others <- list(...)
  others <- others[!(names(others) %in% c("learn_pool", "test_pool", names(arg_list)))]

  if(is.null(others$logging_level)) others$logging_level = "Silent"
  if(is.null(others$bootstrap_type)) others$bootstrap_type = "Bernoulli" # subsample as is
  if(is.null(others$sampling_frequency)) others$sampling_frequency = "PerTree" # subsample as is

  # artificial alias for thread_count (for match xgboost and lightgbm)
  if(is.null(others$thread_count) & is.null(others$nthread)) {
    others$thread_count = 1L # parallelism should be explicitly specified by the user
  } else {
    others$thread_count = ifelse(!is.null(others$thread_count), others$thread_count, others$nthread)
    others$nthread <- NULL
  }

  arg_list <- purrr::compact(c(arg_list, others))
  main_args <- list(
    learn_pool = quote(d),
    params = arg_list
  )

  call <- parsnip:::make_call(fun = "catboost.train", ns = "catboost", main_args)
  rlang::eval_tidy(call, env = rlang::current_env())
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
#' @param categorical_cols indices of categorical columns, when NULL (default) factor columns are automatically detected.
#'
#' @export
#' @importFrom purrr map_df
multi_predict._catboost.Model <- function(object, new_data, type = NULL, trees = NULL, categorical_cols = NULL, ...) {
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
        "prob" = "Probability",
        type
      )
    }

    res <- map_df(trees, catboost_by_tree, object = object, new_data = new_data, type = type, categorical_cols = categorical_cols, ...)
    res <- dplyr::arrange(res, .row, trees)
    res <- split(res[, -1], res$.row)
    names(res) <- NULL

    tibble::tibble(.pred = res)

  }

catboost_by_tree <- function(tree, object, new_data, type, categorical_cols = NULL,...) {
  d <- prepare_df_catboost(new_data, categorical_cols = categorical_cols)
  pred <- predict.catboost.Model(object$fit, d, ntree_end = tree, type = type, categorical_cols = categorical_cols, ...)
  # switch based on prediction type
  if (object$spec$mode == "regression") {
    pred <- tibble::tibble(.pred = pred)
    nms <- names(pred)
  } else {
    if (type == "Class") {
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
predict.catboost.Model <- function(object, new_data, type = "RawFormulaVal", categorical_cols = NULL, ...) {
  if (!inherits(new_data, "catboost.Pool")) {
    d <- prepare_df_catboost(new_data, categorical_cols = categorical_cols)
    new_data <- catboost::catboost.load_pool(d, cat_features = categorical_cols)
  }

  prediction_type <- switch (
    type,
    "raw" = "RawFormulaVal",
    "numeric" = "RawFormulaVal",
    "class" = "Class",
    "prob" = "Probability",
    type
  )

  catboost::catboost.predict(object, new_data, prediction_type = prediction_type, ...)
}




