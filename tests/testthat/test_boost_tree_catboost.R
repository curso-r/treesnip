library(testthat)
library(parsnip)
library(treesnip)

context("boosted tree execution with catboost")
# source("helper-objects.R") from parsnip -------------------------------------
library(modeldata)

data("wa_churn")
data("lending_club")

# ------------------------------------------------------------------------------

ctrl          <- control_parsnip(verbosity = 1, catch = FALSE)
caught_ctrl   <- control_parsnip(verbosity = 1, catch = TRUE)
quiet_ctrl    <- control_parsnip(verbosity = 0, catch = TRUE)

run_glmnet <- utils::compareVersion('3.6.0', as.character(getRversion())) > 0
# end source("helper-objects.R") from parsnip ----------------------------------



test_that("catboost fit works", {
  model <- parsnip::boost_tree(mtry = 1, trees = 1)
  model <- parsnip::set_mode(model, "regression")
  model <- parsnip::set_engine(model, "catboost")

  # regression
  catboost_fit <- parsnip::fit(model, mpg ~ . , data = mtcars)
  expect_equal(class(catboost_fit), c("_catboost.Model", "model_fit"))
  expect_equal(class(catboost_fit$fit$feature_importances)[1], c("matrix"))
  expect_equal(sum(catboost_fit$fit$feature_importances > 0), 2)

  # classification
  mtcars_class <- mtcars
  mtcars_class$vs <- factor(mtcars_class$vs)
  catboost_fit <- parsnip::fit(model, vs ~ . , data = mtcars_class)
  expect_equal(class(catboost_fit), c("_catboost.Model", "model_fit"))
  expect_equal(class(catboost_fit$fit$feature_importances)[1], c("matrix"))
  expect_equal(sum(catboost_fit$fit$feature_importances > 0), 2)

  # multi-classification
  mtcars_class <- mtcars
  mtcars_class$cyl <- factor(mtcars_class$cyl)
  catboost_fit <- parsnip::fit(model, cyl ~ . , data = mtcars_class)
  expect_equal(class(catboost_fit), c("_catboost.Model", "model_fit"))
  expect_equal(class(catboost_fit$fit$feature_importances)[1], c("matrix"))
  expect_equal(sum(catboost_fit$fit$feature_importances > 0), 2)
})

# ------------------------------------------------------------------------------

num_pred <- names(iris)[1:4]

iris_catboost <- boost_tree(trees = 2, mode = "classification") %>%
  set_engine("catboost")

# ------------------------------------------------------------------------------

test_that('catboost execution, classification', {

  skip_if_not_installed("catboost")

  expect_error(
    res <- parsnip::fit(
      iris_catboost,
      Species ~ Sepal.Width + Sepal.Length,
      data = iris,
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- parsnip::fit_xy(
      iris_catboost,
      x = iris[, num_pred],
      y = iris$Species,
      control = ctrl
    ),
    regexp = NA
  )

  expect_true(has_multi_predict(res))
  # expect_equal(multi_predict_args(res), "trees") # parsnips bug?

  expect_error(
    res <- parsnip::fit(
      iris_catboost,
      Species ~ novar,
      data = iris,
      control = ctrl
    )
  )
})


test_that('catboost classification prediction', {

  skip_if_not_installed("catboost")

  library(catboost)
  xy_fit <- fit_xy(
    iris_catboost,
    x = iris[, num_pred],
    y = iris$Species,
    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, new_data = catboost::catboost.load_pool(iris[1:8, num_pred]), type = "class")
  xy_pred <- factor(levels(iris$Species)[xy_pred + 1], levels = levels(iris$Species))

  expect_equal(xy_pred, predict(xy_fit, new_data = iris[1:8, num_pred], type = "class")$.pred_class)
  expect_equal(xy_pred, predict(xy_fit, new_data = iris[1:8, rev(num_pred)], type = "class")$.pred_class)

  form_fit <- fit(
    iris_catboost,
    Species ~ .,
    data = iris,
    control = ctrl
  )

  form_pred <- predict(form_fit$fit, new_data = catboost::catboost.load_pool(iris[1:8, num_pred]), type = "class")
  form_pred <- factor(levels(iris$Species)[form_pred + 1], levels = levels(iris$Species))
  expect_equal(form_pred, predict(form_fit, new_data = iris[1:8, num_pred], type = "class")$.pred_class)
  expect_equal(form_pred, predict(form_fit, new_data = iris[1:8, rev(num_pred)], type = "class")$.pred_class)
})

# ------------------------------------------------------------------------------

num_pred <- names(mtcars)[3:6]

car_basic <- boost_tree(mode = "regression") %>%
  set_engine("catboost", leaf_estimation_method = -1)

test_that('catboost execution, regression', {

  skip_if_not_installed("catboost")

  expect_error(
    res <- parsnip::fit(
      car_basic,
      mpg ~ .,
      data = mtcars,
      control = ctrl
    )
  )

  expect_error(
    res <- parsnip::fit_xy(
      car_basic,
      x = mtcars[, num_pred],
      y = mtcars$mpg,
      control = ctrl
    )
  )
})


car_basic <-
  boost_tree(mode = "regression") %>%
  set_engine("catboost")

test_that('catboost regression prediction', {

  skip_if_not_installed("catboost")

  xy_fit <- fit_xy(
    car_basic,
    x = mtcars[, -1],
    y = mtcars$mpg,
    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, new_data = catboost::catboost.load_pool(mtcars[1:8, -1]))
  expect_equal(xy_pred, predict(xy_fit, new_data = mtcars[1:8, -1])$.pred)

  form_fit <- fit(
    car_basic,
    mpg ~ .,
    data = mtcars,
    control = ctrl
  )

  form_pred <- predict(form_fit$fit, new_data = catboost::catboost.load_pool(mtcars[1:8, -1]))
  expect_equal(form_pred, predict(form_fit, new_data = mtcars[1:8, -1])$.pred)
})



test_that('submodel prediction', {

  skip_if_not_installed("catboost")
  library(catboost)

  reg_fit <- boost_tree(trees = 20, mode = "regression") %>%
    set_engine("catboost") %>%
    fit(mpg ~ ., data = mtcars[-(1:4), ])

  x <- catboost::catboost.load_pool(mtcars[1:4, -1])

  pruned_pred <- predict(reg_fit$fit, x, ntree_end = 5)

  mp_res <- multi_predict(object = reg_fit, new_data = mtcars[1:4, -1], trees = 5)
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred"]], pruned_pred)


  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    boost_tree(trees = 20, mode = "classification") %>%
    set_engine("catboost") %>%
    fit(churn ~ ., data = wa_churn[-(1:4), c("churn", vars)])

  x <-  catboost::catboost.load_pool(wa_churn[1:4, vars])

  pred_class <- predict(class_fit$fit, x, ntree_end = 5)

  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], trees = 5, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], pred_class)

  expect_error(
    multi_predict(class_fit, newdata = wa_churn[1:4, vars], trees = 5, type = "prob"),
    "Did you mean"
  )
})

model_with_tune <-
  boost_tree(mode = "regression", trees = tune()) %>%
  set_engine("catboost")

rs <- rsample::vfold_cv(data = mtcars[-(1:4), ], 2)
tg <- tune::tune_grid(
  model_with_tune,
  mpg ~ .,
  resamples = rs
)

final_model <- finalize_model(model_with_tune, select_best(tg, "rsq"))

test_that('tune package for catboost works', {
  expect_equal(class(rlang::quo_get_expr(final_model$args$trees)), "integer")
})


final_model <-
  boost_tree(mode = "regression", trees = 5) %>%
  set_engine("xgboost")

test_that('recipe and workflows packages for catboost works', {

  final_model <-
    parsnip::boost_tree(mode = "regression", trees = 5) %>%
    parsnip::set_engine("catboost")

  rec <- recipes::recipe(mpg ~ ., mtcars) %>%
    recipes::step_center(recipes::all_outcomes(), skip = TRUE)

  wf <- workflows::workflow() %>%
    workflows::add_model(final_model) %>%
    workflows::add_recipe(rec)

  model <- parsnip::fit(
    wf,
    data = mtcars
  )

  # not working with transformations on outcomes
  expect_equal(class(workflows:::predict.workflow(model, new_data = mtcars[1:4, ]))[1], "tbl_df")
})

