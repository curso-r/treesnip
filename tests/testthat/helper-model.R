mtcars_class <- mtcars
mtcars_class$cyl <- as.factor(mtcars$cyl)
mtcars_class_binary <- mtcars
mtcars_class_binary$vs <- as.factor(mtcars$vs)

expect_all_modes_works <- function(model, engine, ...) {
  if(engine == "lightgbm") {
    model <- parsnip::set_engine(model, engine, verbosity = -1L, ...)
  } else {
    model <- parsnip::set_engine(model, engine, ...)
  }

  expect_regression_works(parsnip::set_mode(model, "regression"))
  expect_multiclass_classification_works(parsnip::set_mode(model, "classification"))
  expect_binary_classification_works(parsnip::set_mode(model, "classification"))
  expect_categorical_vars_works(parsnip::set_mode(model, "regression"))
}

expect_regression_works <- function(model) {

  adj <- parsnip::fit(model, mpg ~ ., data = mtcars)

  pred <- predict(adj, mtcars)
  expect_equal(nrow(pred), nrow(mtcars))

  expect_mse(pred, mtcars$mpg, less_than = mean(mtcars$mpg))
  expect_not_constant_predictions(pred$.pred)
}

expect_binary_classification_works <- function(model) {

  adj <- parsnip::fit(model, vs ~ ., data = mtcars_class_binary)

  # type prob
  pred <- predict(adj, mtcars_class_binary, type = "prob")
  expect_equal(nrow(pred), nrow(mtcars_class_binary))
  expect_equal(names(pred), c(".pred_0", ".pred_1"))
  expect_not_constant_predictions(pred$.pred_1)
  expect_between_0_and_1(pred$.pred_1)
  expect_between_0_and_1(pred$.pred_0)

  # type raw
  pred <- predict(adj, mtcars_class_binary, type = "raw")
  expect_equal(length(pred), nrow(mtcars_class_binary))

  # type class
  pred <- predict(adj, mtcars_class_binary)
  expect_equal(nrow(pred), nrow(mtcars_class_binary))
  expect_equal(names(pred), ".pred_class")

  expect_accuracy(pred$.pred_class, mtcars_class_binary$vs, at_least = 0.4)
}

expect_multiclass_classification_works <- function(model) {

  adj <- parsnip::fit(model, cyl ~ ., data = mtcars_class)

  pred <- predict(adj, mtcars_class, type = "prob")
  expect_equal(nrow(pred), nrow(mtcars_class))
  expect_equal(names(pred), c(".pred_4", ".pred_6", ".pred_8"))
  expect_not_constant_predictions(pred$.pred_8)

  pred <- predict(adj, mtcars_class)
  expect_equal(nrow(pred), nrow(mtcars_class))
  expect_equal(names(pred), ".pred_class")

  expect_accuracy(pred$.pred_class, mtcars_class$cyl, at_least = 0.4)
}

expect_categorical_vars_works <- function(model) {

  df <- data.frame(
    x1 = as.factor(sample(letters, 1000, replace = TRUE)),
    y = runif(1000)
  )

  adj <- parsnip::fit(model, y ~ ., data = df)

  p <- predict(adj, df)

  expect_true(length(unique(p$.pred)) <= length(unique(df$x1)))

  expect_error(
    predict(adj, data.frame(x1 = c("str", "str2"), stringsAsFactors = FALSE))
  )

  expect_equal(
    predict(adj, data.frame(x1 = factor(c("a", "b"), levels = c("b", "a"))))$.pred,
    predict(adj, data.frame(x1 = factor(c("a", "b"), levels = c("a", "b"))))$.pred
  )

}

expect_can_tune_boost_tree <- function(model) {
  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$vs <- factor(mtcars$vs)

  resamples <- rsample::vfold_cv(mtcars, v = 2)

  # regression
  adj <- tune::tune_grid(
    parsnip::set_mode(model, "regression"),
    mpg ~ .,
    resamples = resamples,
    grid = 2,
    metrics = yardstick::metric_set(yardstick::rmse)
  )

  expect_equal(nrow(adj), nrow(resamples))
  expect_equal(nrow(tune::collect_metrics(adj)), 2)
  expect_true(all(!is.nan(tune::collect_metrics(adj)$mean)))

  # classification
  adj <- tune::tune_grid(
    parsnip::set_mode(model, "classification"),
    cyl ~ .,
    resamples = resamples,
    grid = 2,
    metrics = yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)
  )

  expect_equal(nrow(adj), nrow(resamples))
  expect_equal(nrow(tune::collect_metrics(adj)), 4)
  expect_true(all(!is.nan(tune::collect_metrics(adj)$mean)))


}

expect_mse <- function(pred, true, less_than) {
  mse <- sqrt(mean((pred$.pred - true)^2))
  expect_true(mse < less_than)
}

expect_accuracy <- function(pred, true, at_least) {
  x <- table(pred, true)
  expect_true(sum(diag(x))/sum(x) > at_least)
}

expect_not_constant_predictions <- function(pred) {
  expect_true(length(unique(pred)) > 1)
}

expect_between_0_and_1 <- function(pred) {
  expect_true(max(pred) <= 1)
  expect_true(min(pred) >= 0)
}

expect_multi_predict_works <- function(model) {
  df <- data.frame(x1 = runif(1000))
  df$y <- df$x1 + rnorm(1000)

  adj <- parsnip::fit(model, y ~ ., data = df)

  expect_true(parsnip::has_multi_predict(adj))
  expect_equal(parsnip::multi_predict_args(adj), "trees")

  mp <- parsnip::multi_predict(adj, df[1:3,], trees = c(1, 3, 5))
  expect_equal(nrow(mp), 3)
  expect_equal(nrow(tidyr::unnest(mp, .pred)), 3*3)

  # expect that predictions from different trees would result in different predictions
  expect_true(sum(purrr::map_dbl(mp$.pred, ~var(.x$.pred))) > 0)
}
