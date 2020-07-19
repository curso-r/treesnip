mtcars_class <- mtcars
mtcars_class$cyl <- as.factor(mtcars$cyl)
mtcars_class_binary <- mtcars
mtcars_class_binary$vs <- as.factor(mtcars$vs)

expect_all_modes_works <- function(model, engine) {
  model <- parsnip::set_engine(model, engine, verbosity = -1L)
  expect_regression_works(parsnip::set_mode(model, "regression"))
  expect_multiclass_classification_works(parsnip::set_mode(model, "classification"))
  expect_binary_classification_works(parsnip::set_mode(model, "classification"))
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

  pred <- predict(adj, mtcars_class_binary, type = "prob")
  expect_equal(nrow(pred), nrow(mtcars_class_binary))
  expect_equal(names(pred), c(".pred_0", ".pred_1"))
  expect_not_constant_predictions(pred$.pred_1)

  pred <- predict(adj, mtcars_class_binary)
  expect_equal(nrow(pred), nrow(mtcars_class_binary))
  expect_equal(names(pred), ".pred_class")

  expect_accuracy(pred$.pred_class, mtcars_class_binary$vs, at_least = 0.1)
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

  expect_accuracy(pred$.pred_class, mtcars_class$cyl, at_least = 0.1)
}

expect_can_tune_boost_tree <- function(model) {

  grid_df <- data.frame(trees = c(10, 20))
  resamples <- rsample::vfold_cv(mtcars, v = 2)
  adj <- tune::tune_grid(
    model,
    mpg ~ .,
    resamples = resamples,
    grid = 3,
    metrics = yardstick::metric_set(yardstick::rmse)
  )

  expect_equal(nrow(adj), nrow(resamples))
  expect_equal(nrow(tune::collect_metrics(adj)), 3)

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
