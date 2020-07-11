mtcars_class <- mtcars
mtcars_class$cyl <- as.factor(mtcars$cyl)

expect_regression_works <- function(model) {

  adj <- parsnip::fit(model, mpg ~ ., data = mtcars)

  pred <- predict(adj, mtcars)
  expect_equal(nrow(pred), nrow(mtcars))

  expect_mse(pred, mtcars$mpg, less_than = mean(mtcars$mpg))
}

expect_classification_works <- function(model) {

  adj <- parsnip::fit(model, cyl ~ ., data = mtcars_class)

  pred <- predict(adj, mtcars_class, type = "prob")
  expect_equal(nrow(pred), nrow(mtcars_class))

  pred <- predict(adj, mtcars_class)
  expect_equal(nrow(pred), nrow(mtcars_class))

  expect_accuracy(pred$.pred_class, mtcars_class$cyl, at_least = 0.7)
}

expect_mse <- function(pred, true, less_than) {
  mse <- sqrt(mean((pred$.pred - true)^2))
  expect_true(mse < less_than)
}

expect_accuracy <- function(pred, true, at_least) {
  x <- table(pred, true)
  expect_true(sum(diag(x))/sum(x) > at_least)
}
