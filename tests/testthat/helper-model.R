library(modeldata)
library(parsnip)

data("wa_churn")
data("lending_club")

expect_regression_works <- function(model) {

  adj <- fit(model, mpg ~ ., data = mtcars)

  pred <- predict(adj, mtcars)
  expect_equal(nrow(pred), nrow(mtcars))

  expect_mse(pred, mtcars$mpg, less_than = mean(mtcars$mpg))
}

expect_classification_works <- function(model) {

  adj <- fit(model, churn ~ ., data = wa_churn)

  pred <- predict(adj, wa_churn, type = "prob")
  expect_equal(nrow(pred), nrow(wa_churn))

  pred <- predict(adj, wa_churn)
  expect_equal(nrow(pred), nrow(wa_churn))

  expect_accuracy(pred$.pred_class, wa_churn$churn, at_least = 0.7)
}

expect_mse <- function(pred, true, less_than) {
  mse <- sqrt(mean((pred$.pred - true)^2))
  expect_true(mse < less_than)
}

expect_accuracy <- function(pred, true, at_least) {
  x <- table(pred, true)
  expect_true(sum(diag(x))/sum(x) > at_least)
}
