test_that("lightgbm", {

  model <- parsnip::boost_tree(mtry = 1, trees = 50)
  model <- parsnip::set_engine(model, "lightgbm", verbosity = -1L)

  expect_regression_works(parsnip::set_mode(model, "regression"))
  expect_multiclass_classification_works(parsnip::set_mode(model, "classification"))
  expect_binary_classification_works(parsnip::set_mode(model, "classification"))

})


test_that("lightgbm with tune", {
  library(treesnip)
  model <- parsnip::boost_tree(mtry = 1, trees = tune())
  model <- parsnip::set_engine(model, "lightgbm", verbosity = -1L)
  model <- parsnip::set_mode(model, "regression")
  grid_df <- data.frame(trees = c(10, 20))
  adj <- tune::tune_grid(model, mpg ~ ., resamples = rsample::vfold_cv(mtcars, v = 2), grid = grid_df)

})
