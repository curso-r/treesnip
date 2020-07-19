test_that("lightgbm", {

  model <- parsnip::boost_tree(mtry = 1, trees = 50)
  model <- parsnip::set_engine(model, "lightgbm", verbosity = -1L)

  expect_regression_works(parsnip::set_mode(model, "regression"))
  expect_multiclass_classification_works(parsnip::set_mode(model, "classification"))
  expect_binary_classification_works(parsnip::set_mode(model, "classification"))

})


test_that("lightgbm with tune", {

  model <- parsnip::boost_tree(mtry = 5, trees = tune())
  model <- parsnip::set_engine(model, "xgboost")
  model <- parsnip::set_mode(model, "regression")

  expect_can_tune_boost_tree(model)

})
