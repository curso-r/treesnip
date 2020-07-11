test_that("lightgbm", {

  model <- parsnip::boost_tree(mtry = 1, trees = 50)
  model <- parsnip::set_engine(model, "lightgbm")

  expect_regression_works(model %>% set_mode("regression"))
  expect_classification_works(model %>% set_mode("classification"))

})


