test_that("catboost", {

  model <- parsnip::boost_tree(mtry = 1, trees = 50)
  model <- parsnip::set_engine(model, "catboost")

  expect_regression_works(model %>% set_mode("regression"))
  expect_classification_works(model %>% set_mode("classification"))

})

