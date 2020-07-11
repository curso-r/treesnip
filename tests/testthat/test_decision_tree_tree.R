test_that("tree", {

  model <- parsnip::decision_tree(min_n = 1)
  model <- parsnip::set_engine(model, "tree")

  expect_regression_works(model %>% set_mode("regression"))

})
