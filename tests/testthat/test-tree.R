skip("need to rework tests")
test_that("tree", {

  model <- parsnip::decision_tree(min_n = 1)
  model <- parsnip::set_engine(model, "tree")

  expect_regression_works(parsnip::set_mode(model, "regression"))

})
