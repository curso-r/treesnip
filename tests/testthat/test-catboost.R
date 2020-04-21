test_that("catboost works", {
  model <- parsnip::boost_tree(mtry = 1, trees = 1)
  model <- parsnip::set_mode(model, "regression")
  model <- parsnip::set_engine(model, "catboost")

  model <- parsnip::fit(model, mpg ~ . , data = mtcars)
})
