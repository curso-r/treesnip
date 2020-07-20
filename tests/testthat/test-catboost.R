test_that("catboost", {

  model <- parsnip::boost_tree(mtry = 1, trees = 50)
  expect_all_modes_works(model, 'catboost')

})


test_that("catboost with tune", {

  model <- parsnip::boost_tree(mtry = 5, trees = tune())
  model <- parsnip::set_engine(model, "catboost")
  model <- parsnip::set_mode(model, "regression")

  expect_can_tune_boost_tree(model)

})


test_that("catboost mtry", {

  hiperparameters <- data.frame(mtry = c(1, 2, 6))
  for(i in 1:nrow(hiperparameters)) {
    model <- parsnip::boost_tree(mtry = hiperparameters$mtry[i])
    expect_all_modes_works(model, 'catboost')
  }

})

test_that("catboost trees", {

  hiperparameters <- data.frame(trees = c(1, 20, 300))
  for(i in 1:nrow(hiperparameters)) {
    model <- parsnip::boost_tree(trees = hiperparameters$trees[i])
    expect_all_modes_works(model, 'catboost')
  }

})


test_that("catboost min_n hiperparameter", {

  hiperparameters <- data.frame(min_n = c(1, 10))
  for(i in 1:nrow(hiperparameters)) {
    model <- parsnip::boost_tree(min_n = hiperparameters$min_n[i])
    expect_all_modes_works(model, 'catboost')
  }

})

test_that("catboost tree_depth", {

  hiperparameters <- data.frame(tree_depth = c(1, 16))
  for(i in 1:nrow(hiperparameters)) {
    cat(hiperparameters$tree_depth[i], "\n")
    model <- parsnip::boost_tree(tree_depth = hiperparameters$tree_depth[i])
    expect_all_modes_works(model, 'catboost')
  }

})
