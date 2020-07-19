test_that("lightgbm", {

  model <- parsnip::boost_tree(mtry = 1, trees = 50, tree_depth = 50)
  expect_all_modes_works(model, 'lightgbm')

})


test_that("lightgbm with tune", {

  model <- parsnip::boost_tree(mtry = 5, trees = tune())
  model <- parsnip::set_engine(model, "lightgbm")
  model <- parsnip::set_mode(model, "regression")

  expect_can_tune_boost_tree(model)

})


test_that("lightgbm mtry", {

  hiperparameters <- data.frame(mtry = c(1, 2, 3, 6))
  for(i in 1:nrow(hiperparameters)) {
    model <- parsnip::boost_tree(mtry = hiperparameters$mtry[i])
    expect_all_modes_works(model, 'lightgbm')
  }

})

test_that("lightgbm trees", {

  hiperparameters <- data.frame(trees = c(1, 20, 300, 6000))
  for(i in 1:nrow(hiperparameters)) {
    model <- parsnip::boost_tree(trees = hiperparameters$trees[i])
    expect_all_modes_works(model, 'lightgbm')
  }

})


test_that("lightgbm min_n hiperparameter", {

  hiperparameters <- data.frame(min_n = c(1, 10))
  for(i in 1:nrow(hiperparameters)) {
    model <- parsnip::boost_tree(min_n = hiperparameters$min_n[i])
    expect_all_modes_works(model, 'lightgbm')
  }

})

test_that("lightgbm tree_depth", {

  hiperparameters <- data.frame(tree_depth = c(1, 17, 50))
  for(i in 1:nrow(hiperparameters)) {
    cat(hiperparameters$tree_depth[i], "\n")
    model <- parsnip::boost_tree(tree_depth = hiperparameters$tree_depth[i])
    expect_all_modes_works(model, 'lightgbm')
  }

})
