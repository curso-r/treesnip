test_that("lightgbm", {

  model <- parsnip::boost_tree(mtry = 1, trees = 50, tree_depth = 15)

  expect_all_modes_works(model, 'lightgbm')
})


test_that("lightgbm with tune", {

  model <- parsnip::boost_tree(
    mtry = 5,
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    trees = tune(),
    min_n = tune(),
    tree_depth = tune()
  )
  model <- parsnip::set_engine(model, "lightgbm")

  expect_can_tune_boost_tree(model)

})


test_that("lightgbm mtry", {

  hyperparameters <- data.frame(mtry = c(1, 2, 6))
  for(i in 1:nrow(hyperparameters)) {
    model <- parsnip::boost_tree(mtry = hyperparameters$mtry[i])
    expect_all_modes_works(model, 'lightgbm')
  }

})

test_that("lightgbm trees", {

  hyperparameters <- data.frame(trees = c(1, 20, 300))
  for(i in 1:nrow(hyperparameters)) {
    model <- parsnip::boost_tree(trees = hyperparameters$trees[i])
    expect_all_modes_works(model, 'lightgbm')
  }

})


test_that("lightgbm min_n hyperparameter", {

  hyperparameters <- data.frame(min_n = c(1, 10))
  for(i in 1:nrow(hyperparameters)) {
    model <- parsnip::boost_tree(min_n = hyperparameters$min_n[i])
    expect_all_modes_works(model, 'lightgbm')
  }

})

test_that("lightgbm tree_depth", {
  hyperparameters <- data.frame(tree_depth = c(1, 16))
  for(i in 1:nrow(hyperparameters)) {
    model <- parsnip::boost_tree(tree_depth = hyperparameters$tree_depth[i])
    expect_all_modes_works(model, 'lightgbm')
  }
})



test_that("lightgbm multi_predict", {
  model <- parsnip::boost_tree(mtry = 5, trees = 5, mode = "regression")
  model <- parsnip::set_engine(model, "lightgbm")

  expect_multi_predict_works(model)
})

