skip("need to rework tests")

test_that("lightgbm", {

  model <- parsnip::boost_tree(mtry = 1, trees = 50, tree_depth = 15, min_n = 1)

  expect_all_modes_works(model, 'lightgbm')
})


test_that('lightgbm alternate objective', {
  skip_if_not_installed("lightgbm")

  spec <- boost_tree(mtry = 1, trees = 50, tree_depth = 15, min_n = 1) %>%
    set_engine("lightgbm", objective = "huber") %>%
    set_mode("regression")

  lgb_fit <- spec %>% fit(mpg ~ ., data = mtcars)

  info <- jsonlite::fromJSON(lightgbm::lgb.dump(lgb_fit$fit))

  expect_equal(info$objective, "huber")
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
    model <- parsnip::boost_tree(mtry = hyperparameters$mtry[i], min_n = 1)
    expect_all_modes_works(model, 'lightgbm')
  }

})

test_that("lightgbm trees", {

  hyperparameters <- data.frame(trees = c(1, 20, 50))
  for(i in 1:nrow(hyperparameters)) {
    model <- parsnip::boost_tree(trees = hyperparameters$trees[i], min_n = 1)
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
    model <- parsnip::boost_tree(tree_depth = hyperparameters$tree_depth[i], min_n = 1)
    expect_all_modes_works(model, 'lightgbm')
  }
})

test_that("lightgbm loss_reduction", {
  hyperparameters <- data.frame(loss_reduction = c(0, 0.2, 2))
  for(i in 1:nrow(hyperparameters)) {
    model <- parsnip::boost_tree(loss_reduction = hyperparameters$loss_reduction[i], min_n = 1)
    expect_all_modes_works(model, 'lightgbm')
  }
})

test_that("lightgbm tree_depth", {
  hyperparameters <- data.frame(loss_reduction = c(0, 0.2, 2))
  for(i in 1:nrow(hyperparameters)) {
    model <- parsnip::boost_tree(loss_reduction = hyperparameters$loss_reduction[i], min_n = 1)
    expect_all_modes_works(model, 'lightgbm')
  }
})

test_that("lightgbm multi_predict", {
  model <- parsnip::boost_tree(mtry = 5, trees = 5, mode = "regression", min_n = 1)
  model <- parsnip::set_engine(model, "lightgbm")

  expect_multi_predict_works(model)
})

