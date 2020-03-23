test_that("tree engine works", {

  library(tidymodels)
  df <- data.frame(x = runif(10), y= runif(10))

  mod <- decision_tree(min_n = 0, cost_complexity = 0) %>%
     set_engine("tree") %>%
     set_mode("regression")

  res <- fit(mod, y ~x, df)
  pred <- predict(res, df)$.pred

  expect_equal(pred, df$y)
})

test_that("can tune tree parameters", {

  df <- data.frame(x = runif(10), y= runif(10))

  mod <- decision_tree(min_n = tune(), cost_complexity = tune()) %>%
    set_engine("tree") %>%
    set_mode("regression")

  wfl <- workflow() %>%
    add_model(mod) %>%
    add_formula(y ~ x)

  param <- parameters(mod) %>%
    update(cost_complexity = cost_complexity(c(0, 0.1)))
  grid <- grid_regular(param)

  res <- tune_grid(
    wfl, grid = grid,
    resamples = mc_cv(df, prop = 0.8, times = 5),
    metrics = metric_set(rmse)
    )
  collect_metrics(res)

  expect_equal(nrow(res), 5)
})
