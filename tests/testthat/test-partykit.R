
test_that("condition inference trees",{
  skip_if_not_installed("partykit")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages(library(partykit))

  expect_snapshot(
    decision_tree() %>% set_engine("partykit") %>% set_mode("regression")
  )
  expect_snapshot(
    decision_tree() %>% set_engine("partykit", teststat = "maximum") %>% set_mode("classification")
  )

  # ----------------------------------------------------------------------------
  # regression

  expect_error_free({
    ct_fit_1 <-
      decision_tree() %>%
      set_engine("partykit") %>%
      set_mode("regression") %>%
      fit(mpg ~ ., data = mtcars)
  })
  pk_fit_1 <- ctree(mpg ~ ., data = mtcars)
  expect_equal(pk_fit_1$fitted, ct_fit_1$fit$fitted)

  expect_error_free(ct_pred_1 <- predict(ct_fit_1, mtcars)$.pred)
  pk_pred_1 <- unname(predict(pk_fit_1, mtcars))
  expect_equal(pk_pred_1, ct_pred_1)

  expect_error_free({
    ct_fit_2 <-
      decision_tree(tree_depth = 1) %>%
      set_engine("partykit") %>%
      set_mode("regression") %>%
      fit(mpg ~ ., data = mtcars)
  })
  pk_fit_2 <- ctree(mpg ~ ., data = mtcars, control = ctree_control(maxdepth = 1))
  expect_equal(pk_fit_2$fitted, ct_fit_2$fit$fitted)

  expect_error_free(ct_pred_2 <- predict(ct_fit_2, mtcars)$.pred)
  pk_pred_2 <- unname(predict(pk_fit_2, mtcars))
  expect_equal(pk_pred_2, ct_pred_2)

  expect_error_free({
    ct_fit_3 <-
      decision_tree() %>%
      set_engine("partykit", mincriterion = .99) %>%
      set_mode("regression") %>%
      fit(mpg ~ ., data = mtcars)
  })
  pk_fit_3 <- ctree(mpg ~ ., data = mtcars, control = ctree_control(mincriterion = .99))
  expect_equal(pk_fit_3$fitted, ct_fit_3$fit$fitted)

  expect_error_free(ct_pred_3 <- predict(ct_fit_3, mtcars)$.pred)
  pk_pred_3 <- unname(predict(pk_fit_3, mtcars))
  expect_equal(pk_pred_3, ct_pred_3)

  # ----------------------------------------------------------------------------
  # classification

  data(ad_data, package = "modeldata")

  expect_error_free({
    ct_fit_4 <-
      decision_tree() %>%
      set_engine("partykit") %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = ad_data)
  })
  pk_fit_4 <- ctree(Class ~ ., data = ad_data)
  expect_equal(pk_fit_4$fitted, ct_fit_4$fit$fitted)

  expect_error_free(ct_pred_4 <- predict(ct_fit_4, ad_data)$.pred_class)
  pk_pred_4 <- unname(predict(pk_fit_4, ad_data))
  expect_equal(pk_pred_4, ct_pred_4)

  expect_error_free(ct_prob_4 <- predict(ct_fit_4, ad_data, type = "prob")[[2]])
  pk_prob_4 <- unname(predict(pk_fit_4, ad_data, type = "prob")[,2])
  expect_equal(pk_prob_4, ct_prob_4)

})



test_that("condition inference forests",{
  skip_if_not_installed("partykit")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages(library(partykit))

  expect_snapshot(
    rand_forest() %>% set_engine("partykit") %>% set_mode("regression")
  )
  expect_snapshot(
    rand_forest() %>% set_engine("partykit", teststat = "maximum") %>% set_mode("classification")
  )

  # ----------------------------------------------------------------------------
  # regression

  expect_error_free({
    set.seed(1)
    cf_fit_1 <-
      rand_forest(trees = 5) %>%
      set_engine("partykit") %>%
      set_mode("regression") %>%
      fit(mpg ~ ., data = mtcars)
  })
  set.seed(1)
  pk_fit_1 <- cforest(mpg ~ ., data = mtcars, ntree = 5)
  expect_equal(pk_fit_1$fitted, cf_fit_1$fit$fitted)

  expect_error_free(cf_pred_1 <- predict(cf_fit_1, mtcars)$.pred)
  pk_pred_1 <- unname(predict(pk_fit_1, mtcars))
  expect_equal(pk_pred_1, cf_pred_1)

  expect_error_free({
    set.seed(1)
    cf_fit_2 <-
      rand_forest(trees = 5, mtry = 2) %>%
      set_engine("partykit") %>%
      set_mode("regression") %>%
      fit(mpg ~ ., data = mtcars)
  })
  set.seed(1)
  pk_fit_2 <- cforest(mpg ~ ., data = mtcars, ntree = 5, control = ctree_control(mtry = 2))
  expect_equal(pk_fit_2$fitted, cf_fit_2$fit$fitted)

  # TODO unsure why this is close but not the same
  # expect_error_free(cf_pred_2 <- predict(cf_fit_2, mtcars)$.pred)
  # pk_pred_2 <- unname(predict(pk_fit_2, mtcars))
  # expect_equal(pk_pred_2, cf_pred_2)

  expect_error_free({
    set.seed(1)
    cf_fit_3 <-
      rand_forest(trees = 5) %>%
      set_engine("partykit", mincriterion = .99) %>%
      set_mode("regression") %>%
      fit(mpg ~ ., data = mtcars)
  })
  set.seed(1)
  pk_fit_3 <- cforest(mpg ~ ., data = mtcars,  ntree = 5, control = ctree_control(mincriterion = .99))
  expect_equal(pk_fit_3$fitted, cf_fit_3$fit$fitted)

  expect_error_free(cf_pred_3 <- predict(cf_fit_3, mtcars)$.pred)
  pk_pred_3 <- unname(predict(pk_fit_3, mtcars))
  expect_equal(pk_pred_3, cf_pred_3)

  # ----------------------------------------------------------------------------
  # classification

  data(ad_data, package = "modeldata")

  expect_error_free({
    set.seed(1)
    cf_fit_4 <-
      rand_forest(trees = 5) %>%
      set_engine("partykit") %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = ad_data)
  })
  set.seed(1)
  pk_fit_4 <- cforest(Class ~ ., data = ad_data,  ntree = 5)
  expect_equal(pk_fit_4$fitted, cf_fit_4$fit$fitted)

  expect_error_free(cf_pred_4 <- predict(cf_fit_4, ad_data)$.pred_class)
  pk_pred_4 <- unname(predict(pk_fit_4, ad_data))
  expect_equal(pk_pred_4, cf_pred_4)

  expect_error_free(cf_prob_4 <- predict(cf_fit_4, ad_data, type = "prob")[[2]])
  pk_prob_4 <- unname(predict(pk_fit_4, ad_data, type = "prob")[,2])
  expect_equal(pk_prob_4, cf_prob_4)

})


