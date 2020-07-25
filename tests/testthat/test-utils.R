test_that("categorical features are identified in data.frames and tibbles",{
  test_frame <- data.frame(
    num1 = 1:10,
    cat1 = letters[1:10],
    num2 = 10:10,
    cat2 = letters[10:1],
    stringsAsFactors = TRUE
  )
  test_frame2 <- tibble::as_tibble(test_frame)
  expect_equal(
    categorical_columns(test_frame),
    c(2,4)
  )
  expect_equal(
    categorical_columns(test_frame2),
    c(2,4)
  )
})

test_that("categorical features are replaced with integers",{
  test_frame <- data.frame(
    num1 = 1:10,
    cat1 = letters[1:10],
    num2 = 10:10,
    cat2 = letters[10:1],
    stringsAsFactors = TRUE
  )
  expected <- data.frame(
    num1 = 1:10,
    cat1 = 0:9,
    num2 = 10:10,
    cat2 = 9:0
  )
  expect_equal(
    categorical_features_to_int(test_frame, c(2,4)),
    expected
  )
})
