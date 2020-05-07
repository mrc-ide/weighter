test_that("Model weights are as expected", {

  pred <- data.frame(
    model = rep(LETTERS[1:2], times = 3),
    country = rep(c("c1", "c2", "c3"), each = 2),
    date = rep("2020-04-01", times = 6),
    error = c(2, 4, 5, 4, 3, 4)
  )
  pred$date <- as.Date(pred$date)
  ranked <- model_ranks(
    pred = pred,
    groupvars = c("country", "date"),
    errvar = "error"
  )
  weights <- model_weights(ranked)
  out <- c(2.5, 2)
  expect_true(all(weights$weight == out))
 }
)
