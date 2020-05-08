test_that("Model weights within a  group are as expected", {

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
  weights <- model_weights_in_group(ranked)
  out <- c(2.5, 2)
  expect_true(all(weights$weight == out))
 }
)


test_that("Model weights overall are as expected", {

  pred <- data.frame(
    model = rep(c("m1","m2", "m3"), times = 3),
    country = rep(c("c1", "c2", "c3"), each = 3),
    error = c(
      4L, 7L, 19L,
      17L, 5L, 18L,
      20L, 2L, 14L
    )
  )
  pred <- rbind(
    pred,
    data.frame(
      model = rep(c("m1", "m2"), times = 3),
      country = rep(c("c4", "c5", "c6"), each = 2),
      error = c(19L, 1L, 20L, 7L, 12L, 18L)
    )
  )

  weights <- model_weights(pred, "country", "error")
  right <- list(
    m1_m2_m3 = data.frame(
      model = c("m1", "m2", "m3"),
      weight = c(1.8, 2.5, 1.2),
      npreds = c(3, 3, 3),
      row.names = c("m1", "m2", "m3")
    ),
    m1_m2 = data.frame(
      model = c("m1", "m2"),
      weight = c(2.0, 2.5),
      npreds = c(3, 3),
      row.names = c("m1", "m2")
    )
  )

  expect_true(
    all(right$m1_m2_m3[["weight"]] == weights$m1_m2_m3[["weight"]])
  )

  expect_true(
    all(right$m1_m2[["weight"]] == weights$m1_m2[["weight"]])
  )

 }
)
