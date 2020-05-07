test_that("Model are ranked correctly", {

  ## With one groupvar
  pred <- data.frame(
    model = rep(LETTERS[1:2], each = 3),
    country = rep(c("c1", "c2", "c3"), 2),
    error = c(1,2, 3, 2, 1, 3.5)
  )
  ## Expected ranks when grouped by country
  out <- c(1, 2, 2, 1, 1, 2)
  ranked <- model_ranks(
    pred = pred,
    groupvars = "country",
    errvar = "error"
  )
  expect_true(all(ranked$rank == out))

  ## With mutiple group vars
  pred <- data.frame(
    model = rep(LETTERS[1:3], times = 2),
    country = rep(c("c1", "c2"), each = 3),
    date = rep(c("2020-04-01", "2020-04-02"), each = 3),
    error = c(1,2, 3, 3.5, 1, 2)
  )
  pred$date <- as.Date(pred$date)
  ranked <- model_ranks(
    pred = pred,
    groupvars = c("country", "date"),
    errvar = "error"
  )
  out <- c(1, 2, 3, 3, 1, 2)
  expect_true(all(ranked$rank == out))

})
