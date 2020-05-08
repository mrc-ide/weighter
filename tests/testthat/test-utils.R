test_that("Mapping models to groupvars and vice versa works", {

  pred <- data.frame(
    model = c(
      "m1", "m2", "m3",
      "m1", "m2", "m3",
      "m1", "m2", "m3", "m4",
      "m1", "m2", "m3", "m4"
    ),
    country = c(
      "c1", "c1", "c1",
      "c2", "c2", "c2",
      "c3", "c3", "c3", "c3",
      "c4", "c4", "c4", "c4"
    )
  )

  out <- groupvar_to_model(pred, "country")
  right <- list(
    `m1_m2_m3` = c("c1", "c2"),
    `m1_m2_m3_m4` = c("c3", "c4")
  )
  expect_true(all(names(out) %in% names(right)))
  same <- TRUE
  for (var in names(out)) {
    same <- same &
      all(right[[var]] == out[[var]])
  }
  expect_true(same)

  out <- model_to_groupvar(pred, "country")
  right <- list(
    m1 = c("c1", "c2", "c3", "c4"),
    m2 = c("c1", "c2", "c3", "c4"),
    m3 = c("c1", "c2", "c3", "c4"),
    m4 = c("c3", "c4")
  )

  expect_true(all(names(out) %in% names(right)))
  same <- TRUE
  for (var in names(out)) {
    same <- same &
      all(right[[var]] == out[[var]])
  }
  expect_true(same)
 }
)
