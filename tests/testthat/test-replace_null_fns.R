test_that(".replace_null_fns works", {
  env_curr <- environment()
  fn1 <- function(x) x
  fn2 <- function(x = 2) y
  fn3 <- NULL
  expected_params <- list(fn1 = "x", fn2 = "y", fn3 = "x")
  analysispipeline:::.replace_null_fns(env = env_curr, expected_params = expected_params)
  expect_identical(formals(fn3), pairlist(x = NULL))
  expect_identical(names(formals(fn2)), "x")
})
