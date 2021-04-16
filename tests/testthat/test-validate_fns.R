test_that("check that .validate_fns works", {
   env_curr <- environment()
   fn1 <- function(x) x; fn2 <- function(x) y
   expected_params <- list(fn1 = 'x', fn2 = 'y', fn3 = 'x')
   expect_error(analysispipeline:::.validate_fns(env = env_curr, expected_params = expected_params)) # error
   fn2 <- NULL
   expect_true(analysispipeline:::.validate_fns(env = env_curr, expected_params = expected_params)) # no error given if fn is NULL
})
