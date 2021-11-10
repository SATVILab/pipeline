test_that("calc_dir_proj works", {
  iter <- structure(
    list(
      var_exp = "Progressor",
      var_exp_spline = list(
        `tc~soma~C9` = list(fn = "ns", params = list(df = 2))
      ),
      var_int = list(
        c("Progressor", "C9")
      ),
      wins = "wins_n",
      var_conf = list(
        "none"
      ),
      ttb_max = NA_real_,
      var_dep = "risk6"
    ),
    row.names = c(
      NA,
      -1L
    ),
    class = c("tbl_df", "tbl", "data.frame")
  )
  dir_base <- file.path(tempdir())
  expect_type(
    .create_dir_proj(dir_base = dir_base),
    "character"
  )
  dir_out <- .create_dir_proj(dir_base = dir_base, iter = iter)
  unlink(dir_out, recursive = TRUE)
  dir_out <- .create_dir_proj(dir_base = dir_base, iter = iter)
  expect_type(
    dir_out,
    "character"
  )
  expect_identical(
    dir.exists(dir_out),
    TRUE
  )

})

test_that(".create_text_ref works", {
  expect_identical(
    .create_text_ref(c("a" = "b")),
    "a"
  )
  expect_identical(
    .create_text_ref(c("b")),
    "b"
  )
  expect_identical(
    .create_text_ref(c(1)),
    "1"
  )
  expect_identical(
    .create_text_ref(list("a" = function(x) x)),
    "a"
  )
  expect_error(
    .create_text_ref(function(x) x)
  )
  expect_error(
    .create_text_ref(list(function(x) x))
  )

  iter <- structure(
    list(
      var_exp = "Progressor",
      var_exp_spline = list(
        `tc~soma~C9` = list(fn = "ns", params = list(df = 2))
      ),
      var_int = list(
        c("Progressor", "C9")
      ),
      wins = "wins_n",
      var_conf = list(
        "none"
      ),
      ttb_max = NA_real_,
      var_dep = "risk6"
    ),
    row.names = c(
      NA,
      -1L
    ),
    class = c("tbl_df", "tbl", "data.frame")
  )
  expect_identical(
    .create_text_ref(iter$var_exp[[1]]),
    "Progressor"
  )
  expect_identical(
    .create_text_ref(iter$var_exp_spline),
    "tc~soma~C9"
  )
  expect_identical(
    .create_text_ref(iter$var_int),
    "ProgressorC9"
  )
  expect_identical(
    .create_text_ref(iter[[3]]),
    "ProgressorC9"
  )
  expect_identical(
    .create_text_ref(iter$var_int),
    "ProgressorC9"
  )
  expect_identical(
    .create_text_ref(iter$wins),
    "wins_n"
  )
  expect_identical(
    .create_text_ref(iter$var_conf),
    "none"
  )
  expect_identical(
    .create_text_ref(iter$ttb_max),
    "NA"
  )
  expect_identical(
    .create_text_ref(iter$var_dep),
    "risk6"
  )
  expect_error(
    .create_text_ref("a/c")
  )
  expect_error(
    .create_text_ref("a//c")
  )
  expect_error(
    .create_text_ref("a\\c")
  )
  expect_error(
    .create_text_ref("a\\\\c")
  )
  expect_error(
    .create_text_ref("a]\\n")
  )

  iter_list <- list(
    V1 = list(list("a"), list("b")),
    V2 = list(list("c" = 1, "d" = 2))
  )
  iter_tbl <- purrr::cross_df(iter_list)
  x <- .create_dir_proj(dir_base = tempdir(), iter = iter_list)
  expect_identical(
    x,
    file.path(tempdir(), "ab", "12") %>% fs::path_norm()
  )
  iter_tbl <- datautils::cross_df_safe(iter_list)
  x <- .create_dir_proj(dir_base = tempdir(), iter = iter_list)
  expect_identical(
    x,
    file.path(tempdir(), "ab", "12") %>% fs::path_norm()
  )
})