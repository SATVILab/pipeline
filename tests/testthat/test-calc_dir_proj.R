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
  dir_base <- file.path(tempdir(), "test_calc_dir_proj")
  if (!dir.exists(dir_base)) dir.create(dir_base, recursive = TRUE)
  if (dir.exists(dir_base)) unlink(dir_base, recursive = TRUE)
  temp_dir_len <- stringr::str_length(tempdir())
  expect_identical(
    stringr::str_sub(
      .create_dir_proj(
        dir_base = dir_base,
        iter = iter,
        var_to_encode = "~none~"
      ),
      start = temp_dir_len + 2
    ),
    "test_calc_dir_proj/Progressor/tc~soma~C9/ProgressorC9/wins_n/none/NA/risk6"
  )

  expect_identical(
    stringr::str_sub(
      .create_dir_proj(
        dir_base = dir_base,
        iter = iter,
        var_to_encode = c("var_exp_spline")
      ),
      start = temp_dir_len + 2
    ),
    "test_calc_dir_proj/Progressor/v1/ProgressorC9/wins_n/none/NA/risk6"
  )
  expect_identical(
    stringr::str_sub(
      .create_dir_proj(
        dir_base = dir_base,
        iter = iter,
        var_to_encode = c("var_exp_spline")
      ),
      start = temp_dir_len + 2
    ),
    "test_calc_dir_proj/Progressor/v1/ProgressorC9/wins_n/none/NA/risk6"
  )
  expect_identical(
    stringr::str_sub(
      .create_dir_proj(
        dir_base = dir_base,
        iter = iter,
        var_to_encode = c("var_exp_spline", "var_exp")
      ),
      start = temp_dir_len + 2
    ),
    "test_calc_dir_proj/v1/v1/ProgressorC9/wins_n/none/NA/risk6"
  )
  expect_identical(
    stringr::str_sub(
      .create_dir_proj(
        dir_base = dir_base,
        iter = iter %>%
          dplyr::mutate(var_exp = "fairy_count") %>%
          dplyr::select(var_exp, var_exp_spline),
        var_to_encode = c("var_exp_spline", "var_exp")
      ),
      start = temp_dir_len + 2
    ),
    "test_calc_dir_proj/v2/v1"
  )
  expect_identical(
    stringr::str_sub(
      .create_dir_proj(
        dir_base = dir_base,
        iter = iter %>%
          dplyr::mutate(var_exp = "fairy_count") %>%
          dplyr::select(var_exp, var_exp_spline),
        var_to_encode = c("var_exp_spline", "var_exp")
      ),
      start = temp_dir_len + 2
    ),
    "test_calc_dir_proj/v2/v1"
  )

  expect_identical(
    stringr::str_sub(
      .create_dir_proj(
        dir_base = dir_base,
        iter = iter %>%
          dplyr::select(var_exp, var_exp_spline),
        var_to_encode = c("var_exp_spline", "var_exp")
      ),
      start = temp_dir_len + 2
    ),
    "test_calc_dir_proj/v1/v1"
  )
  expect_identical(
    stringr::str_sub(
      .create_dir_proj(
        dir_base = dir_base,
        iter = iter %>%
          dplyr::select(var_exp, var_exp_spline) %>%
          dplyr::mutate(
            var_exp_spline = "~~~",
            var_exp = "1"
          ),
        var_to_encode = c("var_exp_spline", "var_exp")
      ),
      start = temp_dir_len + 2
    ),
    "test_calc_dir_proj/v3/v2"
  )

  var_to_encode <- "var_exp_spline"
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

  if (dir.exists(dir_base)) unlink(dir_base, recursive = TRUE)
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
  expect_identical(
    suppressWarnings(.create_text_ref("a/c")),
    "ac"
  )
  expect_identical(
    suppressWarnings(.create_text_ref("a//c")),
    "ac"
  )
  expect_identical(
    suppressWarnings(.create_text_ref("a\\c")),
    "ac"
  )
  expect_identical(
    suppressWarnings(.create_text_ref("a\\c\\")),
    "ac"
  )
  expect_identical(
    suppressWarnings(.create_text_ref("a\\\\c")),
    "ac"
  )
  expect_identical(
    suppressWarnings(.create_text_ref("a]\\n")),
    "a]"
  )
  expect_warning(
    .create_text_ref("a]\\n")
  )
  expect_warning(
    .create_text_ref("\\\\")
  )
  expect_warning(
    .create_text_ref("/")
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
