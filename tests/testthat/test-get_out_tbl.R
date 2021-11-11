test_that(".get_out_tbl works", {
  dir_proj <- file.path(tempdir(), "test_get_out_tbl")
  for (x in c("a", "b", "c")) {
    dir_curr <- file.path(dir_proj, x)
    if (dir.exists(dir_curr)) unlink(dir_curr, recursive = TRUE)
    dir.create(dir_curr, recursive = TRUE)
    for (i in 1:4) {
      file.create(file.path(dir_curr, paste0(i, ".rds")))
      file.create(file.path(dir_curr, paste0(i, ".png")))
    }
  }
  iter_list <- list(x = 1, y = list(list("a", "b", "c")))
  out_tbl <- .get_out_tbl(
    dir_proj = dir_proj,
    dir_sub = NULL,
    iter = NULL
  )
  expect_identical(
    class(out_tbl),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_identical(
    nrow(out_tbl),
    1L
  )
  expect_length(
    out_tbl$fn[[1]],
    24L
  )

  out_tbl <- .get_out_tbl(
    dir_proj = dir_proj,
    dir_sub = c("a", "b"),
    iter = NULL
  )
  expect_length(
    out_tbl$fn[[1]],
    16L
  )
  out_tbl <- .get_out_tbl(
    dir_proj = dir_proj,
    dir_sub = c("a", "b"),
    iter = iter_list
  )
  expect_identical(
    nrow(out_tbl),
    1L
  )
  expect_identical(
    colnames(out_tbl),
    c("path_proj", "fn", "x", "y")
  )

  iter_tbl <- tibble::tibble(x = 1, y = list(list("a", "b", "c")))
  out_tbl <- .get_out_tbl(
    dir_proj = dir_proj,
    dir_sub = NULL,
    iter = iter_tbl
  )
  expect_identical(
    class(out_tbl),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_identical(
    nrow(out_tbl),
    1L
  )
})
