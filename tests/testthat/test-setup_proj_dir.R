testthat::test_that(".setup_proj_dir works if dir_proj is a character", {

  dir_proj <- file.path(tempdir(), "test")
  .setup_proj_dir(dir_proj = dir_proj, dir_proj_empty = FALSE)
  expect_true(dir.exists(dir_proj))
  path_fn <- file.path(dir_proj, 'test.txt')
  file.create(path_fn)
  expect_true(file.exists(path_fn))
  .setup_proj_dir(dir_proj = dir_proj, dir_proj_empty = FALSE)
  expect_true(file.exists(path_fn))
  .setup_proj_dir(dir_proj = dir_proj, dir_proj_empty = TRUE)
  expect_false(file.exists(path_fn))
  dir_proj_norm <- suppressWarnings(normalizePath(dir_proj))
  dir_proj_out_of_fn <- .setup_proj_dir(dir_proj = dir_proj, dir_proj_empty = FALSE)
  expect_match(dir_proj_out_of_fn,
               dir_proj_norm, fixed = TRUE)
  unlink(dir_proj, recursive = TRUE)
})


testthat::test_that(".setup_proj_dir works if dir_proj is a function", {

  dir_proj <- function() 1
  expect_error(.setup_proj_dir(dir_proj = dir_proj))

  dir_proj <- function() file.path(tempdir(), "test")
  expect_error(.setup_proj_dir(dir_proj = dir_proj))

  dir_proj <- function(x) file.path(tempdir(), "test", x)
  dir_proj_out_of_fn <- .setup_proj_dir(dir_proj = dir_proj,
                                        dir_proj_empty = FALSE,
                                        p_dots = 1)
  expect_true(is.character(dir_proj_out_of_fn))
  expect_true(file.exists(dir_proj_out_of_fn))
  expect_match(dir_proj_out_of_fn,
               suppressWarnings(normalizePath(file.path(tempdir(), "test", 1))),
               fixed = TRUE)

  unlink(dir_proj_out_of_fn, recursive = TRUE)
})

