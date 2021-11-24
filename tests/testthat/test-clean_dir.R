test_that("clean_dir works", {
  dir_base <- file.path(tempdir(), "test_clean")

  dir_vec <- file.path(dir_base, c("a", c("c", "b")))
  dir_vec <- file.path(dir_vec, c("a", "b", "c", "d"))
  fn_vec <- NULL
  for (x in dir_vec) {
    if (!dir.exists(x)) dir.create(x, recursive = TRUE)
    file.create(file.path(x, "test.txt"))
    fn_vec <- c(fn_vec, file.path(x, "test.txt"))
  }
  set_up_dir <- function() {
    if (dir.exists(dir_base)) {
      unlink(dir_base, recursive = TRUE)
    }

    dir_vec <- file.path(dir_base, c("a", c("c", "b")))
    dir_vec <- file.path(dir_vec, c("a", "b", "c", "d"))
    fn_vec <- NULL
    for (x in dir_vec) {
      if (!dir.exists(x)) dir.create(x, recursive = TRUE)
      file.create(file.path(x, "test.txt"))
      fn_vec <- c(fn_vec, file.path(x, "test.txt"))
    }
    invisible(TRUE)
  }
  extra_file <- file.path(dir_base, "a", "z", "test2.txt")
  fn_vec_2 <- c(fn_vec, extra_file)
  set_up_dir_2 <- function() {
    set_up_dir()
    if (!dir.exists(dirname(extra_file))) {
      dir.create(dirname(extra_file), recursive = TRUE)
    }
    file.create(extra_file)
    invisible(TRUE)
  }

  extra_file_2 <- file.path(dir_base, "c", "test2.txt")
  fn_vec_3 <- c(fn_vec_2, extra_file_2)
  set_up_dir_3 <- function() {
    set_up_dir_2()
    if (!dir.exists(dirname(extra_file_2))) {
      dir.create(dirname(extra_file_2), recursive = TRUE)
    }
    if (!file.exists(extra_file_2)) {
      file.create(extra_file_2)
    }
    invisible(TRUE)
  }

  # test simply retaining certain files
  set_up_dir()
  clean_dir(
    clean_dir = dir_base,
    keep_fn = fn_vec[2]
  )
  expect_identical(
    list.files(dir_base, recursive = TRUE),
    "c/b/test.txt"
  )
  expect_identical(
    length(list.dirs(dir_base)),
    3L
  )
  expect_identical(
    file.exists(fn_vec),
    c(FALSE, TRUE, FALSE, FALSE)
  )

  # test retaining certain directories
  set_up_dir()

  clean_dir(
    clean_dir = dir_base,
    keep_dir_sub = dirname(fn_vec[2])
  )

  expect_identical(
    list.files(dir_base, recursive = TRUE),
    c("c/b/test.txt")
  )

  expect_identical(
    length(list.dirs(dir_base)),
    3L
  )
  expect_identical(
    file.exists(fn_vec),
    c(FALSE, TRUE, FALSE, FALSE)
  )

  # test if we correctly keep test2.txt

  set_up_dir_2()

  clean_dir(
    clean_dir = dir_base,
    keep_fn = fn_vec[2],
    keep_base_name = "test2.txt"
  )
  expect_identical(
    list.files(dir_base, recursive = TRUE),
    c("a/z/test2.txt", "c/b/test.txt")
  )
  expect_identical(
    length(list.dirs(dir_base)),
    5L
  )
  expect_identical(
    file.exists(fn_vec_2),
    c(FALSE, TRUE, FALSE, FALSE, TRUE)
  )

  # test if we correctly exclude a/z/test2.txt
  set_up_dir_2()

  clean_dir(
    clean_dir = dir_base,
    keep_fn = fn_vec[2],
    keep_base_name = "test2.txt",
    keep_base_name_strict = TRUE
  )
  expect_identical(
    list.files(dir_base, recursive = TRUE),
    c("c/b/test.txt")
  )
  expect_identical(
    length(list.dirs(dir_base)),
    3L
  )
  expect_identical(
    file.exists(fn_vec_2),
    c(FALSE, TRUE, FALSE, FALSE, FALSE)
  )

  # test if we correctly keep c/test2.txt
  set_up_dir_3()

  clean_dir(
    clean_dir = dir_base,
    keep_fn = fn_vec[2],
    keep_dir_sub = NULL,
    keep_base_name = "test2.txt",
    keep_base_name_strict = TRUE
  )
  expect_identical(
    list.files(dir_base, recursive = TRUE),
    c("c/b/test.txt", "c/test2.txt")
  )
  expect_identical(
    length(list.dirs(dir_base)),
    3L
  )
  expect_identical(
    file.exists(fn_vec_3),
    c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)
  )

})
