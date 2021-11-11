test_that("save_objects works", {
  dir_proj <- tempdir()

  # test that saving using ... works
  x <- 1
  y <- 2
  save_objects(x = x, y = y, dir_proj = dir_proj, dir_sub = "test")
  expect_true(file.exists(file.path(dir_proj, "test", "x.rds")))
  expect_true(file.exists(file.path(dir_proj, "test", "y.rds")))
  expect_identical(
    readRDS(file.path(dir_proj, "test", "x.rds")),
    x
  )
  expect_identical(
    readRDS(file.path(dir_proj, "test", "y.rds")),
    y
  )

  # test that emptying the directory works
  z <- 3
  save_objects(
    z = 2, dir_proj = dir_proj, dir_sub = "test",
    empty = TRUE
  )
  expect_true(file.exists(file.path(dir_proj, "test", "z.rds")))
  expect_true(!file.exists(file.path(dir_proj, "test", "x.rds")))

  # test that saving using obj_list and ... works
  x <- 1
  obj_list <- list("y" = 4)
  save_objects(
    x = x, obj_list = obj_list,
    dir_proj = dir_proj, dir_sub = "test"
  )
  expect_true(file.exists(file.path(dir_proj, "test", "x.rds")))
  expect_true(file.exists(file.path(dir_proj, "test", "y.rds")))
  expect_identical(
    readRDS(file.path(dir_proj, "test", "x.rds")),
    x
  )
  expect_identical(
    readRDS(file.path(dir_proj, "test", "y.rds")),
    4
  )

  # test that not emptying the directory works
  v <- 1
  save_objects(
    v = v, dir_proj = dir_proj, dir_sub = "test",
    empty = FALSE, silent = TRUE
  )
  expect_true(file.exists(file.path(dir_proj, "test", "x.rds")))
  expect_true(file.exists(file.path(dir_proj, "test", "y.rds")))
  expect_true(file.exists(file.path(dir_proj, "test", "v.rds")))

  # test that saving without anything named works
  v <- 1
  obj_list <- list(2)
  save_objects(v,
    obj_list = obj_list, dir_proj = dir_proj,
    dir_sub = "test", silent = TRUE
  )
  expect_true(file.exists(file.path(dir_proj, "test", "obj_1.rds")))
  expect_true(file.exists(file.path(dir_proj, "test", "obj_2.rds")))
  expect_true(!file.exists(file.path(dir_proj, "test", "v.rds")))

  # test that you can save without using dots
  obj_list <- list(z = 3)
  save_objects(
    obj_list = obj_list, dir_proj = dir_proj,
    dir_sub = "test", empty = TRUE
  )
  expect_true(file.exists(file.path(dir_proj, "test", "z.rds")))
  expect_true(!file.exists(file.path(dir_proj, "test", "v.rds")))

  # test that it runs successfully when not saving anything
  expect_message(save_objects())

  # test that it gives warnings when supplying names when
  # there are no names
  expect_warning(save_objects(1, dir_proj = dir_proj))
  expect_warning(save_objects(obj_list = list(1), dir_proj = dir_proj))
  expect_warning(save_objects(obj_list = list(1, z = 1), dir_proj = dir_proj))

  # test that it works when some objects do not have unique names
  expect_warning(save_objects(
    x = 1, x = 2, dir_proj = dir_proj,
    dir_sub = "test"
  ))
  expect_true(file.exists(file.path(
    dir_proj, "test",
    "x-uni_1.rds"
  )))
  expect_true(file.exists(file.path(
    dir_proj, "test",
    "x-uni_2.rds"
  )))
  expect_true(!file.exists(file.path(
    dir_proj, "test",
    "x-uni_3.rds"
  )))
  expect_identical(
    readRDS(file.path(
      dir_proj, "test",
      "x-uni_1.rds"
    )),
    1
  )
  expect_identical(
    readRDS(file.path(
      dir_proj, "test",
      "x-uni_2.rds"
    )),
    2
  )

  # test that it works when some objects do not have unique names
  expect_warning(save_objects(
    x = 1, obj_list = list(x = 3),
    dir_proj = dir_proj,
    dir_sub = "test"
  ))
  expect_true(file.exists(file.path(
    dir_proj, "test",
    "x-uni_1.rds"
  )))
  expect_true(file.exists(file.path(
    dir_proj, "test",
    "x-uni_2.rds"
  )))
  expect_true(!file.exists(file.path(
    dir_proj, "test",
    "x-uni_3.rds"
  )))
  expect_identical(
    readRDS(file.path(
      dir_proj, "test",
      "x-uni_1.rds"
    )),
    1
  )
  expect_identical(
    readRDS(file.path(
      dir_proj, "test",
      "x-uni_2.rds"
    )),
    3
  )

  # test that specifying multiple sub-directories works
  save_objects(
    x = 1, dir_proj = dir_proj,
    dir_sub = c("test", "test_sub"), silent = TRUE
  )
  expect_true(file.exists(file.path(dir_proj, "test", "test_sub", "x.rds")))

  # test that specifying multiple sub-directories works,
  # with the sub-directories specified as a list
  save_objects(
    x = 1, dir_proj = dir_proj,
    dir_sub = list("test", "test_sub_2"), silent = TRUE
  )
  expect_true(file.exists(file.path(dir_proj, "test", "test_sub_2", "x.rds")))


  # ==========================
  # Saving plots
  # ==========================

  p <- ggplot2::ggplot()

  # test if saving named plots works
  save_objects(
    p = p, x = 1, dir_proj = dir_proj,
    dir_sub = c("test"), silent = TRUE
  )
  expect_true(file.exists(file.path(
    dir_proj, "test",
    "p.png"
  )))
  expect_true(file.exists(file.path(
    dir_proj, "test",
    "x.rds"
  )))

  # test if saving unnamed plots works
  save_objects(p,
    x = 1, dir_proj = dir_proj,
    dir_sub = c("test"), silent = TRUE
  )
  expect_true(file.exists(file.path(
    dir_proj, "test",
    "obj_1.png"
  )))
  expect_true(file.exists(file.path(
    dir_proj, "test",
    "x.rds"
  )))

  # test if function behaves as it should
  # when obj_list is passed a ggplot2 object
  # not wrapped in a list
  suppressWarnings(save_objects(
    obj_list = p, dir_proj = dir_proj,
    dir_sub = c("test"), silent = FALSE
  ))
  expect_true(file.exists(file.path(
    dir_proj, "test",
    "obj_1.png"
  )))
  expect_true(!file.exists(file.path(
    dir_proj, "test",
    "x.rds"
  )))
  expect_true(!file.exists(file.path(
    dir_proj, "test",
    "data.rds"
  )))

  # test if passing a ggplot2 object to obj_list works
  unlink(file.path(dir_proj, "test"), recursive = TRUE)
  unlink(file.path(dir_proj, "test", "test_sub"), recursive = TRUE)
  unlink(file.path(dir_proj, "test", "test_sub_2"), recursive = TRUE)
})
