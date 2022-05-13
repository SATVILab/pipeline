test_that("get_var works", {
  expect_identical(
    get_var(list("x1", "x2")),
    get_var(c("x1", "x2")),
  )
  expect_identical(
    get_var(list("x1", "x2"), exp = c("a", "b")),
    get_var(list("x1", "x2"), exp = list(c("a", "b")))
  )
  
  expect_identical(
    get_var(list("x1", "x2"), exp = list(list("a"), list("b"))),
    get_var(list("x1", "x2"), exp = list("a", "b"))
  )
  expect_identical(
    nrow(get_var(list("x1", "x2"), exp = list(list("a"), list("b")))),
    4L
  )
  # test interaction
  expect_identical(
    get_var(
      list("x1", "x2"),
      exp = c("a", "b"),
      interaction = c("a", "b")
      )$interaction, # nolint
    list(list(c("a", "b")), list(c("a", "b")))
  )
  expect_identical(
    get_var(
      list("x1", "x2"),
      exp = c("a", "b"),
      interaction = NULL
      )$interaction, # nolint
    lapply(1:2, function(x) list(NULL))
  )
  expect_identical(
    get_var(
      list("x1", "x2"),
      exp = c("c"),
      interaction = NULL
      )$interaction, # nolint
    lapply(1:2, function(x) list(NULL))
  )
})