test_that("get_var works", {
  expect_identical(
    get_var(dep = "x")
  )
})

test_that(".fortify_var works", {
  expect_identical(
    .fortify_var("x"),
    list(
      list(
        "cn_orig" = "x", "cn_new" = "x",
        label = "x", "trans" = NULL, expansion = NULL
      )
    )
  )
  expect_identical(
    .fortify_var(list(list("cn_orig" = "x"))),
    list(
      list(
        "cn_orig" = "x", "cn_new" = "x",
        label = "x", "trans" = NULL, expansion = NULL
      )
    )
  )
  # don't allow lists whose elements aren't lists
  expect_error(
    .fortify_var(list("cn_orig" = "x"))
  )
  expect_identical(
    .fortify_var(list(list(
      "cn_orig" = "x",
      expansion = list("pkg" = "splines", "fn" = "ns", args = list(df = 2))
      ))),
    list(
      list(
        "cn_orig" = "x", "cn_new" = "x",
        label = "x", "trans" = NULL, expansion =  list(
          "pkg" = "splines", "fn" = "ns", args = list(df = 2)
          )
      )
    )
  )
  expect_identical(
    .fortify_var(list(list(
      "cn_orig" = "x",
      trans = function(x) x^2
      ))),
    list(
      list(
        "cn_orig" = "x", "cn_new" = "x",
        label = "x", "trans" = function(x) x^2,
        expansion = NULL
      )
    )
  )
  expect_identical(
    .fortify_var(list(list(
      "cn_orig" = "x",
      "cn_new" = "X",
      "label" = "CAPITAL X"
      ))),
    list(
      list(
        "cn_orig" = "x", "cn_new" = "X",
        label = "CAPITAL X", trans = NULL, expansion = NULL
      )
    )
  )
  expect_error(.fortify_var(list(list(cn = "x"))))
})