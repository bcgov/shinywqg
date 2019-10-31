context("internal-utils")

test_that("set_class", {
  x <- list()
  expect_identical(class(x), "list")
  x <- set_class(x, "new_class")
  expect_identical(class(x), "new_class")
})

test_that("set_names", {
  x <- 1
  expect_null(names(x))
  x <- set_names(x, "a")
  expect_identical(names(x), "a")
})

test_that("is_try_error", {
  expect_false(is_try_error(1))
  expect_false(is_try_error(identity(1)))
  expect_true(is_try_error(try(stop(), silent = TRUE)))
})

test_that("sys_time", {
  expect_is(sys_time(), "POSIXct")
  expect_identical(attr(sys_time(), "tzone"), "UTC")
})

test_that("remove_nulls", {
  # vector
  expect_identical(remove_nulls(integer(0)), integer(0))
  expect_identical(remove_nulls(1), 1)
  expect_identical(remove_nulls(NA), NA)
  expect_identical(remove_nulls(c(2, NA)), c(2, NA))

  # list
  expect_identical(remove_nulls(list()), list())
  expect_identical(remove_nulls(list(1)), list(1))
  expect_identical(remove_nulls(list(NA)), list(NA))
  expect_identical(remove_nulls(list(2, NA)), list(2, NA))
  expect_identical(remove_nulls(list(2, NA, NULL)), list(2, NA))
  expect_identical(remove_nulls(list(NULL)), list())
  expect_identical(remove_nulls(list(x = 1, y = NULL)), list(x = 1))
})

test_that("str_extract", {
  expect_error(
    str_extract(character(0), character(0)),
    "^invalid 'pattern' argument$"
  )

  expect_identical(str_extract(character(0), ".*"), character(0))
  expect_identical(str_extract(NA_character_, ".*"), character(0))
  expect_identical(str_extract("a", ".*"), "a")
  expect_identical(str_extract(c("a", NA), ".*"), "a")
  expect_identical(str_extract(c("a", "b"), ".*"), c("a", "b"))
  expect_identical(str_extract(c("aa a", "bbb"), "\\w"), c("a", "b"))
  expect_identical(str_extract(c("aa a", "bbb"), "\\w\\w"), c("aa", "bb"))
  expect_identical(str_extract(c("aa a", "bbb"), "\\w+"), c("aa", "bbb"))
  expect_identical(str_extract(c("aa a", "bbb"), "\\s\\w+"), " a")
})

test_that("str_extract_all", {
  expect_error(
    str_extract_all(character(0), character(0)),
    "^invalid 'pattern' argument$"
  )

  expect_identical(str_extract_all(character(0), ".*"), list())
  expect_identical(str_extract_all(NA_character_, ".*"), list(character(0)))
  expect_identical(str_extract_all("a", ".*"), list("a"))
  expect_identical(str_extract_all(c("a", NA), ".*"), list("a", character(0)))
  expect_identical(str_extract_all(c("a", "b"), ".*"), list("a", "b"))
  expect_identical(str_extract_all(c("aa a", "bbb"), "\\w"), list(c("a", "a", "a"), c("b", "b", "b")))
  expect_identical(str_extract_all(c("aa a", "bbb"), "\\w\\w"), list("aa", "bb"))
  expect_identical(str_extract_all(c("aa a", "bbb"), "\\w+"), list(c("aa", "a"), "bbb"))
  expect_identical(str_extract_all(c("aa a", "bbb"), "\\s\\w+"), list(" a", character(0)))
})

test_that("tibble", {
  expect_identical(
    tibble(data.frame()),
    structure(list(), .Names = character(0), class = c(
      "tbl_df",
      "tbl", "data.frame"
    ), row.names = integer(0))
  )

  expect_identical(
    tibble(data.frame(x = 1)),
    structure(list(x = 1), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L))
  )

  expect_identical(
    tibble(x = 1),
    structure(list(x = 1), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L))
  )

  expect_identical(
    tibble(x = "1"),
    structure(list(x = "1"), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L))
  )
})

test_that("%||%", {
  expect_identical(1 %||% 2, 1)
  expect_identical(NULL %||% 2, 2)
  expect_identical(integer(0) %||% 2, 2)
})

test_that("rinteger", {
  set.seed(-4)
  expect_identical(rinteger(), -486688331L)
  set.seed(-4)
  expect_identical(rinteger(), -486688331L)
  set.seed(3)
  expect_identical(rinteger(), -1425750787L)
  set.seed(4)
  expect_identical(rinteger(), 368509503L)
  set.seed(-4)
  expect_identical(rinteger(), -486688331L)
  set.seed(-4)
  expect_identical(rinteger(3), c(-486688331L, -1758167562L, 645423485L))
  expect_true(all(!is.na(rinteger(10^6))))
})

test_that("last", {
  x <- 1:2
  expect_identical(last(x), 2L)
})

test_that("sum2intswrap", {
  mx <- 2147483647L
  expect_identical(sum2intswrap(0, 1), 1L)
  expect_identical(sum2intswrap(0, -1), -1L)

  expect_identical(sum2intswrap(mx, 0L), mx)
  expect_identical(sum2intswrap(-mx, 0L), -mx)

  expect_identical(sum2intswrap(mx, -1L), mx - 1L)
  expect_identical(sum2intswrap(-mx, 1L), -mx + 1L)

  expect_identical(sum2intswrap(mx, -mx), 0L)
  expect_identical(sum2intswrap(-mx, mx), 0L)

  expect_identical(sum2intswrap(mx, mx), 0L)
  expect_identical(sum2intswrap(-mx, -mx), 0L)

  expect_identical(sum2intswrap(mx, 1L), -2147483646L)
  expect_identical(sum2intswrap(mx, 2L), -2147483645L)
  expect_identical(sum2intswrap(-mx, -1L), 2147483646L)
  expect_identical(sum2intswrap(-mx, -2L), 2147483645L)

  expect_identical(sum2intswrap(-mx, NA), NA_integer_)

  expect_identical(sum2intswrap(mx, -1:1), c(2147483646L, 2147483647L, -2147483646L))
  expect_identical(sum2intswrap(-mx, -1:1), c(2147483646L, -2147483647L, -2147483646L))
  expect_identical(sum2intswrap(mx, c(-1:1, NA)), c(2147483646L, 2147483647L, -2147483646L, NA))
  expect_identical(sum2intswrap(-mx, c(-1:1, NA)), c(2147483646L, -2147483647L, -2147483646L, NA))
})
