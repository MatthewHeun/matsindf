# Tests for matsindf_apply

###########################################################
context("Testing matsindf_apply")
###########################################################

example_fun <- function(a, b){
  return(list(c = sum_byname(a, b), d = difference_byname(a, b)))
}

test_that("matsindf_apply works as expected for single values", {
  expect_equal(example_fun(a = 2, b = 2), list(c = 4, d = 0))
  expect_equal(matsindf_apply(FUN = example_fun, a = 2, b = 2), list(c = 4, d = 0))
})

test_that("matsindf_apply works as expected for single matrices", {
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  expected_list <- list(c = a + b, d = a - b)
  expect_equal(example_fun(a, b), expected_list)
  expect_equal(matsindf_apply(FUN = example_fun, a = a, b = b), expected_list)
})

test_that("matsindf_apply works as expected for lists of single values", {
  expect_equal(example_fun(a = list(2, 2), b = list(2, 2)), list(c = list(4, 4), d = list(0, 0)))
  expect_equivalent(matsindf_apply(FUN = example_fun, a = list(2, 2, 2), b = list(2, 2, 2)),
                    data.frame(c = c(4, 4, 4), d = c(0, 0, 0)))
  expect_equivalent(matsindf_apply(FUN = example_fun, a = list(2, 2), b = list(1, 2)),
                    data.frame(c = c(3, 4), d = c(1, 0)))
})

test_that("matsindf_apply works as expected for lists of matrices", {
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  c <- a + b
  d <- a - b
  a <- list(a, a)
  b <- list(b, b)
  DF_expected <- data.frame(c = I(list(c, c)), d = I(list(d, d)))
  # Because DF_expected$c and DF_expected$d are created with I(list()), their class is "AsIs".
  # Need to set the class of DF_expected$c and DF_expected$d to NULL to get a match.
  attr(DF_expected$c, which = "class") <- NULL
  attr(DF_expected$d, which = "class") <- NULL
  expect_equal(matsindf_apply(FUN = example_fun, a = a, b = b), DF_expected)
})

test_that("matsindf_apply works as expected using .DF with single numbers", {
  DF <- data.frame(a = c(4, 4, 5), b = c(4, 4, 4))
  expect_equal(matsindf_apply(DF, FUN = example_fun, a = "a", b = "b"),
               data.frame(a = c(4, 4, 5), b = c(4, 4, 4), c = c(8, 8, 9), d = c(0, 0, 1)))
})

test_that("matsindf_apply works as expected using .DF with matrices", {
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  c <- a + b
  d <- a - b
  DF <- data.frame(a = I(list(a, a)), b = I(list(b,b)))
  result <- matsindf_apply(DF, FUN = example_fun, a = "a", b = "b")
  expected <- bind_cols(DF, data.frame(c = I(list(c, c)), d = I(list(d, d))))
  expect_equivalent(result, expected)
  # Try with piped .DF argument
  result <- DF %>% matsindf_apply(FUN = example_fun, a = "a", b = "b")
  expect_equivalent(result, expected)
})

test_that("matsindf_apply fails properly when not all same type for ...", {
  expect_error(matsindf_apply(FUN = example_fun, a = "a", b = 2), "unknown state in matsindf_apply")
})

test_that("matsindf_apply fails gracefully when some of ... are NULL", {
  expect_error(matsindf_apply(FUN = example_fun, a = 1, b = 2, c = NULL), "unused argument")
})

test_that("matsindf_apply fails as expected when .DF argument is missing from a data frame", {
  expect_error(matsindf_apply(FUN = example_fun, a = "a", b = "b"),
               "unknown state in matsindf_apply ... must be missing or all same type:")
})

test_that("matsindf_apply fails as expected when .DF argument is not a data frame or a list", {
  expect_error(matsindf_apply(.DF = "string", FUN = example_fun, a = "a", b = "b"),
               "unknown state in matsindf_apply ... must be missing or all same type: all single numbers, ")
})

test_that("matsindf_apply_types works as expected", {
  expect_equal(matsindf_apply_types(a = 1, b = 2),
               list(dots_present = TRUE, all_dots_num = TRUE, all_dots_mats = FALSE, all_dots_list = FALSE, all_dots_char = FALSE))
  expect_equal(matsindf_apply_types(a = matrix(c(1, 2)), b = matrix(c(2, 3)), c = matrix(c(3, 4))),
               list(dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = TRUE, all_dots_list = FALSE, all_dots_char = FALSE))
  expect_equal(matsindf_apply_types(a = list(1, 2), b = list(3, 4), c = list(5, 6)),
               list(dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = FALSE, all_dots_list = TRUE, all_dots_char = FALSE))
  expect_equal(matsindf_apply_types(a = "a", b = "b", c = "c"),
               list(dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = FALSE, all_dots_list = FALSE, all_dots_char = TRUE))
  expect_equal(matsindf_apply_types(),
               list(dots_present = FALSE, all_dots_num = FALSE, all_dots_mats = FALSE, all_dots_list = FALSE, all_dots_char = FALSE))
})

test_that("matsindf_apply works with a NULL argument", {
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  c <- a + b
  d <- a - b
  DF <- data.frame(a = I(list(a, a)), b = I(list(b,b)))
  # Here is where the NULL is given as an argument to matsindp_apply.
  result <- matsindf_apply(DF, FUN = example_fun, a = "a", b = "b", z = NULL)
  expected <- bind_cols(DF, data.frame(c = I(list(c, c)), d = I(list(d, d))))
  expect_equivalent(result, expected)
  # Try with piped .DF argument
  result <- DF %>% matsindf_apply(FUN = example_fun, a = "a", b = "b", z = NULL)
  expect_equivalent(result, expected)
})

test_that("matsindf_apply works when .DF is a list", {
  expect_equal(matsindf_apply(list(a = 1, b = 2), FUN = example_fun, a = "a", b = "b"),
               list(a = 1, b = 2, c = 3, d = -1))
})

test_that("matsindf_apply works when .DF supplies some or all argument names", {
  # All arguments are supplied by named items in .DF
  expect_equal(matsindf_apply(list(a = 1, b = 2), FUN = example_fun),
               list(a = 1, b = 2, c = 3, d = -1))
  # All arguments are supplied by named arguments in ...
  expect_equal(matsindf_apply(list(a = 1, b = 2), FUN = example_fun, a = "a", b = "b"),
               list(a = 1, b = 2, c = 3, d = -1))
  # All arguments are supplied by named arguments in ..., but mix them up.
  # Note that the named argumens override the items in .DF
  expect_equal(matsindf_apply(list(a = 1, b = 2, z = 10), FUN = example_fun, a = "z", b = "b"),
               list(a = 1, b = 2, z = 10, c = 12, d = 8))
  # Try when one of the output names is same as an input name
  expect_warning(matsindf_apply(list(a = 1, b = 2, c = 10), FUN = example_fun, a = "c", b = "b"),
                 "name collision in matsindf_apply: c") %>%
    expect_equal(c(list(a = 1, b = 2, c = 10), list(c = 12, d = 8)))
})
