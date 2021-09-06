# Tests for matsindf_apply

library(dplyr)
library(matsbyname)
library(testthat)


example_fun <- function(a, b){
  return(list(c = sum_byname(a, b), d = difference_byname(a, b)))
}


test_that("matsindf_apply() fails with an unexpected argument", {
  expect_error(matsindf_apply(.dat = "a string", FUN = example_fun, a = 2, b = 2),
               ".dat must be a data frame or a list in matsindf_apply, was character")
})


test_that("matsindf_apply() works as expected for single values", {
  expect_equal(example_fun(a = 2, b = 2), list(c = 4, d = 0))
  expect_equal(matsindf_apply(FUN = example_fun, a = 2, b = 2), list(c = 4, d = 0))
})


test_that("matsindf_apply() works as expected for single matrices", {
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  expected_list <- list(c = a + b, d = a - b)
  expect_equal(example_fun(a, b), expected_list)
  expect_equal(matsindf_apply(FUN = example_fun, a = a, b = b), expected_list)
})


test_that("matsindf_apply() works as expected for lists of single values", {
  expect_equal(example_fun(a = list(2, 2), b = list(2, 2)), list(c = list(4, 4), d = list(0, 0)))
  expect_equivalent(matsindf_apply(FUN = example_fun, a = list(2, 2, 2), b = list(2, 2, 2)),
                    data.frame(c = c(4, 4, 4), d = c(0, 0, 0), stringsAsFactors = FALSE))
  expect_equivalent(matsindf_apply(FUN = example_fun, a = list(2, 2), b = list(1, 2)),
                    data.frame(c = c(3, 4), d = c(1, 0), stringsAsFactors = FALSE))
})


test_that("matsindf_apply() works as expected for lists of matrices", {
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  c <- a + b
  d <- a - b
  a <- list(a, a)
  b <- list(b, b)
  DF_expected <- data.frame(c = I(list(c, c)), d = I(list(d, d)), stringsAsFactors = FALSE)
  # Because DF_expected$c and DF_expected$d are created with I(list()), their class is "AsIs".
  # Need to set the class of DF_expected$c and DF_expected$d to NULL to get a match.
  attr(DF_expected$c, which = "class") <- NULL
  attr(DF_expected$d, which = "class") <- NULL
  expect_equal(matsindf_apply(FUN = example_fun, a = a, b = b), DF_expected)
})


test_that("matsindf_apply() works as expected using .DF with single numbers", {
  DF <- tibble::tibble(a = c(4, 4, 5), b = c(4, 4, 4))
  expect_equal(matsindf_apply(DF, FUN = example_fun, a = "a", b = "b"),
               tibble::tibble(a = c(4, 4, 5), b = c(4, 4, 4), c = c(8, 8, 9), d = c(0, 0, 1)))
})


test_that("matsindf_apply() works as expected using .DF with matrices", {
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  c <- a + b
  d <- a - b
  DF <- data.frame(a = I(list(a, a)), b = I(list(b,b)), stringsAsFactors = FALSE)
  result <- matsindf_apply(DF, FUN = example_fun, a = "a", b = "b")
  expected <- dplyr::bind_cols(DF, data.frame(c = I(list(c, c)), d = I(list(d, d)), stringsAsFactors = FALSE))
  expect_equivalent(result, expected)
  # Try with piped .DF argument
  result <- DF %>% matsindf_apply(FUN = example_fun, a = "a", b = "b")
  expect_equivalent(result, expected)
})


test_that("matsindf_apply() fails as expected when not all same type for ...", {
  expect_error(matsindf_apply(FUN = example_fun, a = "a", b = 2), 'argument "a" is missing, with no default')
})


test_that("matsindf_apply() fails as expected when wrong type of data is sent in ...", {
  expect_error(matsindf_apply(FUN = example_fun, a = list("a"), b = list(2)), "non-numeric argument to binary operator")
})


test_that("matsindf_apply() fails gracefully when some of ... are NULL", {
  expect_error(matsindf_apply(FUN = example_fun, a = 1, b = 2, c = NULL), "unused argument")
})


test_that("matsindf_apply() fails as expected when .DF argument is missing from a data frame", {
  expect_error(matsindf_apply(FUN = example_fun, a = "a", b = "b"),
               ".dat was missing and all arguments were strings")
})


test_that("matsindf_apply() fails as expected when .DF argument is not a data frame or a list", {
  expect_error(matsindf_apply(.DF = "string", FUN = example_fun, a = "a", b = "b"),
               ".dat was missing and all arguments were strings")
})


test_that("matsindf_apply_types() works as expected", {
  expect_equal(matsindf_apply_types(a = 1, b = 2),
               list(dots_present = TRUE, all_dots_num = TRUE, all_dots_mats = FALSE, all_dots_list = FALSE, all_dots_vect = FALSE, all_dots_char = FALSE))
  expect_equal(matsindf_apply_types(a = matrix(c(1, 2)), b = matrix(c(2, 3)), c = matrix(c(3, 4))),
               list(dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = TRUE, all_dots_list = FALSE, all_dots_vect = FALSE, all_dots_char = FALSE))
  expect_equal(matsindf_apply_types(a = list(1, 2), b = list(3, 4), c = list(5, 6)),
               list(dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = FALSE, all_dots_list = TRUE, all_dots_vect = TRUE, all_dots_char = FALSE))
  expect_equal(matsindf_apply_types(a = "a", b = "b", c = "c"),
               list(dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = FALSE, all_dots_list = FALSE, all_dots_vect = FALSE, all_dots_char = TRUE))
  expect_equal(matsindf_apply_types(),
               list(dots_present = FALSE, all_dots_num = FALSE, all_dots_mats = FALSE, all_dots_list = FALSE, all_dots_vect = FALSE, all_dots_char = FALSE))
})


test_that("matsindf_apply() works with a NULL argument", {
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  c <- a + b
  d <- a - b
  DF <- data.frame(a = I(list(a, a)), b = I(list(b,b)), stringsAsFactors = FALSE)
  # Here is where the NULL is given as an argument to matsindp_apply.
  result <- matsindf_apply(DF, FUN = example_fun, a = "a", b = "b", z = NULL)
  expected <- dplyr::bind_cols(DF, data.frame(c = I(list(c, c)), d = I(list(d, d)), stringsAsFactors = FALSE))
  expect_equivalent(result, expected)
  # Try with piped .DF argument
  result <- DF %>% matsindf_apply(FUN = example_fun, a = "a", b = "b", z = NULL)
  expect_equivalent(result, expected)
})


test_that("matsindf_apply() works when .DF is a list", {
  expect_equal(matsindf_apply(list(a = 1, b = 2), FUN = example_fun, a = "a", b = "b"),
               list(a = 1, b = 2, c = 3, d = -1))
})


test_that("matsindf_apply() works when .DF supplies some or all argument names", {
  # All arguments to FUN are supplied by named items in .DF
  expect_equal(matsindf_apply(list(a = 1, b = 2), FUN = example_fun),
               list(a = 1, b = 2, c = 3, d = -1))
  # All arguments are supplied by named arguments in ...
  expect_equal(matsindf_apply(list(a = 1, b = 2), FUN = example_fun, a = "a", b = "b"),
               list(a = 1, b = 2, c = 3, d = -1))
  # All arguments are supplied by named arguments in ..., but mix them up.
  # Note that the named arguments override the items in .DF
  expect_equal(matsindf_apply(list(a = 1, b = 2, z = 10), FUN = example_fun, a = "z", b = "b"),
               list(a = 1, b = 2, z = 10, c = 12, d = 8))
  # Try when one of the output names is same as an input name
  expect_warning(matsindf_apply(list(a = 1, b = 2, c = 10), FUN = example_fun, a = "c", b = "b"),
                 "name collision in matsindf_apply: c") %>%
    expect_equal(c(list(a = 1, b = 2, c = 10), list(c = 12, d = 8)))
})


test_that("matsindf_apply() works for single numbers in data frame columns", {
  DF <- tibble::tibble(a = c(4, 4, 5), b = c(4, 4, 4))
  expected <- DF
  expected$c <- c(8, 8, 9)
  expected$d <- c(0, 0, 1)
  expect_equal(matsindf_apply(DF, FUN = example_fun, a = "a", b = "b"), expected)
  expect_equal(matsindf_apply(DF, FUN = example_fun), expected)
})


test_that("override works() for single numbers supplied in a list", {
  expect_equal(matsindf_apply(list(a = 2, b = 1), FUN = example_fun, a = 10),
               list(a = 10, b = 1, c = 11, d = 9))
})


test_that("matsindf_apply() works when an argument is missing", {
  outer_fun <- function(.DF = NULL, a = "a", b = "b"){
    inner_fun <- function(a_num, b_num = NULL){
      return(list(c = sum_byname(a_num, b_num), d = difference_byname(a_num, b_num)))
    }
    matsindf_apply(.DF, FUN = inner_fun, a_num = a, b_num = b)
  }
  # Make sure it works when all arguments are present.
  expect_equal(outer_fun(a = 2, b = 2), list(c = 4, d = 0))
  # Now try when an argument is missing and the inner function can handle it.
  expect_equal(outer_fun(a = 2), list(c = 2, d = 2))
  # Try when an argument is missing and the inner function can't handle it.
  expect_error(outer_fun(b = 2),
               'argument "a_num" is missing, with no default')
})


test_that("matsindf_apply() works with functions similar in form to those in `Recca`", {

  # This function is similar in form to functions in `Recca`.
  find_matching_rownames <- function(.DF = NULL,
                                     prefixes,
                                     m = "m",
                                     out_colname = "matching_rows") {

    rowmatch_fun <- function(prefixes_arg, m_arg) {
      out <- m_arg %>%
        matsbyname::select_rows_byname(retain_pattern =
                                         matsbyname::make_pattern(row_col_names = prefixes_arg, pattern_type = "leading")) %>%
        matsbyname::getrownames_byname()
      # If we don't have a list, make a list.
      if (!is.list(out)) {
        out <- list(out)
      }
      # At this point, we already have a list.
      # All that remains is to set the name of the list.
      out %>%
        magrittr::set_names(out_colname)
    }

    matsindf_apply(.DF,
                   FUN = rowmatch_fun,
                   prefixes_arg = prefixes,
                   m_arg = m)
  }

  # Set up a matrix
  mat <- matrix(c(0, 1,
                  2, 3,
                  4, 5), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("ra1", "ra2", "b3"), c("c1", "c2")))

  # This fails, because we go into find_matching_rownames without the string prefixes_arg argument.
  expect_error(find_matching_rownames(prefixes = "ra", m = mat), 'argument "prefixes_arg" is missing, with no default')

  # Now make lists and try again.  This works fine.
  res2 <- find_matching_rownames(prefixes = list("ra"), m = list(mat))
  expect_type(res2, type = "list")
  expect_s3_class(res2, class = "data.frame")
  expect_equal(res2[["matching_rows"]][[1]], c("ra1", "ra2"))

  # Use with lists and 2 items in each list.
  res3 <- find_matching_rownames(prefixes = list("ra", "ra"), m = list(mat, mat))
  expect_type(res3, type = "list")
  expect_s3_class(res3, class = "data.frame")
  expect_equal(res3[["matching_rows"]][[1]], c("ra1", "ra2"))
  expect_equal(res3[["matching_rows"]][[2]], c("ra1", "ra2"))
  expect_equal(ncol(res3), 1)

  # Try in a data frame.
  df <- tibble::tibble(prefixes = list("ra", c("r", "b"), "b"), m = list(mat, mat, mat)) %>%
    find_matching_rownames(prefixes = "prefixes")
  expect_equal(df$matching_rows[[1]], c("ra1", "ra2"))
  expect_equal(df$matching_rows[[2]], c("ra1", "ra2", "b3"))
  expect_equal(df$matching_rows[[3]], "b3")

})


test_that("matsindf_apply() issues a warning when replacing a column", {
  # Create a data frame with a matrix in a column.
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- 2 * a
  DF <- data.frame(a = I(list(a, a)), b = I(list(b, b)), stringsAsFactors = FALSE)

  replace_func <- function(a_mat, b_mat) {
    a <- matsbyname::difference_byname(a_mat, b_mat)
    d <- matsbyname::sum_byname(a_mat, b_mat)
    list(a, d) %>%
      magrittr::set_names(c(a_name, d_name))
  }

  a_name <- "a"
  d_name <- "d"
  expect_warning(matsindf_apply(DF, FUN = replace_func, a_mat = "a", b_mat = "b"), "name collision in matsindf_apply: a")

})





