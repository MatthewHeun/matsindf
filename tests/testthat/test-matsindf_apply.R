
test_that("matsindf_apply() fails with an unexpected argument", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_error(matsindf_apply(.dat = "a string", FUN = example_fun, a = 2, b = 2),
               ".dat must be NULL, a data frame, or a list in matsindf_apply\\(\\), was character")
})


test_that("matsindf_apply() works as expected for single values", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_equal(example_fun(a = 2, b = 2), list(c = 4, d = 0))
  expect_equal(matsindf_apply(FUN = example_fun, a = 2, b = 2),
               list(c = 4, d = 0))
})


test_that("matsindf_apply() works as expected for single matrices", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  c <- matsbyname::sum_byname(a, b)
  d <- matsbyname::difference_byname(a, b)

  expected_list <- list(c = c, d = d)
  expect_equal(example_fun(a, b), expected_list)
  expect_equal(matsindf_apply(FUN = example_fun, a = a, b = b),
               expected_list)
})


test_that("matsindf_apply() works as expected for single Matrix objects", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b),
                d = matsbyname::difference_byname(a, b)))
  }
  a <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
                          dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  c <- matsbyname::sum_byname(a, b)
  d <- matsbyname::difference_byname(a, b)
  expected_list <- list(c = a + b, d = a - b)
  expect_equal(example_fun(a, b), expected_list)
  expect_equal(matsindf_apply(FUN = example_fun, a = a, b = b), expected_list)
})


test_that("matsindf_apply() works as expected for lists of single values", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_equal(example_fun(a = list(2, 2), b = list(2, 2)), list(c = list(4, 4), d = list(0, 0)))

  expect_equal(matsindf_apply(FUN = example_fun, a = list(2, 2, 2), b = list(2, 2, 2)),
               data.frame(c = I(list(4, 4, 4)), d = I(list(0, 0, 0))),
               ignore_attr = TRUE)
  expect_equal(matsindf_apply(FUN = example_fun, a = list(2, 2), b = list(1, 2)),
               data.frame(c = I(list(3, 4)), d = I(list(1, 0))),
               ignore_attr = TRUE)
})


test_that("matsindf_apply() works as expected for lists of matrices", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  c <- a + b
  d <- a - b
  a <- list(a, a)
  b <- list(b, b)
  list_expected <- list(c = list(c, c), d = list(d, d))
  # Because DF_expected$c and DF_expected$d are created with I(list()), their class is "AsIs".
  # Need to set the class of DF_expected$c and DF_expected$d to NULL to get a match.
  attr(list_expected$c, which = "class") <- NULL
  attr(list_expected$d, which = "class") <- NULL
  expect_equal(matsindf_apply(FUN = example_fun, a = a, b = b),
               list_expected)
})


test_that("matsindf_apply() works as expected for lists of Matrix objects", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  a <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
                          dimnames = list(c("r1", "r2"), c("c1", "c2")),
                          rowtype = "rows", coltype = "cols")
  b <- a
  c <- matsbyname::sum_byname(a, b)
  d <- matsbyname::difference_byname(a, b)
  a <- list(a, a)
  b <- list(b, b)
  list_expected <- list(c = list(c, c), d = list(d, d))
  # Because DF_expected$c and DF_expected$d are created with I(list()), their class is "AsIs".
  # Need to set the class of DF_expected$c and DF_expected$d to NULL to get a match.
  attr(list_expected$c, which = "class") <- NULL
  attr(list_expected$d, which = "class") <- NULL
  expect_equal(matsindf_apply(FUN = example_fun, a = a, b = b),
               list_expected)
})


test_that("matsindf_apply() works as expected using .dat with single numbers", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  DF <- tibble::tibble(a = c(4, 4, 5), b = c(4, 4, 4))
  expect_equal(matsindf_apply(DF, FUN = example_fun, a = "a", b = "b"),
               tibble::tibble(a = c(4, 4, 5), b = c(4, 4, 4), c = c(8, 8, 9), d = c(0, 0, 1)))
})


test_that("matsindf_apply() works as expected using .DF with matrices", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  c <- a + b
  d <- a - b
  DF <- data.frame(a = I(list(a, a)), b = I(list(b,b)))
  result <- matsindf_apply(DF, FUN = example_fun, a = "a", b = "b")
  expected <- dplyr::bind_cols(DF, data.frame(c = I(list(c, c)), d = I(list(d, d))))
  expect_equal(result, expected, ignore_attr = TRUE)
  # Try with piped .DF argument
  result <- DF |>
    matsindf_apply(FUN = example_fun, a = "a", b = "b")
  expect_equal(result, expected, ignore_attr = TRUE)
})


test_that("matsindf_apply() works as expected using .DF with Matrix objects", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  a <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
                          dimnames = list(c("r1", "r2"), c("c1", "c2")),
                          rowtype = "rows", coltype = "cols")
  b <- a
  c <- matsbyname::sum_byname(a, b)
  d <- matsbyname::difference_byname(a, b)
  DF <- data.frame(a = I(list(a, a)), b = I(list(b,b)), stringsAsFactors = FALSE)
  result <- matsindf_apply(DF, FUN = example_fun, a = "a", b = "b")
  expected <- dplyr::bind_cols(DF, data.frame(c = I(list(c, c)), d = I(list(d, d)), stringsAsFactors = FALSE))
  expect_equal(result, expected, ignore_attr = TRUE)
  # Try with piped .DF argument
  result <- DF |>
    matsindf_apply(FUN = example_fun, a = "a", b = "b")
  expect_equal(result, expected, ignore_attr = TRUE)
})


test_that("matsindf_apply() works with .DF containing lists of Matrix objects, only some of which are named", {
  # This is a failure mode for one set of calculations in the PFUAGgDatabase pipeline,
  # namely when doing chops.
  # See if we can reproduce the error here.
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  a <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
                          dimnames = list(c("r1", "r2"), c("c1", "c2")),
                          rowtype = "rows", coltype = "cols")
  b <- a
  c <- matsbyname::sum_byname(a, b)
  d <- matsbyname::difference_byname(a, b)
  DF <- data.frame(a = I(list(a, named_a = a)), b = I(list(b, named_b = b)), stringsAsFactors = FALSE)
  result <- matsindf_apply(DF, FUN = example_fun, a = "a", b = "b")
  expected <- dplyr::bind_cols(DF, data.frame(c = I(list(c, c)), d = I(list(d, d)), stringsAsFactors = FALSE))
  expect_equal(result, expected, ignore_attr = TRUE)
})


test_that("matsindf_apply() fails as expected when not all same type for ...", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b),
                d = matsbyname::difference_byname(a, b)))
  }
  matsindf_apply(FUN = example_fun, a = "a", b = 2) |>
    expect_warning("In matsindf::matsindf_apply\\(\\), the following named arguments to FUN were not found in any of .dat, ..., or defaults to FUN: a") |>
    expect_error('argument "a" is missing, with no default')
})


test_that("matsindf_apply() fails as expected when wrong type of data is sent in ...", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_error(matsindf_apply(FUN = example_fun, a = list("a"), b = list(2)), "non-numeric argument to binary operator")
})


test_that("matsindf_apply() fails gracefully when some of ... are NULL", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_error(matsindf_apply(FUN = example_fun, a = 1, b = 2, c = NULL),
               "In matsindf::matsindf_apply\\(\\), the following unused arguments appeared in ...: c")
})


test_that("matsindf_apply() fails as expected when .DF argument is missing from a data frame", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  matsindf_apply(FUN = example_fun, a = "a", b = "b") |>
    expect_warning("In matsindf::matsindf_apply\\(\\), the following named arguments to FUN were not found in any of .dat, ..., or defaults to FUN: a, b")
})


test_that("matsindf_apply() fails as expected when .DF argument is not a data frame or a list", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  matsindf_apply(.DF = "string", FUN = example_fun, a = "a", b = "b") |>
    expect_warning("In matsindf::matsindf_apply\\(\\), the following named arguments to FUN were not found in any of .dat, ..., or defaults to FUN: a, b") |>
    expect_error("In matsindf::matsindf_apply\\(\\), the following unused arguments appeared in ...: .DF")
})


test_that("matsindf_apply() works with a NULL argument", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  c <- a + b
  d <- a - b
  DF <- data.frame(a = I(list(a, a)), b = I(list(b,b)))
  # Here is where the NULL is given as an argument to matsindf_apply.
  # This attempt fails, because z is an extra argument in ... .
  expect_error(matsindf_apply(DF, FUN = example_fun, a = "a", b = "b", z = NULL),
               "In matsindf::matsindf_apply\\(\\), the following unused arguments appeared in ...: z")
  # Try with piped .dat argument
  expect_error(matsindf_apply(DF, FUN = example_fun, a = "a", b = "b", z = NULL),
               "In matsindf::matsindf_apply\\(\\), the following unused arguments appeared in ...: z")
})


test_that("matsindf_apply() works with a NULL argument and Matrix objects", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  a <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  c <- matsbyname::sum_byname(a, b)
  d <- matsbyname::difference_byname(a, b)
  DF <- data.frame(a = I(list(a, a)), b = I(list(b,b)), stringsAsFactors = FALSE)
  # Here is where the NULL is given as an argument to matsindp_apply.
  # This attempt fails, because z is an extra argument in ... .
  expect_error(matsindf_apply(DF, FUN = example_fun, a = "a", b = "b", z = NULL),
               "In matsindf::matsindf_apply\\(\\), the following unused arguments appeared in ...: z")
  # Try with piped .DF argument
  expect_error(matsindf_apply(DF, FUN = example_fun, a = "a", b = "b", z = NULL),
               "In matsindf::matsindf_apply\\(\\), the following unused arguments appeared in ...: z")
})


test_that("matsindf_apply() works when .dat is a list", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b),
                d = matsbyname::difference_byname(a, b)))
  }
  expect_equal(matsindf_apply(list(a = 1, b = 2), FUN = example_fun, a = "a", b = "b"),
               list(a = 1, b = 2, c = 3, d = -1))
})


test_that("matsindf_apply() works when .dat supplies some or all argument names", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  # All arguments to FUN are supplied by named items in .dat (Case 11)
  expect_equal(matsindf_apply(list(a = 1, b = 2), FUN = example_fun),
               list(a = 1, b = 2, c = 3, d = -1))
  # All arguments are supplied by named arguments in ... (Case 3)
  expect_equal(matsindf_apply(list(a = 1, b = 2), FUN = example_fun, a = "a", b = "b"),
               list(a = 1, b = 2, c = 3, d = -1))
  # All arguments are supplied by named arguments in ..., but mix them up. (Case 3)
  # Note that the named arguments override the items in .DF
  expect_equal(matsindf_apply(list(a = 1, b = 2, z = 10), FUN = example_fun, a = "z", b = "b"),
               list(a = 1, b = 2, z = 10, c = 12, d = 8))
  # Try when one of the output names is same as an input name (Case 3)
  expect_warning(res <- matsindf_apply(list(a = 1, b = 2, c = 10), FUN = example_fun, a = "c", b = "b"),
                 "Name collision in matsindf::matsindf_apply\\(\\). The following arguments appear both in .dat and in the output of `FUN`: c")
  expect_equal(res, c(list(a = 1, b = 2, c = 10), list(c = 12, d = 8)))
})


test_that("matsindf_apply() works for single numbers in data frame columns", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  DF <- tibble::tibble(a = c(4, 4, 5), b = c(4, 4, 4))
  expected <- DF
  expected$c <- c(8, 8, 9)
  expected$d <- c(0, 0, 1)
  expect_equal(matsindf_apply(DF, FUN = example_fun, a = "a", b = "b"), expected)
  expect_equal(matsindf_apply(DF, FUN = example_fun), expected)
})


test_that("override works() for single numbers supplied in a list", {
  example_fun <- function(a, b) {
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_equal(matsindf_apply(list(a = 2, b = 1), FUN = example_fun, a = 10),
               list(a = 10, b = 1, c = 11, d = 9))
})


test_that("matsindf_apply() works when an argument is missing", {
  outer_fun <- function(.DF = NULL, a = "a", b = "b") {
    inner_fun <- function(a_num, b_num = NULL) {
      return(list(c = matsbyname::sum_byname(a_num, b_num),
                  d = matsbyname::difference_byname(a_num, b_num)))
    }
    matsindf_apply(.DF, FUN = inner_fun, a_num = a, b_num = b)
  }
  # Make sure it works when all arguments are present.
  expect_equal(outer_fun(a = 2, b = 2), list(c = 4, d = 0))
  # Now try when an argument is missing and the inner function can handle it.
  expect_equal(outer_fun(a = 2), list(c = 2, d = 2))
  # Try when an argument is missing and the inner function can't handle it.
  outer_fun(b = 2) |>
    expect_warning("In matsindf::matsindf_apply\\(\\), the following named arguments to FUN were not found in any of .dat, ..., or defaults to FUN: a_num") |>
    expect_error('argument "a_num" is missing, with no default')
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
                                         RCLabels::make_or_pattern(strings = prefixes_arg, pattern_type = "leading")) |>
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

  expect_equal(find_matching_rownames(prefixes = list("ra"), m = mat),
               list(matching_rows = c("ra1", "ra2")))

  # Now make lists and try again.  This works fine.
  res2 <- find_matching_rownames(prefixes = list("ra"), m = list(mat))
  expect_type(res2, type = "list")
  expect_equal(res2[["matching_rows"]][[1]], "ra1")
  expect_equal(res2[["matching_rows"]][[2]], "ra2")

  # Use with lists and 2 items in each list.
  res3 <- find_matching_rownames(prefixes = list("ra", "ra"), m = list(mat, mat))
  expect_type(res3, type = "list")
  expect_equal(res3[["matching_rows"]][[1]], c("ra1", "ra2"))
  expect_equal(res3[["matching_rows"]][[2]], c("ra1", "ra2"))
  expect_equal(length(res3), 1)

  # Try in a data frame.
  df <- tibble::tibble(prefixes = list("ra", c("r", "b"), "b"), m = list(mat, mat, mat)) |>
    find_matching_rownames(prefixes = "prefixes")
  expect_equal(df$matching_rows[[1]], c("ra1", "ra2"))
  expect_equal(df$matching_rows[[2]], c("ra1", "ra2", "b3"))
  expect_equal(df$matching_rows[[3]], "b3")
})


test_that("matsindf_apply() works with functions similar in form to those in `Recca` with Matrix objects", {
  # This function is similar in form to functions in `Recca`.
  find_matching_rownames <- function(.DF = NULL,
                                     prefixes,
                                     m = "m",
                                     out_colname = "matching_rows") {

    rowmatch_fun <- function(prefixes_arg, m_arg) {
      out <- m_arg %>%
        matsbyname::select_rows_byname(retain_pattern =
                                         RCLabels::make_or_pattern(strings = prefixes_arg, pattern_type = "leading")) %>%
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
  mat <- matsbyname::Matrix(c(0, 1,
                              2, 3,
                              4, 5), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("ra1", "ra2", "b3"), c("c1", "c2")))

  # This fails, because we go into find_matching_rownames without the string prefixes_arg argument.
  expect_equal(find_matching_rownames(prefixes = list("ra"), m = mat),
               list(matching_rows = c("ra1", "ra2")))

  res2 <- find_matching_rownames(prefixes = list("ra"), m = list(mat))
  expect_type(res2, type = "list")
  expect_equal(res2[["matching_rows"]][[1]], "ra1")
  expect_equal(res2[["matching_rows"]][[2]], "ra2")

  # Use with lists and 2 items in each list. (Case 15)
  res3 <- find_matching_rownames(prefixes = list("ra", "ra"), m = list(mat, mat))
  expect_type(res3, type = "list")
  expect_equal(res3[["matching_rows"]][[1]], c("ra1", "ra2"))
  expect_equal(res3[["matching_rows"]][[2]], c("ra1", "ra2"))
  expect_equal(length(res3), 1)

  # Try in a data frame. (Case 13)
  df <- tibble::tibble(prefixes = list("ra", c("r", "b"), "b"), m = list(mat, mat, mat)) |>
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
  expect_warning(
    suppressMessages(
      # The next call gives a warning but also a "New names:" message.
      # The message clutters the output of the tests,
      # so suppress it.
      # We already know of the problem and are testing for it, anyway.
      matsindf_apply(DF, FUN = replace_func, a_mat = "a", b_mat = "b")
    ),
    "Name collision in matsindf::matsindf_apply\\(\\). The following arguments appear both in .dat and in the output of `FUN`: a"
  )
})


test_that("matsindf_apply() issues a warning when replacing a column with Matrix objects", {
  # Create a data frame with a matrix in a column.
  a <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
                          dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- matsbyname::hadamardproduct_byname(2, a)
  DF <- data.frame(a = I(list(a, a)), b = I(list(b, b)), stringsAsFactors = FALSE)

  replace_func <- function(a_mat, b_mat) {
    a <- matsbyname::difference_byname(a_mat, b_mat)
    d <- matsbyname::sum_byname(a_mat, b_mat)
    list(a, d) %>%
      magrittr::set_names(c(a_name, d_name))
  }

  a_name <- "a"
  d_name <- "d"
  expect_warning(
    suppressMessages(
      # The next call gives a warning but also a "New names:" message.
      # The message clutters the output of the tests,
      # so suppress it.
      # We already know of the problem and are testing for it, anyway.
      matsindf_apply(DF, FUN = replace_func, a_mat = "a", b_mat = "b")
    ),
    "Name collision in matsindf::matsindf_apply\\(\\). The following arguments appear both in .dat and in the output of `FUN`: a"
  )
})


test_that("matsindf_apply() works for a string and numbers", {
  example_fun <- function(str_a, b) {
    a <- as.numeric(str_a)
    list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b))
  }
  res <- matsindf_apply(FUN = example_fun, str_a = list("1"), b = list(2))
  expect_equal(res$c[[1]], 3)
  expect_equal(res$d[[1]], -1)
})


test_that("matsindf_apply() works as desired in degenerate case", {

  sum_fun <- function(a, b) {
    paste0(a, b) |>
      magrittr::set_names("term")
  }

  expected <- list(term = list("string142", "string243"))

  expect_equal(
    matsindf_apply(FUN = sum_fun, a = c("string1", "string2"), b = list("42", "43")),
    expected
    )
})


test_that("matsindf_apply() works as desired with zero-row data frames", {
  example_fun <- function(a_var, b_var) {
    return(list(c = matsbyname::sum_byname(a_var, b_var),
                d = matsbyname::difference_byname(a_var, b_var)))
  }
  a <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
                          dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a + 1

  # Make a data frame
  df <- tibble::tribble(~acol, ~bcol,
                        a, b,
                        a, b)

  expected_df <- df |>
    dplyr::mutate(
      c = matsbyname::sum_byname(acol, bcol),
      d = matsbyname::difference_byname(acol, bcol)
    )

  res <- matsindf_apply(df, FUN = example_fun, a_var = "acol", b_var = "bcol")
  expect_equal(res, expected_df)

  # Now try with no rows in df
  dfzero <- df[0, ]
  res <- matsindf_apply(dfzero, FUN = example_fun, a_var = "acol", b_var = "bcol")
  # Should return the input data frame unmodified.
  expect_equal(res, dfzero)
})


test_that("matsindf_apply() works as desired with zero-length lists and matrix objects", {
  example_fun <- function(a_var, b_var) {
    return(list(c = matsbyname::sum_byname(a_var, b_var),
                d = matsbyname::difference_byname(a_var, b_var)))
  }
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
              dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a + 1
  c <- a + b
  d <- a - b
  expected_list <- list(c = list(c, c), d = list(d, d))

  res <- matsindf_apply(FUN = example_fun, a_var = list(a, a), b_var = list(b, b))

  expect_equal(res, expected_list)

  # Now try with zero-length lists
  res_zero <- matsindf_apply(FUN = example_fun, a_var = list(), b_var = list())
  expect_equal(res_zero, list(a_var = list(), b_var = list()))
})


test_that("matsindf_apply() works as desired with zero-length lists and Matrix objects", {
  example_fun <- function(a_var, b_var) {
    return(list(c = matsbyname::sum_byname(a_var, b_var),
                d = matsbyname::difference_byname(a_var, b_var)))
  }
  a <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
                          dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a + 1
  c <- a + b
  d <- a - b
  expected_list <- list(c = list(c, c), d = list(d, d))

  res <- matsindf_apply(FUN = example_fun, a_var = list(a, a), b_var = list(b, b))

  expect_equal(res, expected_list)

  # Now try with zero-length lists
  res_zero <- matsindf_apply(FUN = example_fun, a_var = list(), b_var = list())
  expect_equal(res_zero, list(a_var = list(), b_var = list()))
})


test_that("matsindf_apply() works with empty lists", {
  example_fun <- function(a_var, b_var) {
    return(list(c = matsbyname::sum_byname(a_var, b_var),
                d = matsbyname::difference_byname(a_var, b_var)))
  }
  a <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
                          dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a + 1

  a_list <- list(a = a)
  b_list <- list(b = b)
  a_list_0 <- a_list[0]
  b_list_0 <- b_list[0]

  # Try with zero-length lists (Case 5)
  res_zero <- matsindf_apply(FUN = example_fun, a_var = a_list_0, b_var = b_list_0)
  expect_equal(res_zero, list(a_var = a_list[0], b_var = b_list[0]))

  # Try with a zero-length variable store (Case 15)
  var_store <- list(a_var = a_list_0, b_var = b_list_0)
  res_zero_2 <- matsindf_apply(var_store, FUN = example_fun)
  expect_equal(res_zero_2, list(a_var = a_list[0], b_var = b_list[0]))
})


test_that("matsindf_apply() works with a no-argument function", {
  example_fun <- function() {42}

  expect_equal(matsindf_apply(FUN = example_fun), 42)
})


test_that("matsindf_apply_types() works as expected", {
  example_fun <- function(a, b) {
    c(a, b)
  }
  matsindf_apply_types(.dat = NULL, FUN = example_fun) |>
    expect_warning("In matsindf::matsindf_apply\\(\\), the following named arguments to FUN were not found in any of .dat, ..., or defaults to FUN: a, b")

  # Set the warning flag to FALSE to suppress the warning
  no_warn <- matsindf_apply_types(.dat = NULL, FUN = example_fun, .warn_missing_FUN_args = FALSE)
  expect_equal(no_warn$keep_args, list(.dat = NULL, FUN = NULL, dots = NULL))

  matsindf_apply_types(.dat = NULL, FUN = example_fun, a = 1, b = 2) |>
    expect_equal(list(.dat_null = TRUE, .dat_df = FALSE, .dat_list = FALSE, .dat_names = NULL,
                      FUN_arg_all_names = c("a", "b"),
                      FUN_arg_default_names = NULL,
                      FUN_arg_default_values = NULL,
                      dots_present = TRUE, all_dots_num = TRUE, all_dots_mats = FALSE, all_dots_list = FALSE,
                      all_dots_vect = TRUE, all_dots_char = FALSE, all_dots_longer_than_1 = FALSE,
                      dots_names = c("a", "b"),
                      keep_args = list(.dat = NULL,
                                       FUN = NULL,
                                       dots = c(a = "a", b = "b"))))

  matsindf_apply_types(.dat = data.frame(a = 42), FUN = example_fun,
                       a = matrix(c(1, 2)), b = matrix(c(2, 3))) |>
    expect_equal(list(.dat_null = FALSE, .dat_df = TRUE, .dat_list = TRUE, .dat_names = "a",
                      FUN_arg_all_names = c("a", "b"),
                      FUN_arg_default_names = NULL,
                      FUN_arg_default_values = NULL,
                      dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = TRUE, all_dots_list = FALSE,
                      all_dots_vect = FALSE, all_dots_char = FALSE, all_dots_longer_than_1 = FALSE,
                      dots_names = c("a", "b"),
                      keep_args = list(.dat = NULL,
                                       FUN = NULL,
                                       dots = c(a = "a", b = "b"))))

  matsindf_apply_types(.dat = list(a = 1, b = 2), FUN = example_fun,
                       a = list(1, 2), b = list(3, 4)) |>
    expect_equal(list(.dat_null = FALSE, .dat_df = FALSE, .dat_list = TRUE, .dat_names = c("a", "b"),
                      FUN_arg_all_names = c("a", "b"),
                      FUN_arg_default_names = NULL,
                      FUN_arg_default_values = NULL,
                      dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = FALSE, all_dots_list = TRUE,
                      all_dots_vect = TRUE, all_dots_char = FALSE, all_dots_longer_than_1 = TRUE,
                      dots_names = c("a", "b"),
                      keep_args = list(.dat = NULL,
                                       FUN = NULL,
                                       dots = c(a = "a", b = "b"))))

  matsindf_apply_types(.dat = NULL, FUN = example_fun, a = "a", b = "b") |>
    expect_warning("In matsindf::matsindf_apply\\(\\), the following named arguments to FUN were not found in any of .dat, ..., or defaults to FUN: a, b")

  # Try with Matrix objects
  matsindf_apply_types(.dat = NULL, FUN = example_fun,
                       a = matsbyname::Matrix(c(1, 2)), b = matsbyname::Matrix(c(2, 3))) |>
    expect_equal(list(.dat_null = TRUE, .dat_df = FALSE, .dat_list = FALSE, .dat_names = NULL,
                      FUN_arg_all_names = c("a", "b"),
                      FUN_arg_default_names = NULL,
                      FUN_arg_default_values = NULL,
                      dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = TRUE, all_dots_list = FALSE,
                      all_dots_vect = FALSE, all_dots_char = FALSE, all_dots_longer_than_1 = FALSE,
                      dots_names = c("a", "b"),
                      keep_args = list(.dat = NULL,
                                       FUN = NULL,
                                       dots = c(a = "a", b = "b"))))

  # Try with data coming from one but not the other source.
  matsindf_apply_types(.dat = data.frame(a = 42), FUN = example_fun,
                       b = matrix(c(2, 3))) |>
    expect_equal(list(.dat_null = FALSE, .dat_df = TRUE, .dat_list = TRUE, .dat_names = "a",
                      FUN_arg_all_names = c("a", "b"),
                      FUN_arg_default_names = NULL,
                      FUN_arg_default_values = NULL,
                      dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = TRUE, all_dots_list = FALSE,
                      all_dots_vect = FALSE, all_dots_char = FALSE, all_dots_longer_than_1 = FALSE,
                      dots_names = "b",
                      keep_args = list(.dat = c(a = "a"),
                                       FUN = NULL,
                                       dots = c(b = "b"))))
})


test_that("matsindf_apply_types() works with functions that have default values", {
  example_fun <- function(a = 2, b, c = "string") {
    list(a = a, b = b, c = c)
  }

  expect_equal(matsindf_apply_types(.dat = NULL, FUN = example_fun,
                                    a = 1, b = 2),
               list(.dat_null = TRUE, .dat_df = FALSE, .dat_list = FALSE, .dat_names = NULL,
                    FUN_arg_all_names = c("a", "b", "c"),
                    FUN_arg_default_names = c("a", "c"),
                    FUN_arg_default_values = list(a = 2, c = "string"),
                    dots_present = TRUE, all_dots_num = TRUE, all_dots_mats = FALSE, all_dots_list = FALSE,
                    all_dots_vect = TRUE, all_dots_char = FALSE, all_dots_longer_than_1 = FALSE,
                    dots_names = c("a", "b"),
                    keep_args = list(.dat = NULL,
                                     FUN = c(c = "c"),
                                     dots = c(a = "a", b = "b"))))
})


test_that("matsindf_apply_types() works with a degenerate case with simple FUN and no parameters", {
  example_fun <- function() {42}

  expect_equal(matsindf_apply_types(FUN = example_fun),
               list(.dat_null = TRUE, .dat_df = FALSE, .dat_list = FALSE, .dat_names = NULL,
                    FUN_arg_all_names = NULL,
                    FUN_arg_default_names = NULL,
                    FUN_arg_default_values = NULL,
                    dots_present = FALSE, all_dots_num = FALSE, all_dots_mats = FALSE, all_dots_list = FALSE,
                    all_dots_vect = FALSE, all_dots_char = FALSE, all_dots_longer_than_1 = FALSE,
                    dots_names = NULL,
                    keep_args = list(.dat = NULL,
                                     FUN = NULL,
                                     dots = NULL)))
})


test_that("matsindf_apply_types() works for examples", {
  identity_fun <- function(a, b) {list(a = a, b = b)}
  types <- matsindf_apply_types(.dat = data.frame(), FUN = identity_fun,
                                a = matrix(c(1, 2)), b = matrix(c(2, 3)))
  expect_null(types$keep_args$.dat)
})


test_that("build_matsindf_apply_data_frame() works as expected", {
  example_fun <- function(a_var, b_var = c(42, 43)) {
    c(a_var, b_var)
  }

  DF <- tibble::tribble(~a, ~b, ~z,
                        1, 2, 3,
                        4, 5, NULL)
  expected_df <- DF |>
    dplyr::select("a", "b") |>
    dplyr::rename(
      a_var = "a", b_var = "b"
    )
  res_df <- build_matsindf_apply_data_frame(.dat = DF, FUN = example_fun, a_var = "a", b_var = "b")
  expect_equal(names(res_df), c("a_var", "b_var"))
  expect_equal(res_df, expected_df)

  DF_2 <- DF |>
    dplyr::select(-b)
  expected_df_2 <- DF |>
    dplyr::mutate(
      b_var = c(42, 43),
      b = NULL) |>
    dplyr::rename(a_var = "a") |>
    dplyr::select(a_var, b_var)
  res_df_2 <- build_matsindf_apply_data_frame(.dat = DF_2, FUN = example_fun, a_var = "a")
  expect_equal(res_df_2, expected_df_2)
})


test_that("build_matsindf_apply_data_frame() works with NULL args in ...", {
  example_fun <- function(R_mat, U_mat) {
    return(matsbyname::sum_byname(R_mat, matsbyname::transpose_byname(U_mat)))
  }
  expected <- tibble::tribble(~R_mat, ~U_mat,
                              NULL, matrix(c(42, 43)))
  res <- build_matsindf_apply_data_frame(FUN = example_fun, R_mat = NULL, U_mat = matrix(c(42, 43)))
  expect_equal(res, expected)
})


test_that("matsindf_apply() works when FUN can handle zero-row DF's", {
  example_fun <- function(a, b) {
    if (length(a) == 0 & length(b) == 0) {
      return(list(c = numeric(), d = numeric()))
    }
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }

  expect_equal(example_fun(a = 2, b = 2), list(c = 4, d = 0))
  expect_equal(matsindf_apply(FUN = example_fun, a = 2, b = 2), list(c = 4, d = 0))
  expect_equal(matsindf_apply(list(a = 2, b = 2), FUN = example_fun), list(a = 2, b = 2, c = 4, d = 0))

  df <- data.frame(a = 2, b = 2)
  df_expected <- tibble::tibble(a = 2, b = 2, c = 4, d = 0)
  expect_equal(matsindf_apply(df, example_fun), df_expected)

  dfzero <- df[0, ]
  dfzero_expected <- df_expected[0, ]
  expect_equal(matsindf_apply(dfzero, example_fun), dfzero_expected)
})


test_that("matsindf_apply() works when FUN can handle zero-length lists", {
  example_fun <- function(a, b) {
    if (length(a) == 0 & length(b) == 0) {
      return(list(c = numeric(), d = numeric()))
    }
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }

  expect_equal(matsindf_apply(FUN = example_fun, a = numeric(), b = numeric()),
               list(c = numeric(), d = numeric()))

  input <- list(a = numeric(), b = numeric())
  output_expected <- list(a = numeric(), b = numeric(), c = numeric(), d = numeric())
  expect_equal(matsindf_apply(input, example_fun), output_expected)
})


test_that("matsindf_apply() works for an edge case", {
  # This example comes from the matsindf_apply primer.
  calc_W <- function(.DF = NULL, U = "U", V = "V", W = "W") {
    # The inner function does all the work.
    W_func <- function(U_mat, V_mat){
      # When we get here, U_mat and V_mat will be single matrices or single numbers,
      # not a column in a data frame or an item in a list.
      if (length(U_mat) == 0 & length(V_mat == 0)) {
        # Tolerate zero-length arguments by returning a zero-length
        # a list with the correct name and return type.
        return(list(numeric()) |> magrittr::setnames(W))
      }
      # Calculate W_mat from the inputs U_mat and V_mat.
      W_mat <- matsbyname::difference_byname(matsbyname::transpose_byname(V_mat), U_mat)
      # Return a named list.
      list(W_mat) |> magrittr::set_names(W)
    }
    # The body of the main function consists of a call to matsindf_apply
    # that specifies the inner function in the FUN argument.
    matsindf_apply(.DF, FUN = W_func, U_mat = U, V_mat = V)
  }

  expect_equal(calc_W(list(U = c(2, 2, 2, 2), V = c(3, 4, 5, 6))),
               list(U = c(2, 2, 2, 2), V = c(3, 4, 5, 6), W = c(1, 2, 3, 4)))
})


test_that("should_unlist() works as expected", {
  DF <- tibble::tibble(a = list(1, 2, 3), b = c("a", "b", "c"),
                       c = list(matrix(c(42, 43)),
                                matrix(c(44, 45)),
                                matrix(c(46, 47))))
  expect_true(matsindf:::should_unlist(DF$a))
  expect_false(matsindf:::should_unlist(DF$b))
  expect_false(matsindf:::should_unlist(DF$c))
  expect_equal(sapply(DF, FUN = function(this_col) {matsindf:::should_unlist(this_col)}),
               c(a = TRUE, b = FALSE, c = FALSE))
  expect_equal(matsindf:::should_unlist(DF),
               c(a = TRUE, b = FALSE, c = FALSE))
})


test_that("should_unlist() correctly says 'FALSE' for a list of NULL items", {
  # Unlist when length is 1
  expect_true(matsindf:::should_unlist(list(NULL)))
  # Do not unlist when the length is > 1
  expect_false(matsindf:::should_unlist(list(NULL, NULL, NULL)))
})


test_that("should_unlist() correctly says 'FALSE' for a list of data frames", {
  my_col <- list(data.frame(a = 42), tibble::tibble(b = 43) )
  expect_false(matsindf:::should_unlist(my_col))
})


test_that("matsindf_apply() works for missing arg in .dat", {
  example_fun <- function(a_val = 2, b_val = 1) {
    list(c = matsbyname::sum_byname(a_val, b_val),
         d = matsbyname::difference_byname(a_val, b_val))
  }

  # Try when everything is available.
  df <- tibble::tibble(a = c(10, 11, 12), b = c(5, 6, 7))
  expected_df <- dplyr::bind_cols(df, data.frame(c = c(15, 17, 19), d = c(5, 5, 5)))
  res <- df |>
    matsindf_apply(FUN = example_fun, a_val = "a", b_val = "b")
  expect_equal(res, expected_df)

  # Try when df lacks a column.
  # matsindf_apply() should use the default value in the signature of example_fun.
  # But as of 4 April 2023, it does not.
  # This test exposes that bug.
  res <- df |>
    dplyr::select("a") |>
    matsindf_apply(FUN = example_fun, a_val = "a")
  expect_equal(res, tibble::tribble(~a, ~c, ~d,
                                    10, 11, 9,
                                    11, 12, 10,
                                    12, 13, 11))
})


test_that("where_to_get_args() works as intended", {
  example_fun <- function(a = 1, b) {
    list(c = a + b, d = a - b)
  }

  matsindf:::where_to_get_args(FUN = example_fun) |>
    expect_equal(list(a = c(source = "FUN", arg_name = "a"),
                      b = NULL))

  matsindf:::where_to_get_args(FUN = example_fun, b = 2) |>
    expect_equal(list(a = c(source = "FUN", arg_name = "a"),
                      b = c(source = "...", arg_name = "b")))

  matsindf:::where_to_get_args(list(a = 2), FUN = example_fun, b = 2) |>
    expect_equal(list(a = c(source = ".dat", arg_name = "a"),
                      b = c(source = "...", arg_name = "b")))

  matsindf:::where_to_get_args(list(a = 2, b = 2), FUN = example_fun) |>
    expect_equal(list(a = c(source = ".dat", arg_name = "a"),
                      b = c(source = ".dat", arg_name = "b")))

  # Map to self
  matsindf:::where_to_get_args(list(a = 2, b = 2, c = 2), FUN = example_fun, a = "a", b = "b") |>
    expect_equal(list(a = c(source = ".dat", arg_name = "a"),
                      b = c(source = ".dat", arg_name = "b")))

  # Check redirection
  matsindf:::where_to_get_args(list(a = 2, b = 2, c = 2), FUN = example_fun, b = "c") |>
    expect_equal(list(a = c(source = ".dat", arg_name = "a"),
                      b = c(source = ".dat", arg_name = "c")))

  # Leave an argument out
  matsindf:::where_to_get_args(list(a = 2), FUN = example_fun) |>
    expect_equal(list(a = c(source = ".dat", arg_name = "a"),
                      b = NULL))

  # Try redirecting to an item that doesn't exist in .dat
  matsindf:::where_to_get_args(list(a = 2, b = 2, c = 2), FUN = example_fun, b = "d") |>
    expect_equal(list(a = c(source = ".dat", arg_name = "a"),
                      b = NULL))

  # In this case a comes from .dat and b comes from .dat as well.
  matsindf:::where_to_get_args(list(a = 2, c = 2), FUN = example_fun, b = "a") |>
    expect_equal(list(a = c(source = ".dat", arg_name = "a"),
                      b = c(source = ".dat", arg_name = "a")))

  # In this case, both a and b should be found in defaults to FUN.
  matsindf:::where_to_get_args(list(c = 2), FUN = example_fun, b = "a") |>
    expect_equal(list(a = c(source = "FUN", arg_name = "a"),
                      b = c(source = "FUN", arg_name = "a")))

  # In this case, both a and b should be found in .dat.
  matsindf:::where_to_get_args(list(a = 2, c = 2), FUN = example_fun, b = "a") |>
    expect_equal(list(a = c(source = ".dat", arg_name = "a"),
                      b = c(source = ".dat", arg_name = "a")))

  # Cross the mappings
  matsindf:::where_to_get_args(list(a = 2, b = 2), FUN = example_fun, a = "b", b = "a") |>
    expect_equal(list(a = c(source = ".dat", arg_name = "b"),
                      b = c(source = ".dat", arg_name = "a")))
})


test_that("matsindf_apply() handles different lengths correctly (i.e., with an error)", {
  example_fun <- function(a = 1, b) {
    list(c = a + b, d = a - b)
  }
  expect_error(matsindf_apply(FUN = example_fun, a = c(1, 2, 3), b = c(4, 5)),
               "Different lengths in handle_null_args\\(\\)")
})


test_that("matsindf_apply() correctly handles NULL default args on FUN", {
  example_fun <- function(a = 1, b = NULL) {
    list(c = a, d = b)
  }

  expect_equal(matsindf:::where_to_get_args(list(a = 1), FUN = example_fun),
               list(a = c(source = ".dat", arg_name = "a"), b = c(source = "FUN", arg_name = "b")))

  expect_equal(matsindf_apply(list(a = 1), FUN = example_fun),
               list(a = 1, b = NULL, c = 1, d = NULL))
})


test_that("matsindf_apply() works correctly for a list of matrices each with length 1", {
  example_fun <- function(a, b) {
    list(c = matsbyname::sum_byname(a, b),
         d = matsbyname::difference_byname(a, b))
  }
  m <- matrix(c(1, 2,
                3, 4), nrow = 2, ncol = 2, byrow = TRUE)
  my_mat_list <- list(a = m, b = m+1)
  expected_c <- my_mat_list$a + my_mat_list$b
  expected_d <- my_mat_list$a - my_mat_list$b
  expect_equal(matsindf_apply(my_mat_list, example_fun),
               list(a = list(my_mat_list$a), b = list(my_mat_list$b), c = expected_c, d = expected_d))

  expect_equal(matsindf_apply(FUN = example_fun, a = m, b = m+1),
               list(c = expected_c, d = expected_d))
  })

