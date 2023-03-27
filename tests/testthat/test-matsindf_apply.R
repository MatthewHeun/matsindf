
test_that("matsindf_apply() fails with an unexpected argument (Case 1)", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_error(matsindf_apply(.dat = "a string", FUN = example_fun, a = 2, b = 2),
               ".dat must be NULL, a data frame, or a list in matsindf_apply\\(\\), was character")
})


test_that("matsindf_apply() works as expected for single values (Case 2)", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_equal(example_fun(a = 2, b = 2), list(c = 4, d = 0))
  expect_equal(matsindf_apply(FUN = example_fun, a = 2, b = 2),
               list(c = 4, d = 0))
})


test_that("matsindf_apply() works as expected for single matrices (Case 2)", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  expected_list <- list(c = a + b, d = a - b)
  expect_equal(example_fun(a, b), expected_list)
  expect_equal(matsindf_apply(FUN = example_fun, a = a, b = b),
               expected_list)
})


test_that("matsindf_apply() works as expected for single Matrix objects (Case 2)", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b),
                d = matsbyname::difference_byname(a, b)))
  }
  a <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
                          dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  expected_list <- list(c = a + b, d = a - b)
  expect_equal(example_fun(a, b), expected_list)
  expect_equal(matsindf_apply(FUN = example_fun, a = a, b = b), expected_list)
})


test_that("matsindf_apply() works as expected for lists of single values (Case 5)", {
  example_fun <- function(a, b){
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


test_that("matsindf_apply() works as expected for lists of matrices (Case 5)", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
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
  expect_equal(matsindf_apply(FUN = example_fun, a = a, b = b),
               DF_expected)
})


test_that("matsindf_apply() works as expected for lists of Matrix objects (Case 5)", {
  example_fun <- function(a, b){
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
  DF_expected <- data.frame(c = I(list(c, c)), d = I(list(d, d)), stringsAsFactors = FALSE)
  # Because DF_expected$c and DF_expected$d are created with I(list()), their class is "AsIs".
  # Need to set the class of DF_expected$c and DF_expected$d to NULL to get a match.
  attr(DF_expected$c, which = "class") <- NULL
  attr(DF_expected$d, which = "class") <- NULL
  expect_equal(matsindf_apply(FUN = example_fun, a = a, b = b), DF_expected)
})


test_that("matsindf_apply() works as expected using .dat with single numbers (Case 13)", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  DF <- tibble::tibble(a = c(4, 4, 5), b = c(4, 4, 4))
  expect_equal(matsindf_apply(DF, FUN = example_fun, a = "a", b = "b"),
               tibble::tibble(a = c(4, 4, 5), b = c(4, 4, 4), c = c(8, 8, 9), d = c(0, 0, 1)))
})


test_that("matsindf_apply() works as expected using .DF with matrices (Case 13)", {
  example_fun <- function(a, b){
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


test_that("matsindf_apply() works as expected using .DF with Matrix objects (Case 13)", {
  example_fun <- function(a, b){
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


test_that("matsindf_apply() fails as expected when not all same type for ...", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_error(matsindf_apply(FUN = example_fun, a = "a", b = 2),
               "In matsindf::matsindf_apply\\(\\), the following named arguments to FUN were found neither in .dat, nor in ..., nor in defaults: a")
})


test_that("matsindf_apply() fails as expected when wrong type of data is sent in ...", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_error(matsindf_apply(FUN = example_fun, a = list("a"), b = list(2)), "non-numeric argument to binary operator")
})


test_that("matsindf_apply() fails gracefully when some of ... are NULL (Case 2)", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_error(matsindf_apply(FUN = example_fun, a = 1, b = 2, c = NULL), "unused argument")
})


test_that("matsindf_apply() fails as expected when .DF argument is missing from a data frame (Case 13)", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_error(matsindf_apply(FUN = example_fun, a = "a", b = "b"),
               'argument "a" is missing, with no default')
})


test_that("matsindf_apply() fails as expected when .DF argument is not a data frame or a list (Case 6)", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_error(matsindf_apply(.DF = "string", FUN = example_fun, a = "a", b = "b"),
               'argument "a" is missing, with no default')
})


test_that("matsindf_apply() works with a NULL argument (Case 13)", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  c <- a + b
  d <- a - b
  DF <- data.frame(a = I(list(a, a)), b = I(list(b,b)))
  # Here is where the NULL is given as an argument to matsindp_apply.
  result <- matsindf_apply(DF, FUN = example_fun, a = "a", b = "b", z = NULL)
  expected <- dplyr::bind_cols(DF, data.frame(c = I(list(c, c)), d = I(list(d, d))))
  expect_equal(result, expected, ignore_attr = TRUE)
  # Try with piped .DF argument
  result <- DF %>% matsindf_apply(FUN = example_fun, a = "a", b = "b", z = NULL)
  expect_equal(result, expected, ignore_attr = TRUE)
})


test_that("matsindf_apply() works with a NULL argument and Matrix objects (Case 13)", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  a <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a
  c <- matsbyname::sum_byname(a, b)
  d <- matsbyname::difference_byname(a, b)
  DF <- data.frame(a = I(list(a, a)), b = I(list(b,b)), stringsAsFactors = FALSE)
  # Here is where the NULL is given as an argument to matsindp_apply.
  result <- matsindf_apply(DF, FUN = example_fun, a = "a", b = "b", z = NULL)
  expected <- dplyr::bind_cols(DF, data.frame(c = I(list(c, c)), d = I(list(d, d)), stringsAsFactors = FALSE))
  expect_equal(result, expected, ignore_attr = TRUE)
  # Try with piped .DF argument
  result <- DF %>% matsindf_apply(FUN = example_fun, a = "a", b = "b", z = NULL)
  expect_equal(result, expected, ignore_attr = TRUE)
})


test_that("matsindf_apply() works when .dat is a list (Case 13)", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_equal(matsindf_apply(list(a = 1, b = 2), FUN = example_fun, a = "a", b = "b"),
               list(a = 1, b = 2, c = 3, d = -1))
})


test_that("matsindf_apply() works when .dat supplies some or all argument names (Cases 11, 3 )", {
  example_fun <- function(a, b){
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
                 "name collision in matsindf_apply: c")
  expect_equal(res, c(list(a = 1, b = 2, c = 10), list(c = 12, d = 8)))
})


test_that("matsindf_apply() works for single numbers in data frame columns (Case 12)", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  DF <- tibble::tibble(a = c(4, 4, 5), b = c(4, 4, 4))
  expected <- DF
  expected$c <- c(8, 8, 9)
  expected$d <- c(0, 0, 1)
  expect_equal(matsindf_apply(DF, FUN = example_fun, a = "a", b = "b"), expected)
  expect_equal(matsindf_apply(DF, FUN = example_fun), expected)
})


test_that("override works() for single numbers supplied in a list (Case 12)", {
  example_fun <- function(a, b){
    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b)))
  }
  expect_equal(matsindf_apply(list(a = 2, b = 1), FUN = example_fun, a = 10),
               list(a = 10, b = 1, c = 11, d = 9))
})


test_that("matsindf_apply() works when an argument is missing (Case 2)", {
  outer_fun <- function(.DF = NULL, a = "a", b = "b"){
    inner_fun <- function(a_num, b_num = NULL){
      return(list(c = matsbyname::sum_byname(a_num, b_num),
                  d = matsbyname::difference_byname(a_num, b_num)))
    }
    matsindf_apply(.DF, FUN = inner_fun, a_num = a, b_num = b)
  }
  # Make sure it works when all arguments are present.
  expect_equal(outer_fun(a = 2, b = 2),
               list(c = 4, d = 0))
  # Now try when an argument is missing and the inner function can handle it.
  expect_equal(outer_fun(a = 2),
               list(c = 2, d = 2))
  # Try when an argument is missing and the inner function can't handle it.
  expect_error(outer_fun(b = 2),
               "In matsindf::matsindf_apply\\(\\), the following named arguments to FUN were found neither in .dat, nor in ..., nor in defaults: a_num")
})


test_that("matsindf_apply() works with functions similar in form to those in `Recca` (Cases 13 and 15)", {

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
  mat <- matrix(c(0, 1,
                  2, 3,
                  4, 5), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("ra1", "ra2", "b3"), c("c1", "c2")))

  # This fails, because we go into find_matching_rownames without the string prefixes_arg argument.
  expect_error(find_matching_rownames(prefixes = "ra", m = mat),
               "In matsindf::matsindf_apply\\(\\), the following named arguments to FUN were found neither in .dat, nor in ..., nor in defaults: prefixes_arg")

  # Now make lists and try again.  This works fine. (Case 15)
  res2 <- find_matching_rownames(prefixes = list("ra"), m = list(mat))
  expect_type(res2, type = "list")
  expect_s3_class(res2, class = "data.frame")
  expect_equal(res2[["matching_rows"]][[1]], c("ra1", "ra2"))

  # Use with lists and 2 items in each list. (Case 15)
  res3 <- find_matching_rownames(prefixes = list("ra", "ra"), m = list(mat, mat))
  expect_type(res3, type = "list")
  expect_s3_class(res3, class = "data.frame")
  expect_equal(res3[["matching_rows"]][[1]], c("ra1", "ra2"))
  expect_equal(res3[["matching_rows"]][[2]], c("ra1", "ra2"))
  expect_equal(ncol(res3), 1)

  # Try in a data frame. (Case 13)
  df <- tibble::tibble(prefixes = list("ra", c("r", "b"), "b"), m = list(mat, mat, mat)) %>%
    find_matching_rownames(prefixes = "prefixes")
  expect_equal(df$matching_rows[[1]], c("ra1", "ra2"))
  expect_equal(df$matching_rows[[2]], c("ra1", "ra2", "b3"))
  expect_equal(df$matching_rows[[3]], "b3")
})


test_that("matsindf_apply() works with functions similar in form to those in `Recca` with Matrix objects (Cases 13 and 15)", {

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
  expect_error(find_matching_rownames(prefixes = "ra", m = mat),
               "In matsindf::matsindf_apply\\(\\), the following named arguments to FUN were found neither in .dat, nor in ..., nor in defaults: prefixes_arg")

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


test_that("matsindf_apply() issues a warning when replacing a column (Case 13)", {
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
    "name collision in matsindf_apply: a"
  )
})


test_that("matsindf_apply() issues a warning when replacing a column with Matrix objects (Case 13)", {
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
    "name collision in matsindf_apply: a"
  )
})


test_that("matsindf_apply() works for a string and numbers (Case 5)", {
  example_fun <- function(str_a, b) {
    a <- as.numeric(str_a)
    list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a, b))
  }
  res <- matsindf_apply(FUN = example_fun, str_a = list("1"), b = list(2))
  expect_equal(res$c[[1]], 3)
  expect_equal(res$d[[1]], -1)
})


test_that("matsindf_apply() works as desired in degenerate case (Case 5)", {

  expected <- list(a = c("string1", "string2"), b = list(matrix(data = 42), matrix(data = 43)), c = NULL)

  expect_equal(
    matsindf_apply(FUN = `+`, a = c("string1", "string2"), b = list(matrix(data = 42), matrix(data = 43)), c = NULL),
    expected
    )
})


test_that("matsindf_apply() works as desired with zero-row data frames (Case 13)", {
  example_fun <- function(a_var, b_var){
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


test_that("matsindf_apply() works as desired with zero-length lists and matrix objects (Case 5)", {
  example_fun <- function(a_var, b_var){
    return(list(c = matsbyname::sum_byname(a_var, b_var),
                d = matsbyname::difference_byname(a_var, b_var)))
  }
  a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
              dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a + 1
  c <- a + b
  d <- a - b
  expected_df <- tibble::tribble(~c, ~d,
                                 c, d,
                                 c, d) |>
    as.data.frame()

  res <- matsindf_apply(FUN = example_fun, a_var = list(a, a), b_var = list(b, b))

  expect_equal(res, expected_df)

  # Now try with zero-length lists
  res_zero <- matsindf_apply(FUN = example_fun, a_var = list(), b_var = list())
  expect_equal(res_zero, list(a_var = list(), b_var = list()))
})


test_that("matsindf_apply() works as desired with zero-length lists and Matrix objects (Case 5)", {
  example_fun <- function(a_var, b_var){
    return(list(c = matsbyname::sum_byname(a_var, b_var),
                d = matsbyname::difference_byname(a_var, b_var)))
  }
  a <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
                          dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- a + 1
  c <- a + b
  d <- a - b
  expected_df <- tibble::tribble(~c, ~d,
                                 c, d,
                                 c, d) |>
    as.data.frame()

  res <- matsindf_apply(FUN = example_fun, a_var = list(a, a), b_var = list(b, b))

  expect_equal(res, expected_df)

  # Now try with zero-length lists
  res_zero <- matsindf_apply(FUN = example_fun, a_var = list(), b_var = list())
  expect_equal(res_zero, list(a_var = list(), b_var = list()))
})


test_that("matsindf_apply() works with empty lists (Cases 5 and 15)", {
  example_fun <- function(a_var, b_var){
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


test_that("matsindf_apply() works with a no-argument function (Case 2)", {
  example_fun <- function() {42}

  expect_equal(matsindf_apply(FUN = example_fun), 42)
})


test_that("matsindf_apply_types() works as expected", {
  example_fun <- function(a, b) {
    c(a, b)
  }
  expect_error(matsindf_apply_types(.dat = NULL, FUN = example_fun),
               "In matsindf::matsindf_apply\\(\\), the following named arguments to FUN were found neither in .dat, nor in ..., nor in defaults: a, b")

  expect_equal(matsindf_apply_types(.dat = NULL, FUN = example_fun,
                                    a = 1, b = 2),
               list(.dat_null = TRUE, .dat_df = FALSE, .dat_list = FALSE, .dat_names = NULL,
                    FUN_arg_names = c("a", "b"),
                    dots_present = TRUE, all_dots_num = TRUE, all_dots_mats = FALSE, all_dots_list = FALSE, all_dots_vect = FALSE, all_dots_char = FALSE,
                    dots_names = c("a", "b"),
                    arg_source = list(dots = c(a = TRUE, b = TRUE),
                                      .dat = c(a = FALSE, b = FALSE),
                                      defaults = c(a = FALSE, b = FALSE))))

  expect_equal(matsindf_apply_types(.dat = data.frame(a = 42), FUN = example_fun,
                                    a = matrix(c(1, 2)), b = matrix(c(2, 3)), c = matrix(c(3, 4))),
               list(.dat_null = FALSE, .dat_df = TRUE, .dat_list = TRUE, .dat_names = "a",
                    FUN_arg_names = c("a", "b"),
                    dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = TRUE, all_dots_list = FALSE, all_dots_vect = FALSE, all_dots_char = FALSE,
                    dots_names = c("a", "b", "c"),
                    arg_source = list(dots = c(a = TRUE, b = TRUE),
                                      .dat = c(a = FALSE, b = FALSE),
                                      defaults = c(a = FALSE, b = FALSE))))

  expect_equal(matsindf_apply_types(.dat = list(a = 1, b = 2), FUN = example_fun,
                                    a = list(1, 2), b = list(3, 4), c = list(5, 6)),
               list(.dat_null = FALSE, .dat_df = FALSE, .dat_list = TRUE, .dat_names = c("a", "b"),
                    FUN_arg_names = c("a", "b"),
                    dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = FALSE, all_dots_list = TRUE, all_dots_vect = TRUE, all_dots_char = FALSE,
                    dots_names = c("a", "b", "c"),
                    arg_source = list(dots = c(a = TRUE, b = TRUE),
                                      .dat = c(a = FALSE, b = FALSE),
                                      defaults = c(a = FALSE, b = FALSE))))

  expect_equal(matsindf_apply_types(.dat = NULL, FUN = example_fun,
                                    a = "a", b = "b", c = "c"),
               list(.dat_null = TRUE, .dat_df = FALSE, .dat_list = FALSE, .dat_names = NULL,
                    FUN_arg_names = c("a", "b"),
                    dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = FALSE, all_dots_list = FALSE, all_dots_vect = FALSE, all_dots_char = TRUE,
                    dots_names = c("a", "b", "c"),
                    arg_source = list(dots = c(a = TRUE, b = TRUE),
                                      .dat = c(a = FALSE, b = FALSE),
                                      defaults = c(a = FALSE, b = FALSE))))

  # Try with Matrix objects
  expect_equal(matsindf_apply_types(.dat = NULL, FUN = example_fun,
                                    a = matsbyname::Matrix(c(1, 2)), b = matsbyname::Matrix(c(2, 3)), c = matsbyname::Matrix(c(3, 4))),
               list(.dat_null = TRUE, .dat_df = FALSE, .dat_list = FALSE, .dat_names = NULL,
                    FUN_arg_names = c("a", "b"),
                    dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = TRUE, all_dots_list = FALSE, all_dots_vect = FALSE, all_dots_char = FALSE,
                    dots_names = c("a", "b", "c"),
                    arg_source = list(dots = c(a = TRUE, b = TRUE),
                                      .dat = c(a = FALSE, b = FALSE),
                                      defaults = c(a = FALSE, b = FALSE))))

  # Try with data coming from one but not the other source.
  expect_equal(matsindf_apply_types(.dat = data.frame(a = 42), FUN = example_fun,
                                    b = matrix(c(2, 3))),
               list(.dat_null = FALSE, .dat_df = TRUE, .dat_list = TRUE, .dat_names = "a",
                    FUN_arg_names = c("a", "b"),
                    dots_present = TRUE, all_dots_num = FALSE, all_dots_mats = TRUE, all_dots_list = FALSE, all_dots_vect = FALSE, all_dots_char = FALSE,
                    dots_names = "b",
                    arg_source = list(dots = c(a = FALSE, b = TRUE),
                                      .dat = c(a = TRUE, b = FALSE),
                                      defaults = c(a = FALSE, b = FALSE))))
})


test_that("matsindf_apply_types() works with functions that have default values", {
  example_fun <- function(a = 2, b, c = "string") {
    list(a = a, b = b, c = c)
  }

  expect_equal(matsindf_apply_types(.dat = NULL, FUN = example_fun,
                                    a = 1, b = 2),
               list(.dat_null = TRUE, .dat_df = FALSE, .dat_list = FALSE, .dat_names = NULL,
                    FUN_arg_names = c("a", "b", "c"),
                    dots_present = TRUE, all_dots_num = TRUE, all_dots_mats = FALSE, all_dots_list = FALSE, all_dots_vect = FALSE, all_dots_char = FALSE,
                    dots_names = c("a", "b"),
                    arg_source = list(dots = c(a = TRUE, b = TRUE, c = FALSE),
                                      .dat = c(a = FALSE, b = FALSE, c = FALSE),
                                      defaults = c(a = FALSE, b = FALSE, c = TRUE))))
})

