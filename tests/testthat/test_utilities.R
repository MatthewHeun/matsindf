# Contains tests for the matsindf package.

# Need to put dplyr before testthat.
# If not, the "matches" function in dplyr overrides the "matches" function in testthat,
# and tests containing the string "(" don't work as expectged.

library(dplyr)
library(magrittr)
library(lazyeval)
library(tidyr)
library(tibble)
library(byname)
library(testthat)

###########################################################
context("rowcolval --> matrix")
###########################################################

test_that("rowcolval_to_mat works as expected", {
  # Establish some matrices that we expect to see.
  expected_mat <- matrix(c(11, 12,
                           0,  22),
                         nrow = 2, ncol = 2, byrow = TRUE,
                         dimnames = list(c("p1", "p2"), c("i1", "i2")))
  expected_mat_with_types <- expected_mat %>%
    setrowtype("Products") %>% setcoltype("Industries")

  # Create a data frame that can be converted to a matrix.
  rowcolval <- data.frame(Country  = c("GH", "GH", "GH"),
                          rows = c( "p1",  "p1", "p2"),
                          cols = c( "i1",  "i2", "i2"),
                          vals = c(  11  ,  12,   22 ))
  A <- rowcolval_to_mat(rowcolval, rownames = "rows", colnames = "cols", values = "vals")
  expect_equal(A, expected_mat)
  expect_null(rowtype(A)) # rowtype has not been set
  expect_null(coltype(A)) # coltype has not been set

  # Provide single row and column types to be applied to all entries.
  B <- rowcolval_to_mat(rowcolval, rownames = "rows", colnames = "cols", values = "vals",
                        rowtype  = "Products", coltype  = "Industries")
  expect_equal(B, expected_mat_with_types)

  # Provide row and column types in the data frame and specify columns in the call to rowcolval_to_mat.
  C <- rowcolval %>% bind_cols(data.frame(rt = c("Products", "Products", "Products"),
                                          ct = c("Industries", "Industries", "Industries"))) %>%
    rowcolval_to_mat(rownames = "rows", colnames = "cols", values = "vals",
                     rowtype = "rt", coltype = "ct")
  expect_equal(C, expected_mat_with_types)

  # Also works for single values if both the rownames and colnames columns contain NA
  rowcolval2 <- data.frame(Country = c("GH"), rows = c(NA), cols = c(NA),
                           rowtype = c(NA), coltype = c(NA), vals = c(2))
  D <- rowcolval2 %>% rowcolval_to_mat(rownames = "rows", colnames = "cols", values = "vals",
                                       rowtype = "rowtype", coltype = "coltype")
  expect_equal(D, 2)

  # Try without rowtype or coltype columns in the data frame.
  rowcolval3 <- data.frame(Country = c("GH"), rows = c(NA), cols = c(NA), vals = c(2))
  E <- rowcolval3 %>% rowcolval_to_mat(rownames = "rows", colnames = "cols", values = "vals")
  expect_equal(E, 2)

  # Fails when rowtype or coltype not all same. In rowcolval4, column rt is not all same.
  rowcolval4 <- rowcolval %>% bind_cols(data.frame(rt = c("Products", "Industries", "Products"),
                                              ct = c("Industries", "Industries", "Industries")))
  expect_error(rowcolval_to_mat(rowcolval4,
                                rownames = "rows", colnames = "cols",
                                values = "vals",
                                rowtype = "rt", coltype = "ct"), "Not all values in rt \\(rowtype\\) were same as first entry: Products")
})




###########################################################
context("matrix --> rowcolval")
###########################################################

test_that("mat_to_rowcolval works as expected", {


data <- data.frame(Country  = c("GH", "GH", "GH"),
                   rows = c( "c1",  "c1", "c2"),
                   cols = c( "i1",  "i2", "i2"),
                   rt = c("Commodities", "Commodities", "Commodities"),
                   ct = c("Industries", "Industries", "Industries"),
                   vals = c(  11  ,  12,   22 ))
#' data
#' A <- data %>%
#'   rowcolval_to_mat(rownames = "rows", colnames = "cols",
#'                     rowtype = "rt",    coltype = "ct", values = "vals")
#' A
#' mat_to_rowcolval(A, rownames = "rows", colnames = "cols", rowtype = "rt", coltype = "ct", values = "vals")
#' mat_to_rowcolval(A, rownames = "rows", colnames = "cols", rowtype = "rt", coltype = "ct", values = "vals", drop = 0)
#' # This also works for single values
#' mat_to_rowcolval(2, rownames = "rows", colnames = "cols", rowtype = "rt", coltype = "ct", values = "vals")
#' mat_to_rowcolval(0, rownames = "rows", colnames = "cols", rowtype = "rt", coltype = "ct", values = "vals", drop = 0)

})
