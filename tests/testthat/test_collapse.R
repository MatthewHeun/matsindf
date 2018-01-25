# Contains tests for the matsindf package.

# Need to put dplyr before testthat.
# If not, the "matches" function in dplyr overrides the "matches" function in testthat,
# and tests containing the string "(" don't work as expected.

library(dplyr)
library(tidyr)
library(magrittr)
library(tibble)
library(lazyeval)
library(byname)
library(testthat)

###########################################################
context("small example")
###########################################################

test_that("small example works as expected", {
  tidy <- data.frame(matrix = c("V1", "V1", "V1", "V2", "V2"),
                     row = c("i1", "i1", "i2", "i1", "i2"),
                     col = c("p1", "p2", "p2", "p1", "p2"),
                     vals = c(1, 2, 3, 4, 5)) %>%
    mutate(
      rowtype = "Industries",
      coltype  = "Products"
    ) %>%
    group_by(matrix)
  mats <- collapse_to_matrices(tidy, matnames = "matrix", values = "vals",
                               rownames = "row", colnames = "col",
                               rowtypes = "rowtype", coltypes = "coltype")
  # Check that groups are discarded.
  expect_equal(length(group_vars(mats)), 0)
  # Test for V1
  expect_equal(mats$vals[[1]], matrix(c(1, 2, 0, 3), nrow = 2, ncol = 2, byrow = TRUE,
                                            dimnames = list(c("i1", "i2"), c("p1", "p2"))) %>%
    setrowtype("Industries") %>% setcoltype("Products"))
  # Test for V2
  expect_equal(mats$vals[[2]], matrix(c(4, 0, 0, 5), nrow = 2, ncol = 2, byrow = TRUE,
                                      dimnames = list(c("i1", "i2"), c("p1", "p2"))) %>%
                 setrowtype("Industries") %>% setcoltype("Products"))
  # Now expand everything back out, just for good measure
  tidy2 <- mats %>%
    expand_to_tidy(matnames = "matrix", matvals = "vals",
                   rownames = "row", colnames = "col",
                   rowtypes = "rowtype", coltypes = "coltype", drop = 0) %>%
    mutate(
      # The original tidy data frame had factors
      row = as.factor(row),
      col = as.factor(col)
    )
  expect_equal(tidy2, tidy)

  # Try the test when we are missing the rowtype and coltype columns
  tidy_trimmed <- tidy %>%
    mutate(
      rowtype = NULL,
      coltype = NULL
    )
  mats_trimmed <- collapse_to_matrices(tidy, matnames = "matrix", values = "vals",
                                       rownames = "row", colnames = "col")
  # Test for V1
  expect_equal(mats_trimmed$vals[[1]], matrix(c(1, 2, 0, 3), nrow = 2, ncol = 2, byrow = TRUE,
                                              dimnames = list(c("i1", "i2"), c("p1", "p2"))))
  # Test for V2
  expect_equal(mats_trimmed$vals[[2]], matrix(c(4, 0, 0, 5), nrow = 2, ncol = 2, byrow = TRUE,
                                              dimnames = list(c("i1", "i2"), c("p1", "p2"))))
})

###########################################################
context("collapse")
###########################################################

test_that("collapse_to_matrices works as expected", {
  ptype <- "Products"
  itype <- "Industries"
  tidy <- data.frame(Country = c( "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "US",  "US",  "US",  "US", "GH", "US"),
                     Year    = c( 1971,  1971,  1971,  1971,  1971,  1971,  1971,  1980,  1980,  1980,  1980, 1971, 1980),
                     matrix  = c(  "U",  "U",  "Y",  "Y",  "Y",  "V",  "V",  "U",  "U",  "Y",  "Y", "eta", "eta"),
                     row     = c( "p1", "p2", "p1", "p2", "p2", "i1", "i2", "p1", "p1", "p1", "p2",   NA,    NA),
                     col     = c( "i1", "i2", "i1", "i2", "i3", "p1", "p2", "i1", "i2", "i1", "i2",   NA,    NA),
                     rowtype = c(ptype, ptype, ptype, ptype, ptype, itype, itype, ptype, ptype, ptype, ptype, NA, NA),
                     coltype = c(itype, itype, itype, itype, itype, ptype, ptype, itype, itype, itype, itype, NA, NA),
                     vals  = c(   11  ,  22,    11 ,   22 ,   23 ,   11 ,   22 ,   11 ,   12 ,   11 ,   22,   0.2, 0.3)
  ) %>% group_by(Country, Year, matrix)
  mats <- collapse_to_matrices(tidy, matnames = "matrix", values = "vals",
                               rownames = "row", colnames = "col",
                               rowtypes = "rowtype", coltypes = "coltype")
  A <- matrix(c(11, 0,
                0, 22),
              nrow = 2, ncol = 2, byrow = TRUE,
              dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>%
    setrowtype("Products") %>% setcoltype("Industries")

  # Check that the single values turned out OK
  expect_equal((mats %>% filter(Country == "GH", matrix == "eta"))$vals[[1]], 0.2 )
  expect_equal((mats %>% filter(Country == "US", matrix == "eta"))$vals[[1]], 0.3 )

  # Check that GH U turned out OK
  expect_equal((mats %>% filter(Country == "GH", matrix == "U"))$vals[[1]], A)
  # Check that US U turned out OK
  expect_equal((mats %>% filter(Country == "US", matrix == "U"))$vals[[1]],
               matrix(c(11, 12),
                      nrow = 1, ncol = 2, byrow = TRUE,
                      dimnames = list(c("p1"), c("i1", "i2"))) %>%
                 setrowtype("Products") %>% setcoltype("Industries"))
  # Check that GH V turned out OK
  expect_equal((mats %>% filter(Country == "GH", matrix == "V"))$vals[[1]], A %>% transpose_byname)
  # Check that GH Y turned out OK
  expect_equal((mats %>% filter(Country == "GH", matrix == "Y"))$vals[[1]],
               matrix(c(11, 0, 0,
                        0, 22, 23),
                      nrow = 2, ncol = 3, byrow = TRUE,
                      dimnames = list(c("p1", "p2"), c("i1", "i2", "i3"))) %>%
                 setrowtype("Products") %>% setcoltype("Industries"))
  # Check that US Y turned out OK
  expect_equal((mats %>% filter(Country == "US", matrix == "Y"))$vals[[1]], A)
  # Check that groups are discarded.
  expect_equal(length(group_vars(mats)), 0)
})

