# Contains tests for the matsindf package.

# Need to put dplyr before testthat.
# If not, the "matches" function in dplyr overrides the "matches" function in testthat,
# and tests containing the string "(" don't work as expectged.

library(dplyr)
library(tidyr)
library(magrittr)
library(tibble)
library(lazyeval)
library(byname)
library(testthat)

###########################################################
context("expand")
###########################################################

test_that("expand_to_tidy works as expected", {
  ptype <- "Products"
  itype <- "Industries"
  tidy <- data.frame(Country = c( "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "US",  "US",  "US",  "US", "GH", "US"),
                     Year    = c( 1971,  1971,  1971,  1971,  1971,  1971,  1971,  1980,  1980,  1980,  1980, 1971, 1980),
                     matrix  = c(   "U",   "U",   "Y",   "Y",   "Y",   "V",   "V",   "U",   "U",   "Y",   "Y", "eta", "eta"),
                     row     = c(  "p1",  "p2",  "p1",  "p2",  "p2",  "i1",  "i2",  "p1",  "p1",  "p1",  "p2", NA, NA),
                     col     = c(  "i1",  "i2",  "i1",  "i2",  "i3",  "p1",  "p2",  "i1",  "i2",  "i1",  "i2", NA, NA),
                     rowtype = c(ptype, ptype, ptype, ptype, ptype, itype, itype, ptype, ptype, ptype, ptype, NA, NA),
                     coltype = c(itype, itype, itype, itype, itype, ptype, ptype, itype, itype, itype, itype, NA, NA),
                     vals    = c(   11  ,  12,    13 ,   14 ,   15 ,   16 ,   17 ,   49 ,   50 ,   51 ,   52,   0.2, 0.3)
  ) %>% group_by(Country, Year, matrix)
  mats <- collapse_to_matrices(tidy, matnames = "matrix", rownames = "row", colnames = "col",
                                 rowtypes = "rowtype", coltypes = "coltype",
                                 values = "vals") %>%
    ungroup
  # For the first tests, do not drop 0 values.
  A <- expand_to_tidy(mats, matnames = "matrix", matvals = "vals",
                      rownames = "rows", colnames = "cols",
                      rowtypes = "rt",   coltypes = "ct")
  expect_equal((A %>% filter(Country == "GH", Year == 1971, matrix == "U", rows == "p1", cols == "i1"))$vals[[1]], 11)
  expect_equal((A %>% filter(Country == "GH", Year == 1971, matrix == "U", rows == "p1", cols == "i2"))$vals[[1]], 0)
  expect_equal((A %>% filter(Country == "GH", Year == 1971, matrix == "U", rows == "p2", cols == "i1"))$vals[[1]], 0)
  expect_equal((A %>% filter(Country == "GH", Year == 1971, matrix == "U", rows == "p2", cols == "i2"))$vals[[1]], 12)
  expect_equal((A %>% filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i1"))$vals[[1]], 13)
  expect_equal((A %>% filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i2"))$vals[[1]], 0)
  expect_equal((A %>% filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i3"))$vals[[1]], 0)
  expect_equal((A %>% filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i1"))$vals[[1]], 0)
  expect_equal((A %>% filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i2"))$vals[[1]], 14)
  expect_equal((A %>% filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i3"))$vals[[1]], 15)
  expect_equal((A %>% filter(Country == "GH", Year == 1971, matrix == "V", rows == "i1", cols == "p1"))$vals[[1]], 16)
  expect_equal((A %>% filter(Country == "GH", Year == 1971, matrix == "V", rows == "i1", cols == "p2"))$vals[[1]], 0)
  expect_equal((A %>% filter(Country == "GH", Year == 1971, matrix == "V", rows == "i2", cols == "p1"))$vals[[1]], 0)
  expect_equal((A %>% filter(Country == "GH", Year == 1971, matrix == "V", rows == "i2", cols == "p2"))$vals[[1]], 17)

  expect_equal((A %>% filter(Country == "US", Year == 1980, matrix == "U", rows == "p1", cols == "i1"))$vals[[1]], 49)
  expect_equal((A %>% filter(Country == "US", Year == 1980, matrix == "U", rows == "p1", cols == "i2"))$vals[[1]], 50)
  expect_equal((A %>% filter(Country == "US", Year == 1980, matrix == "Y", rows == "p1", cols == "i1"))$vals[[1]], 51)
  expect_equal((A %>% filter(Country == "US", Year == 1980, matrix == "Y", rows == "p1", cols == "i2"))$vals[[1]], 0)
  expect_equal((A %>% filter(Country == "US", Year == 1980, matrix == "Y", rows == "p2", cols == "i1"))$vals[[1]], 0)
  expect_equal((A %>% filter(Country == "US", Year == 1980, matrix == "Y", rows == "p2", cols == "i2"))$vals[[1]], 52)

  expect_equal((A %>% filter(Country == "GH", matrix == "eta"))$vals[[1]], 0.2)
  expect_equal((A %>% filter(Country == "US", matrix == "eta"))$vals[[1]], 0.3)

  # For this second set of tests, drop 0 values.
  B <- expand_to_tidy(mats, matnames = "matrix", matvals = "vals",
                      rownames = "rows", colnames = "cols",
                      rowtypes = "rt",   coltypes = "ct",
                      drop = 0)

  expect_equal((B %>% filter(Country == "GH", Year == 1971, matrix == "U", rows == "p1", cols == "i1"))$vals[[1]], 11)
  expect_error((B %>% filter(Country == "GH", Year == 1971, matrix == "U", rows == "p1", cols == "i2"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% filter(Country == "GH", Year == 1971, matrix == "U", rows == "p2", cols == "i1"))$vals[[1]],
               "subscript out of bounds")
  expect_equal((B %>% filter(Country == "GH", Year == 1971, matrix == "U", rows == "p2", cols == "i2"))$vals[[1]], 12)
  expect_equal((B %>% filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i1"))$vals[[1]], 13)
  expect_error((B %>% filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i2"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i3"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i1"))$vals[[1]],
               "subscript out of bounds")
  expect_equal((B %>% filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i2"))$vals[[1]], 14)
  expect_equal((B %>% filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i3"))$vals[[1]], 15)
  expect_equal((B %>% filter(Country == "GH", Year == 1971, matrix == "V", rows == "i1", cols == "p1"))$vals[[1]], 16)
  expect_error((B %>% filter(Country == "GH", Year == 1971, matrix == "V", rows == "i1", cols == "p2"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% filter(Country == "GH", Year == 1971, matrix == "V", rows == "i2", cols == "p1"))$vals[[1]],
               "subscript out of bounds")
  expect_equal((B %>% filter(Country == "GH", Year == 1971, matrix == "V", rows == "i2", cols == "p2"))$vals[[1]], 17)

  expect_equal((B %>% filter(Country == "US", Year == 1980, matrix == "U", rows == "p1", cols == "i1"))$vals[[1]], 49)
  expect_equal((B %>% filter(Country == "US", Year == 1980, matrix == "U", rows == "p1", cols == "i2"))$vals[[1]], 50)
  expect_equal((B %>% filter(Country == "US", Year == 1980, matrix == "Y", rows == "p1", cols == "i1"))$vals[[1]], 51)
  expect_error((B %>% filter(Country == "US", Year == 1980, matrix == "Y", rows == "p1", cols == "i2"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% filter(Country == "US", Year == 1980, matrix == "Y", rows == "p2", cols == "i1"))$vals[[1]],
               "subscript out of bounds")
  expect_equal((B %>% filter(Country == "US", Year == 1980, matrix == "Y", rows == "p2", cols == "i2"))$vals[[1]], 52)

  expect_equal((B %>% filter(Country == "GH", matrix == "eta"))$vals[[1]], 0.2)
  expect_equal((B %>% filter(Country == "US", matrix == "eta"))$vals[[1]], 0.3)

})
