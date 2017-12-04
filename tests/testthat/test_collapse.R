# Contains tests for the matsindf package.

# Need to put dplyr before testthat.
# If not, the "matches" function in dplyr overrides the "matches" function in testthat,
# and tests containing the string "(" don't work as expectged.

library(dplyr)
library(magrittr)
library(testthat)

###########################################################
context("Collapse")
###########################################################

testthat("collapse_to_matrices works as expected", {
  ptype <- "Products"
  itype <- "Industries"
  tidy <- data.frame(Country = c( "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "US",  "US",  "US",  "US", "GH", "US"),
                    Year    = c( 1971,  1971,  1971,  1971,  1971,  1971,  1971,  1980,  1980,  1980,  1980, 1971, 1980),
                    matrix  = c(   "U",   "U",   "E",   "E",   "E",   "V",   "V",   "U",   "U",   "E",   "E", "eta", "eta"),
                    row     = c( "c 1", "c 2", "c 1", "c 2", "c 2", "i 1", "i 2", "c 1", "c 1", "c 1", "c 2", NA, NA),
                    col     = c( "i 1", "i 2", "i 1", "i 2", "i 3", "c 1", "c 2", "i 1", "i 2", "i 1", "i 2", NA, NA),
                    rowtype = c(ptype, ptype, ptype, ptype, ptype, itype, itype, ptype, ptype, ptype, ptype, NA, NA),
                    coltype = c(itype, itype, itype, itype, itype, ptype, ptype, itype, itype, itype, itype, NA, NA),
                    vals  = c(   11  ,  22,    11 ,   22 ,   23 ,   11 ,   22 ,   11 ,   12 ,   11 ,   22,   0.2, 0.3)
  ) %>% group_by(Country, Year, matrix)
  mats <- collapse_to_matrices(tidy, matnames = "matrix", rownames = "row", colnames = "col",
                                                               rowtypes = "rowtype", coltypes = "coltype",
                                                               values = "vals")
  mats %>% spread(key = matrix, value = vals)
})
