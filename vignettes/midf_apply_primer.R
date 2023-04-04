## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(dplyr)
library(matsbyname)
library(matsindf)
library(tidyr)

## -----------------------------------------------------------------------------
example_fun <- function(a, b){
  return(list(c = matsbyname::sum_byname(a, b), 
              d = matsbyname::difference_byname(a, b)))
}

## -----------------------------------------------------------------------------
matsindf_apply(FUN = example_fun, a = 2, b = 1)

## -----------------------------------------------------------------------------
tryCatch(
  matsindf_apply(FUN = example_fun, a = 2, b = 1, z = 2),
  error = function(e){e}
)

## -----------------------------------------------------------------------------
tryCatch(
  matsindf_apply(FUN = example_fun, a = 2),
  error = function(e){e}
)

