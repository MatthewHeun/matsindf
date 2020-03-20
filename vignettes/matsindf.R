## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(dplyr)
library(ggplot2)
library(matsbyname)
library(matsindf)
library(tidyr)
library(tibble)

## -----------------------------------------------------------------------------
head(UKEnergy2000, 2)

## -----------------------------------------------------------------------------
UKEnergy2000_with_metadata <- UKEnergy2000 %>% 
  # Add a column indicating the matrix in which this entry belongs (U, V, or Y).
  matsindf:::add_UKEnergy2000_matnames(.) %>% 
  # Add columns for row names, column names, row types, and column types.
  matsindf:::add_UKEnergy2000_row_col_meta(.) %>% 
  mutate(
    # Eliminate columns we no longer need
    Ledger.side = NULL,
    Flow.aggregation.point = NULL,
    Flow = NULL,
    Product = NULL, 
    # Ensure that all energy values are positive, as required for analysis.
    E.ktoe = abs(E.ktoe)
  )
head(UKEnergy2000_with_metadata, 2)

