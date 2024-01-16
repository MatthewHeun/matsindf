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
  matsindf:::add_UKEnergy2000_matnames() %>% 
  # Add columns for row names, column names, row types, and column types.
  matsindf:::add_UKEnergy2000_row_col_meta() %>% 
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

## -----------------------------------------------------------------------------
EnergyMats_2000 <- UKEnergy2000_with_metadata %>% 
  group_by(Country, Year, matname) %>% 
  collapse_to_matrices(matnames = "matname", matvals = "E.ktoe",
                       rownames = "rowname", colnames = "colname", 
                       rowtypes = "rowtype", coltypes = "coltype") %>% 
  rename(matrix.name = matname, matrix = E.ktoe)

# The remaining columns are Country, Year, matrix.name, and matrix
glimpse(EnergyMats_2000)

# To access one of the matrices, try one of these approaches:
(EnergyMats_2000 %>% filter(matrix.name == "U"))[["matrix"]] # The U matrix

EnergyMats_2000$matrix[[2]] # The V matrix

EnergyMats_2000$matrix[[3]] # The Y matrix

## -----------------------------------------------------------------------------
Energy <- EnergyMats_2000 %>% 
  # Create rows for a fictitious country "AB".
  # Although the rows for "AB" are same as the "GB" rows,
  # they serve to illustrate functional programming with matsindf.
  rbind(EnergyMats_2000 %>% mutate(Country = "AB")) %>% 
  spread(key = Year, value = matrix) %>% 
  mutate(
    # Create a column for a second year (2001).
    `2001` = `2000`
  ) %>% 
  gather(key = Year, value = matrix, `2000`, `2001`) %>% 
  # Now spread to put each matrix in a column.
  spread(key = matrix.name, value = matrix)

glimpse(Energy)

