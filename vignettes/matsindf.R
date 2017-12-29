## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(dplyr)
library(tidyr)
library(tibble)
library(byname)
library(matsindf)

## ------------------------------------------------------------------------
head(UKEnergy2000)

## ------------------------------------------------------------------------
UKEnergy2000_with_metadata <- UKEnergy2000 %>% 
  mutate(
    # Add a column that indicates the matrix in which this entry belongs.
    UVY = case_when(
      # All negative values on the Supply side of the ledger belong in the use (U) matrix.
      Ledger.side == "Supply" & E.ktoe <= 0 ~ "U",
      # All positive values on the Supply side of the ledger belong in the make (V) matrix.
      Ledger.side == "Supply" & E.ktoe > 0 ~ "V",
      # All Consumption items belong in the final demand (Y) matrix.
      Ledger.side == "Consumption" ~ "Y", 
      # Identify any places where our logic is faulty.
      TRUE ~ NA_character_
    ),
    # Columns for rownames, colnames, rowtypes, and coltypes 
    # use the matrix name column (UVY) as their key,
    # together with the knowledge that
    # U is Product by Industry, 
    # V is Industry by Product, and 
    # Y is Product by (final demand) Sector.
    rownames = case_when(
      UVY == "U" ~ Product,
      UVY == "V" ~ Flow,
      UVY == "Y" ~ Product,
      TRUE ~ NA_character_
    ), 
    colnames = case_when(
      UVY == "U" ~ Flow,
      UVY == "V" ~ Product,
      UVY == "Y" ~ Flow,
      TRUE ~ NA_character_
    ), 
    rowtypes = case_when(
      UVY == "U" ~ "Product",
      UVY == "V" ~ "Industry",
      UVY == "Y" ~ "Product",
      TRUE ~ NA_character_
    ), 
    coltypes = case_when(
      UVY == "U" ~ "Industry",
      UVY == "V" ~ "Product",
      UVY == "Y" ~ "Sector",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(
    # Eliminate columns we no longer need
    Ledger.side = NULL,
    Flow.aggregation.point = NULL,
    Flow = NULL,
    Product = NULL, 
    # Ensure that all energy values are positive, as required for analysis.
    E.ktoe = abs(E.ktoe)
  )

## ------------------------------------------------------------------------
EnergyMats_2000 <- UKEnergy2000_with_metadata %>% 
  group_by(Country, Year, UVY) %>% 
  collapse_to_matrices(matnames = "UVY", 
                       rownames = "rownames", colnames = "colnames", 
                       rowtypes = "rowtypes", coltypes = "coltypes", 
                       values = "E.ktoe") %>% 
  rename(matrix.name = UVY, matrix = E.ktoe)

# The remaining columns are Country, Year, matrix.name, and matrix
glimpse(EnergyMats_2000)

# To access one of the matrices, try one of these approaches:
(EnergyMats_2000 %>% filter(matrix.name == "U"))[["matrix"]] # The U matrix

EnergyMats_2000$matrix[[2]] # The V matrix

EnergyMats_2000$matrix[[3]] # The Y matrix

## ------------------------------------------------------------------------
# Create rows for a fictitious country "AB"
Energy <- EnergyMats_2000 %>% 
  rbind(EnergyMats_2000 %>% mutate(Country = "AB")) %>% 
  spread(key = Year, value = matrix) %>% 
  # Add a column of multipliers to add noise.
  mutate(
    mult = runif(nrow(.), min = 0.9, max = 1.1),
    `2001` = elementproduct_byname(mult, `2000`),
    mult = NULL
  ) %>% 
  gather(key = Year, value = matrix, `2000`, `2001`) %>% 
  spread(key = matrix.name, value = matrix) %>% 
  # Check energy balance
  mutate(
    W = difference_byname(transpose_byname(V), U),
    y = rowsums_byname(Y),
    # Need to set column name and type on y so it can be subtracted from row sums of W
    err = difference_byname(rowsums_byname(W), 
                            y %>% setcolnames_byname("Industry") %>% setcoltype("Industry"))
  )
Energy %>% select(Year, Country, err) %>% arrange(Year)

