## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(dplyr)
library(tibble)
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
    rownames = case_when(
      # The U matrix is Product by Industry.
      UVY == "U" ~ Product,
      # The V matrix is Industry by Product.
      UVY == "V" ~ Flow,
      # The Y matrix is Product by Sector.
      UVY == "Y" ~ Product,
      TRUE ~ NA_integer_
    ), 
    colnames = case_when(
      # The U matrix is Product by Industry.
      UVY == "U" ~ Flow,
      # The V matrix is Industry by Product.
      UVY == "V" ~ Product,
      # The Y matrix is Product by Sector.
      UVY == "Y" ~ Flow,
      TRUE ~ NA_integer_
    )
  )

