## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
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
    # The rownames, colnames, rowtypes, and coltypes columns
    # are constructed using 
    # the matrix name column (in this case, UVY) as a key,
    # together with the knowledge that
    # U is a Product-by-Industry matrix, 
    # V is a Industry-by-Product matrix, and 
    # Y is a Product-by-Final-Demand-Sector matrix.
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
  collapse_to_matrices(matnames = "UVY", values = "E.ktoe",
                       rownames = "rownames", colnames = "colnames", 
                       rowtypes = "rowtypes", coltypes = "coltypes") %>% 
  rename(matrix.name = UVY, matrix = E.ktoe)

# The remaining columns are Country, Year, matrix.name, and matrix
glimpse(EnergyMats_2000)

# To access one of the matrices, try one of these approaches:
(EnergyMats_2000 %>% filter(matrix.name == "U"))[["matrix"]] # The U matrix

EnergyMats_2000$matrix[[2]] # The V matrix

EnergyMats_2000$matrix[[3]] # The Y matrix

## ------------------------------------------------------------------------
Energy <- EnergyMats_2000 %>% 
  # Create rows for a fictitious country "AB".
  # Although these rows are same as the "GB" rows,
  # they serve to illustrate functional programming with matsindf.
  rbind(EnergyMats_2000 %>% mutate(Country = "AB")) %>% 
  spread(key = Year, value = matrix) %>% 
  mutate(
    `2001` = `2000`
  ) %>% 
  gather(key = Year, value = matrix, `2000`, `2001`) %>% 
  # Now spread to put each matrix in a column.
  spread(key = matrix.name, value = matrix)

glimpse(Energy)

## ------------------------------------------------------------------------
Check <- Energy %>% 
  mutate(
    W = difference_byname(transpose_byname(V), U),
    # Need to change column name and type on y so it can be subtracted from row sums of W
    err = difference_byname(rowsums_byname(W), 
                            rowsums_byname(Y) %>% 
                              setcolnames_byname("Industry") %>% setcoltype("Industry")),
    EBalOK = iszero_byname(err)
  )
Check %>% select(Country, Year, EBalOK)
all(Check$EBalOK %>% as.logical())

## ------------------------------------------------------------------------
Etas <- Energy %>% 
  mutate(
    g = rowsums_byname(V),
    eta = transpose_byname(U) %>% rowsums_byname() %>% 
      hatize_byname() %>% invert_byname() %>% 
      matrixproduct_byname(g) %>% 
      setcolnames_byname("eta") %>% setcoltype("Efficiency")
  ) %>% 
  select(Country, Year, eta)

Etas$eta[[1]]

## ------------------------------------------------------------------------
etas_forgraphing <- Etas %>% 
  gather(key = matrix.names, value = matrix, eta) %>% 
  expand_to_tidy(matnames = "matrix.names", matvals = "matrix", 
                 rownames = "Industry", colnames = "etas", 
                 rowtypes = "rowtype", coltypes = "Efficiencies") %>% 
  mutate(
    # Eliminate columns we no longer need.
    matrix.names = NULL,
    etas = NULL, 
    rowtype = NULL, 
    Efficiencies = NULL
  ) %>% 
  rename(
    eta = matrix
  )

# Compare to Figure 8 of Heun, Owen, and Brockway (2017)
etas_forgraphing %>% filter(Country == "GB", Year == 2000)

## ------------------------------------------------------------------------
etas_UK_2000 <- etas_forgraphing %>% filter(Country == "GB", Year == 2000) 

etas_UK_2000 %>% 
  ggplot(mapping = aes_string(x = "Industry", y = "eta", 
                              fill = "Industry", colour = "Industry")) + 
  geom_bar(stat = "identity") +
  labs(x = NULL, y = expression(eta[UK*","*2000]), fill = NULL) + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_fill_manual(values = rep("white", nrow(etas_UK_2000))) +
  scale_colour_manual(values = rep("gray20", nrow(etas_UK_2000))) + 
  guides(fill = FALSE, colour = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1))

