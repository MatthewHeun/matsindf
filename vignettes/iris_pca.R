## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
theme_set(cowplot::theme_cowplot())
filter <- dplyr::filter
library(matsindf)

## -----------------------------------------------------------------------------
data(long_iris)
print(long_iris, n = 5)

## -----------------------------------------------------------------------------
long_pca_embeddings <- long_iris %>%
  collapse_to_matrices(
    rownames = "flower", colnames = "dimension", matvals = "length"
  ) %>%
  transmute(projection = lapply(length, function(mat)
    prcomp(mat, center = TRUE, scale = TRUE)$x
  )) %>%
  expand_to_tidy(
    rownames = "flower", colnames = "component", matvals = "projection"
  )
print(as_tibble(long_pca_embeddings), n = 5)

## -----------------------------------------------------------------------------
long_pca_withspecies <- long_iris %>%
  select(flower, species) %>%
  distinct() %>%
  left_join(long_pca_embeddings, by = "flower")

## -----------------------------------------------------------------------------
long_pca_withspecies %>%
  pivot_wider(
    id_cols = c(flower, species), names_from = component,
    values_from = projection
  ) %>%
  ggplot(aes(x = PC1, y = PC2, colour = species)) + 
  geom_point() +
  coord_equal()

