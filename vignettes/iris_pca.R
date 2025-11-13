## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(datasets)
library(dplyr)
library(ggplot2)
library(matsindf)
library(tidyr)

## -----------------------------------------------------------------------------
long_iris <- datasets::iris %>%
  dplyr::mutate(flower = sprintf("flower_%d", 1:nrow(datasets::iris))) %>%
  tidyr::pivot_longer(
    cols = c(-Species, -flower), names_to = "dimension", values_to = "length"
  ) %>%
  dplyr::rename(species = Species) %>%
  dplyr::select(flower, species, dimension, length) %>%
  dplyr::mutate(species = as.character(species))

head(long_iris, n = 5)

## -----------------------------------------------------------------------------
long_pca_embeddings <- long_iris %>%
  collapse_to_matrices(
    rownames = "flower", colnames = "dimension", matvals = "length"
  ) %>%
  dplyr::transmute(projection = lapply(length, function(mat)
    stats::prcomp(mat, center = TRUE, scale = TRUE)$x
  )) %>%
  expand_to_tidy(
    rownames = "flower", colnames = "component", matvals = "projection"
  )
head(long_pca_embeddings, n = 5)

## -----------------------------------------------------------------------------
long_pca_withspecies <- long_iris %>%
  dplyr::select(flower, species) %>%
  dplyr::distinct() %>%
  dplyr::left_join(long_pca_embeddings, by = "flower")
head(long_pca_withspecies, n = 5)

## ----fig.width=7, fig.align='center', fig.retina=2----------------------------
long_pca_withspecies %>%
  tidyr::pivot_wider(
    id_cols = c(flower, species), names_from = component,
    values_from = projection
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = PC1, y = PC2, colour = species)) + 
  ggplot2::geom_point() +
  ggplot2::labs(colour = ggplot2::element_blank()) +
  ggplot2::theme_bw() +
  ggplot2::coord_equal()

