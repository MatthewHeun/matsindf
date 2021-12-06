library(dplyr)
library(tidyr)
library(usethis)

filter <- dplyr::filter

data(iris)

long_iris <- iris %>%
  mutate(flower = sprintf("flower_%d", 1:nrow(iris))) %>%
  pivot_longer(
    cols = c(-Species, -flower), names_to = "dimension", values_to = "length"
  ) %>%
  rename(species = Species) %>%
  select(flower, species, dimension, length) %>%
  mutate(species = as.character(species))

usethis::use_data(long_iris, overwrite = TRUE)
