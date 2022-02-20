library(matPkg)
library(dplyr)


##### mat_spread_TR_FALSE ####

## simple
iris %>%
  dplyr::count(is_low_6=Sepal.Length>5, Species) %>%
  mat_spread_TR_FALSE(is_low_6, n_col = n)

## prob 1: only one of TRUE/FALSE
iris %>%
  dplyr::count(is_low_6=Sepal.Length>0, Species) %>%
  mat_spread_TR_FALSE(is_low_6)

## prob 2 : bad variable specified
iris %>%
  dplyr::count(is_low_6=Sepal.Length>5, Species) %>%
  mutate(nn=n) %>%
  mat_spread_TR_FALSE(is_low_6, n_col = nn)



