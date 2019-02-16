library(tidyverse)
library(usethis)

#############################
## Import
#############################

CDL_colors <-  read_csv("data_raw/meta_table_Corrected_add_Groups_rCmC.csv")

#############################
## Prepare
#############################

iris_tb <-  as_tibble(iris)

#############################
## export
#############################


use_data(CDL_colors, overwrite = TRUE)
use_data(iris_tb, overwrite = TRUE)
