library(tidyverse)
library(usethis)

#############################
## Import
#############################

CDL_colors <-  read_csv("data_raw/meta_table_Corrected_add_Groups_rCmC.csv")

#############################
## export
#############################


use_data(CDL_colors, overwrite = TRUE)
