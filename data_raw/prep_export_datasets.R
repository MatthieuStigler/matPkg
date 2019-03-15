library(tidyverse)
library(usethis)

#############################
## Import
#############################

CDL_colors <-  read_csv("data_raw/meta_table_Corrected_add_Groups_rCmC.csv")
quick_stats_t <-  read_csv("data_raw/quick_stats.csv")

#############################
## Pre-prepare
#############################

# prices_qs <-  read_csv("data_raw/quick_stats.csv")
#
# prices_qs2 <- prices_qs %>%
#   mutate(Value = str_replace(Value, "\\(D\\)", NA_character_) %>%
#            str_replace(",", "") %>%
#            as.numeric,
#          Value = rnorm(nrow(prices_qs), mean=Value, sd = 5))
#
# prices_qs2 %>%
#   filter(is.na(Value)) %>%
#   select(Value)
#
# write_csv(head(prices_qs2, 200),"/home/matifou/gitReps/my_github/matPkg/data_raw/quick_stats.csv")

#############################
## Prepare
#############################

iris_tb <-  as_tibble(iris)

quick_stats <-  quick_stats_t %>%
  setNames(str_replace_all(colnames(.), " ", "_") %>%
           str_to_lower())

quick_stats


#############################
## export
#############################


use_data(CDL_colors, overwrite = TRUE)
use_data(iris_tb, overwrite = TRUE)
use_data(quick_stats, overwrite = TRUE)
