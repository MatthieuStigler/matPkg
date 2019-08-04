library(tidyverse)
library(usethis)
library(matPkg)

#############################
## Create
#############################

#
iris_regs <- nest(iris, data=-Species) %>%
  mutate(reg_out = map(data, ~lm(Petal.Width~Petal.Length, data=as_tibble(.)))) %>%
  select(-data)

coefs_out_iris <- mat_tidy_do(df=iris_regs)

coefs_out_iris


#############################
## Export
#############################


use_data(coefs_out_iris, overwrite = TRUE)
