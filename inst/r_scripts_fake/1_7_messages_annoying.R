#' ---
#' Title: "Annoying message"
#' Author: "Matthieu"
#' Date: 2019-03-11
#' runMat: TRUE
#' ---


################################
#'## Read data
################################

library(tidyverse)
file_tmp <- tempfile()
write_csv(iris, file_tmp)
a <- read_csv(file_tmp)
file.remove(file_tmp)
