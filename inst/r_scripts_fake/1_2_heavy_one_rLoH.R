#' ---
#' Title: "Heavy code"
#' Author: "Matthieu"
#' Date: 2019-03-11
#' runMat: TRUE
#' ---


################################
#'## Read data
################################

library(stats)
library(base)

heavy_vec <- rnorm(1000000, mean = 1)
# heavy_vec <- rnorm(10000000, mean = 1)
pryr::object_size(heavy_vec)
mean(heavy_vec)

#library(weirdos) and also weirdos3::fo
#library(weirdos2)
