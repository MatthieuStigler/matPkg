#' ---
#' Title: "Heavy code"
#' Author: "Matthieu"
#' Date: 2019-03-11
#' runMat: FALSE
#' ---


################################
#'## Read data
################################

heavy_vec <- rnorm(20000000, mean = 1)
# heavy_vec <- rnorm(10000000, mean = 1)
pryr::object_size(heavy_vec)
mean(heavy_vec)

