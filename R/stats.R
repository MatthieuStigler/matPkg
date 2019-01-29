## nlme get ratio
get_sds_lme <- function(x) {
  x_sd <- as.numeric(nlme::VarCorr(x)[,2])
  tibble(sd_b=x_sd[1],
         sd_w = x_sd[2],
         ratio=x_sd[1]/x_sd[2])
}
