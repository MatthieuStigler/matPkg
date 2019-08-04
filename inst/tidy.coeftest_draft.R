library(matPkg)
iris_tb %>%
  tidyr::nest(data = c(-Species)) %>%
  mat_lm_means_tidy(Petal.Width)

library(lmtest)
library(broom)
reg <- lm(freeny)

## beofre:
tidy(coeftest(reg), conf.int=TRUE)

tidy.coeftest <- function(x, conf.int=FALSE, conf.level = .95,...) {
  co <- as.data.frame(unclass(x))
  nn <- c("estimate", "std.error", "statistic", "p.value")[1:ncol(co)]
  ret <- fix_data_frame(co, nn)
  if(conf.int) {
    CI <- as.data.frame(confint(x, level = conf.level))
    colnames(CI) <- c("conf.low", "conf.high")
    ret <- bind_cols(ret, CI)
  }
  ret
}


tidy(coeftest(reg), conf.int=TRUE)
tidy(reg, conf.int=TRUE)

library(reprex)
reprex(venue="SO")
