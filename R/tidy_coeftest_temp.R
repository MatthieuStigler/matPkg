broom_copy_fix_data_frame <- function (x, newnames = NULL, newcol = "term") {
  if (!is.null(newnames) && length(newnames) != ncol(x)) {
    stop("newnames must be NULL or have length equal to number of columns")
  }
  if (all(rownames(x) == seq_len(nrow(x)))) {
    ret <- data.frame(x, stringsAsFactors = FALSE)
    if (!is.null(newnames)) {
      colnames(ret) <- newnames
    }
  }
  else {
    ret <- data.frame(...new.col... = rownames(x), broom_copy_unrowname(x),
                      stringsAsFactors = FALSE)
    colnames(ret)[1] <- newcol
    if (!is.null(newnames)) {
      colnames(ret)[-1] <- newnames
    }
  }
  as_tibble(ret)
}

broom_copy_unrowname <- function (x) {
  rownames(x) <- NULL
  x
}

tidy.coeftest <- function(x, conf.int=FALSE, conf.level = .95, ...) {
  co <- as.data.frame(unclass(x))
  nn <- c("estimate", "std.error", "statistic", "p.value")[1:ncol(co)]
  ret <- broom_copy_fix_data_frame(co, nn)
  if(conf.int) {
    if(utils::packageVersion("lmtest")<"0.9.37") {
      warning("Needs lmtest version >=0.9.37 for conf.int = TRUE")
      return(ret)
    }
    CI <- as.data.frame(stats::confint(x, level = conf.level))
    colnames(CI) <- c("conf.low", "conf.high")
    ret <- bind_cols(ret, CI)
  }
  ret
}


if(FALSE) {
  library(lmtest)
  library(matPkg)
  tidy.coeftest2(coeftest(lm(freeny)), conf.int=TRUE)

}
