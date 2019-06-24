
#' @export
as.data.frame.density <- function(x, ...) data.frame(x=x$x, y=x$y, ...)

#' Convert quantile() output to df
#' @param x output from quantile
#' @param long Use long format?
#' @examples
#' X <- runif(100)
#' q <- quantile(X, probs= c(0.001, 0.975))
#' mat_quant_to_df(q)
#' mat_quant_to_df(q, long=TRUE)
#' @export
mat_quant_to_df <- function(x, long=FALSE) {
  q_vals <- stringr::str_remove(names(x), "%")
  res <- x %>%
    bind_rows() %>%
    magrittr::set_colnames(paste("q_", stringr::str_remove(q_vals, "\\."), sep=""))
  if(long){
    res <- res %>%
      gather("quantile", "value", everything()) %>%
      mutate(quantile = as.numeric(q_vals))
  }
  res
}

#' @export
#' @rdname mat_quant_to_df
quant_to_df <-  function(x) .Deprecated("mat_quant_to_df")



#' @export
as.data.frame.histogram <- function(x, ...) data.frame(mids=x$mids,
                                                       counts=x$counts,
                                                       density=x$density,
                                                       ...)


if(FALSE){

  density(X) %>% as_tibble()
  hist(X, breaks=seq(0,1, by=0.1)) %>% as_tibble()

}


#' Convert correlation object to df
#'
#' @param x object from cor
#' @param long into long format?
#' @examples
#' library(magrittr)
#' library(dplyr)
#' data(iris_tb)
#' iris_tb %>%
#'   select(-Species) %>%
#'   cor()  %>%
#'   mat_cor_to_df()
#' @export
mat_cor_to_df <- function(x, long=TRUE)  {
  rownames_x <- rownames(x)
  if(long) x[!lower.tri(x)] <- 999
  res <- as_tibble(x) %>%
    mutate(variable= factor(rownames_x, ordered= TRUE, levels = rownames_x)) %>%
    select(.data$variable, tidyselect::everything())
  if(long) {
    res <- res %>%
      gather("variable2", "value", -.data$variable) %>%
      filter(.data$value!=999) %>%
      mutate(variable2 = factor(.data$variable2, ordered= TRUE, levels = rownames_x))
  }
  res
}
