
#' @export
as.data.frame.density <- function(d, ...) data.frame(x=d$x, y=d$y, ...)

#' Convert quantile() output to df
#' @examples
#' X <- runif(100)
#' quantile(X, probs=0.975) %>% mat_quant_to_df()
#' @export
mat_quant_to_df <- function(x) {
  x %>%
    bind_rows() %>%
    set_colnames(paste("q_", str_replace_all(colnames(.), "%|\\.", ""), sep=""))
}

#' @export
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


#' @export
mat_cor_to_df <- function(x, long=TRUE)  {
  rownames_x <- rownames(x)
  if(long) x[!lower.tri(x)] <- 999
  res <- as_tibble(x) %>%
    mutate(variable= factor(rownames_x, ordered= TRUE, levels = rownames_x)) %>%
    select(variable, everything())
  if(long) {
    res <- res %>%
      gather(variable2, value, -variable) %>%
      filter(value!=999) %>%
      mutate(variable2 = factor(variable2, ordered= TRUE, levels = rownames_x))
  }
  res
}
