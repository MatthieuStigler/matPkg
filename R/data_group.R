#' Compute quantiles by group
#'
#' @param df The df
#' @param .value_var the column of values
#' @param \ldots the group_by variables
#' @param .probs quantile probs
#' @param .breaks the breaks to compute table over
#' @param na.rm Remove NA when computing quantile?
#' @param wide should the table be wide?
#' @examples
#' mat_group_quantile(iris, Petal.Width, Species)
#' mat_group_quantile_table(iris, Petal.Width, Species, .breaks = c(0.2, 0.5, 1, Inf))
#' @seealso \code{\link{mat_df_trim_quant}}
#' @export
mat_group_quantile <- function(df, .value_var, ..., .probs =c(0.05,0.25, 0.5, 0.75, 0.95), na.rm = TRUE) {
  group_var <- rlang::quos(...)
  df %>%
    group_by(!!! group_var) %>%
    dplyr::group_modify(~ mat_quant_to_df(stats::quantile(pull(.x, {{.value_var}}), probs = .probs, na.rm=na.rm))) %>%
    ungroup()
}

cut_var_df <- function(x, breaks) {
  table(cut(x, breaks)) %>%
    as_tibble(.name_repair = "minimal") %>%
    `colnames<-`(c("values", "n")) %>%
    mat_add_perc()
}

#' @rdname mat_group_quantile
#' @export
mat_group_quantile_table <- function(df, .value_var, ...,
                                     .breaks = c(0.05, 0.25, 0.5, 0.75, 0.95), wide=TRUE)  {
  group_var <- rlang::quos(...)
  res <- df %>%
    group_by(!!!group_var) %>%
    dplyr::group_modify(~cut_var_df(pull(., {{.value_var}}), breaks = .breaks)) %>%
    ungroup()
  if(wide) {
    res <-  res %>%
      select(-n) %>%
      spread(.data$values, .data$perc)
  }
  res
}


