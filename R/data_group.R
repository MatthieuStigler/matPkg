#' Compute quantiles by group
#'
#' @param df The df
#' @param .value_var the column of values
#' @param \ldots the group_by variables
#' @param .probs quantile probs
#' @param na.rm Remove NA when computing quantile?
#' @examples
#' mat_group_quantile(iris, Petal.Width, Species)
#' @seealso \code{\link{mat_df_trim_quant}}
#' @export
mat_group_quantile <- function(df, .value_var, ..., .probs =c(0.05,0.25, 0.5, 0.75, 0.95), na.rm = TRUE) {
  group_var <- rlang::quos(...)
  df %>%
    group_by(!!! group_var) %>%
    dplyr::group_modify(~ mat_quant_to_df(stats::quantile(pull(.x, {{.value_var}}), probs = .probs, na.rm=na.rm))) %>%
    ungroup()
}
