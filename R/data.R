#' @export
mat_df_trim_quant <- function(df, .value_var, ..., .probs =c(0.02, 0.98), na.rm = TRUE, .rem_quants = TRUE) {
  by_vars <- quos(...)
  value_vari <- enquo(.value_var)

  res <- df %>%
    # nest(-!!!by_vars) %>%
    group_by(!!!by_vars) %>%
    mutate(q_L = quantile(!!value_vari, probs = .probs[1], na.rm = na.rm),
           q_H = quantile(!!value_vari, probs = .probs[2], na.rm = na.rm)) %>%
    ungroup() %>%
    filter(  q_L < !!value_vari & !!value_vari < q_H)

  if(.rem_quants) res <-  res %>%
    select(-q_L, -q_H)
  res

}



#' @export
mat_add_total_row <- function(x) {
  x %>%
    bind_rows(summarise_all(x, funs(if(is.numeric(.)) sum(., na.rm=TRUE) else if(is.logical(.)) NA else "Total")))
}



#' Spread data with TRUE/FALSE (from count)
#' @export
mat_spread_TR_FALSE <- function(df, col, n_col = n) {
  col_here <- enquo(col)
  n_col_here <- enquo(n_col)

  df %>%
    spread(!!col_here, !!n_col_here, fill = 0) %>%
    mutate(perc = 100 * `TRUE` /(`TRUE`+ `FALSE`))

}

#' Remove columns with only one value
#' @export
mat_remo_cols_1val <-  function(x) select(x, -which(map_int(x, n_distinct)==1))

#' Add percentage column
#' @export
#' @examples
#'   df <- tibble(group = rep(letters[1:2], each = 2),
#'   n = c(3, 2,3, 5))
#'   df %>% add_perc()
#'   df %>% add_perc(group)
#'   df %>% rename(N=n) %>% add_perc(.name = N)
#'   df %>% rename(N=n) %>% add_perc(group, .name = N)
mat_add_perc <- function(x, ..., .name =n) {
  group_var <- quos(...)
  .name2 = enquo(.name)

  if(is_grouped_df(x)) {
    warning("Data already grouped, not over-writing!")
    res <- x %>%
      mutate(perc = 100 * !!.name2/sum(!!.name2))
  } else {
    res <- x %>%
      group_by(!!! group_var) %>%
      mutate(perc = 100 * !!.name2/sum(!!.name2)) %>%
      ungroup()
  }
  res
}







#' Use one_of() quietly
#' @export
#' @examples
#' iris %>% as_tibble() %>%
#' select(one_of_quiet(c("Sepal.Length", "caca")))
mat_one_of_quiet <- function(x) quietly(one_of)(x, .vars= tidyselect::peek_vars())$result

