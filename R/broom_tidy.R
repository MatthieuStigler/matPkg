

#' @export
mat_lm_means_tidy <-  function(x, val_name = value, clean = TRUE) {
  val_name_pr <-  rlang::enquo(val_name)

  res <- x %>%
    mutate(reg = map(.data$data, ~lm(value ~ 1, data = rename(., value = !!val_name_pr))),
           n = map_int(.data$data, nrow)) %>%
    ungroup() %>%
    mutate(reg_out = map(.data$reg, ~broom::tidy(., conf.int = TRUE))) %>%
    unnest(.data$reg_out) %>%
    select(-.data$reg, -.data$data, -.data$term)
  if(clean) res <-  res %>%
    setNames(stringr::str_replace(colnames(.), "\\.", "_"))
  res
}

#' Tidy columns: . to _
#' @param df data
#' @export
mat_tidy_clean <- function(df) setNames(df, stringr::str_replace(colnames(df), "\\.", "_"))
