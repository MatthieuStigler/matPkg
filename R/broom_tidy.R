

#' @export
mat_lm_means_tidy <-  function(x, val_name = value, clean = TRUE) {
  require(broom)
  val_name_pr <-  enquo(val_name)
  
  res <- x %>%
    mutate(reg = map(data, ~lm(value ~ 1, data = rename(., value = !!val_name_pr))),
           n = map_int(data, nrow)) %>%
    ungroup() %>%
    mutate(reg_out = map(reg, ~tidy(., conf.int = TRUE))) %>%
    unnest(reg_out) %>%
    select(-reg, -data, -term)
  if(clean) res <-  res %>%
    setNames(str_replace(colnames(.), "\\.", "_"))
  res
}

#' Tidy columns: . to _
#' @export
mat_tidy_clean <- function(x) setNames(x, str_replace(colnames(x), "\\.", "_"))