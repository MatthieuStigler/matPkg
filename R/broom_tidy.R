

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


mat_tidy_coef_plot <- function(df, x_var, fill_var, fac_1, fac_2=.) {
  x_var <- rlang::enquo(x_var)
  fill_var <- rlang::enquo(fill_var)
  fac_1 <- rlang::enquo(fac_1)
  fac_2 <- rlang::enquo(fac_2)


  df %>%
    ggplot(aes(x = !!x_var, y = estimate, fill = !!fill_var)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(ymin =  conf.low, ymax =  conf.high, colour = I("black")),
                  position = "dodge")+
    facet_grid(!!fac_1~!!fac_2, scales = "free")

}
