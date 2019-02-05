

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


#' For broom coef tidy output: bar plot for coefs
#'
#' @param df data
#' @param x_var var used in x axis (y is estimate)
#' @param fill_var var used to have multiple x
#' @param fac_1,fac_2 vars for facetting
#' @export
mat_tidy_coef_plot <- function(df, x_var, fill_var, fac_1=NULL, fac_2=NULL) {
  x_var <- rlang::enquo(x_var)
  fill_var <- rlang::enquo(fill_var)
  fac_1 <- rlang::enquo(fac_1)
  fac_2 <- rlang::enquo(fac_2)


  ## from: https://stackoverflow.com/questions/53218152/ggplot2-facet-grid-with-conditional-facets-and-tidy-evaluation
  if (rlang::quo_is_null(fac_1)) {
    rows <- vars()
  } else {
    rows <- vars(!!fac_1)
  }

  if (rlang::quo_is_null(fac_2)) {
    cols <- vars()
  } else {
    cols <- vars(!!fac_2)
  }

  pl <- df %>%
    ggplot(aes(x = !!x_var, y = .data$estimate, fill = !!fill_var)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(ymin =  .data$conf.low, ymax =  .data$conf.high, colour = I("black")),
                  position = "dodge")
  if(!rlang::quo_is_null(fac_1)|!rlang::quo_is_null(fac_2)) {
    pl <-  pl +
      facet_grid(rows=rows,
                 cols = cols,
                 scales = "free")
  }
  pl


}
