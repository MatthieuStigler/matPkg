#' Plot coefficients from tidy output
#'
#' @param df Data-frame
#' @param fill_var variable for \code{fill}
#' @param fac1_var,fac2_var facet variable
#' @param angle angle in \code{element_text()}
#' @param x_var x variable, default term
#' @param geom Use bar or points?
#' @param scales,fac_space argument to \code{facet_grid}
#'
#' @examples
#' data(coefs_out_iris)
#' mat_plot_coefs_tidy(coefs_out_iris, fac1_var = Species)
#' mat_plot_coefs_tidy(coefs_out_iris, fac2_var = Species)
#' mat_plot_coefs_tidy(coefs_out_iris, fac1_var = Species, fac2_var = term, scales = "free")
#'
#' ## handle duplicates
#' mat_plot_coefs_tidy(coefs_out_iris)
#' @export
mat_plot_coefs_tidy <- function(df, fill_var=term, fac1_var=NULL, fac2_var=NULL, angle = 0, x_var=term,
                                scales = "fixed", fac_space = "fixed",
                                geom= c("bar", "point")) {

  # prep args
  geom_for_coef <- switch (match.arg(geom),
                           bar = ggplot2::geom_col(position = "dodge"),
                           point=ggplot2::geom_point())

  grps <- rlang::enquos(x_var,  fill_var, fac1_var, fac2_var, .ignore_empty = "all")
  grps <- grps[purrr::map_lgl(grps, ~ !rlang::quo_is_null(.))]

  ## Check if unique?
  df_check <- df %>%
    dplyr::count(!!!grps)

  ## if not unique: print message
  if(any(df_check$n>1)) {
    warning("Problem: too many values!?")
    hideMe <- df %>%
      mat_tidy_keep_estimate() %>%
      dplyr::select(-.data$estimate) %>%
      mat_is_unique_combo(!!!grps, .print=TRUE)
  }



  out <- df %>%
    ggplot2::ggplot(aes(x=!!enquo(x_var), y= .data$estimate, fill=!!enquo(fill_var), colour=!!enquo(fill_var)))+
    geom_for_coef +
    ggplot2::theme(axis.text.x  = ggplot2::element_text(angle = angle)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::xlab(rlang::quo_text(enquo(x_var))) + ggplot2::ylab("Coefficient")

  ## add error bar if se provided
  has_se <- all(c("conf_low", "conf_high") %in% colnames(df))
  if(has_se) {
    out <- out+
      ggplot2::geom_errorbar(aes(ymin=.data$conf_low, ymax = .data$conf_high), position = "dodge", colour = I("black"))

  }

  ## facet_grid: needs 3,2
  if(rlang::quo_is_null(enquo(fac1_var)) & rlang::quo_is_null(enquo(fac2_var)))  {
    if(utils::packageVersion("ggplot2")<"3.2.0") warning("Needs ggplot 3.2")
    return(out)
  }

  out +
    facet_grid(rows= if(rlang::quo_is_null(enquo(fac1_var))) NULL else rlang::enquos(fac1_var),
               cols= if(rlang::quo_is_null(enquo(fac2_var))) NULL else rlang::enquos(fac2_var),
               scales = scales, space=fac_space)
}
## check online:
## ggplot one: https://stackoverflow.com/questions/56309158/r-ggplot2-facet-grid-with-vars-how-to-handle-missing-argument
## rlang NULL one: https://stackoverflow.com/questions/56309158/r-ggplot2-facet-grid-with-vars-how-to-handle-missing-argument
