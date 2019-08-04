#' Plot coefficients from tidy output
#'
#' @param df Data-frame
#' @param fill_var variable for \code{fill}
#' @param fac1_var,fac2_var facet variable
#' @param angle angle in \code{element_text()}
#' @param x_var x variable, default term
#' @param scales argument to \code{facet_grid}
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
mat_plot_coefs_tidy <- function(df, fill_var=term, fac1_var=NULL, fac2_var=NULL, angle = 0, x_var=term, scales = "fixed") {

  print(quo_is_null(enquo(fac1_var)))
  print(quo_is_null(enquo(fac2_var)))

  if(quo_is_null(enquo(fac1_var))) fac1_var_b <- missing_arg() else fac1_var_b <- fac1_var
  if(quo_is_null(enquo(fac2_var))) fac2_var_b <- missing_arg() else fac2_var_b <- fac2_var

  ## Check if unique?
  df_check <- df %>%
    dplyr::count(!!enquo(x_var), !!enquo(fill_var), {{fac1_var_b}}, !!enquo(fac2_var_b))

  print("OK")
  ## if not unique: print message
  if(any(df_check$n>1)) {
    warning("Problem: too many values!?")
    hideMe <- df %>%
      mat_tidy_keep_estimate() %>%
      dplyr::select(-.data$estimate) %>%
      mat_is_unique_combo(!!enquo(x_var), !!enquo(fill_var), !!enquo(fac1_var), !!enquo(fac2_var), .print=TRUE)
  }

  print("OK")
  out <- df %>%
    ggplot2::ggplot(aes(x=!!enquo(x_var), y= .data$estimate, fill=!!enquo(fill_var), colour=!!enquo(fill_var)))+
    ggplot2::geom_col(position = "dodge") +
    ggplot2::theme(axis.text.x  = ggplot2::element_text(angle = angle)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_errorbar(aes(ymin=.data$conf_low, ymax = .data$conf_high), position = "dodge", colour = I("black")) +
    ggplot2::xlab(rlang::quo_text(enquo(x_var))) + ggplot2::ylab("Coefficient")

  print("OK")
  ## facet_grid: needs 3,2
  if(is.null(!!enquo(fac1_var)) & is.null(!!enquo(fac2_var)))  {
    if(packageVersion("ggplot2")<"3.2.0") warning("Needs ggplot 3.2")
    return(out)
  }

  print("OK")
  out + gr

}


check <- function(df, fill_var=term, fac1_var=NULL, fac2_var=NULL, angle = 0, x_var=term, scales = "fixed") {

  print(rlang::is_empty(fac1_var))
  print(rlang::is_missing(fac1_var))
  ## Check if unique?
  df_check <- df %>%
    dplyr::count(!!enquo(x_var), !!enquo(fill_var), !!enquo(fac1_var))
}

check(coefs_out_iris)


library(dplyr)

dt <- data.frame(a = sample(LETTERS[1:2], 100, replace = TRUE), b = sample(LETTERS[3:4], 100, replace = TRUE), value = rnorm(100,5,1))

f1 <- function(dt, a, b, c) {
  dt %>%
    mutate(c = ifelse(is_empty(c)==TRUE,NA,c)) %>%
    group_by(a, b,c) %>%
    summarise(mean = mean(value))
}

f1(dt, a = "a", b = "b",c=NULL)

library(rlang)

f1 <- function(dt, a, b, c) {

  # print(is_empty(c))
  # print(is_missing(c))
  if(!is_missing(c) && is_empty(c)) c <- missing_arg()
  # print(is_missing(c))

  dt %>%
    # mutate(c = ifelse(is_empty(c)==TRUE,NA,c)) %>%
    group_by(a, b, {{c}}) %>%
    summarise(mean = mean(value))
}

f1(dt, a = "a", b = "b")
f1(dt, a = "a", b = "b", c=NULL)


f2 <- function(dt, group_var1=a,  group_var2=NULL) {

  group_var2_orig <- quo_get_expr(quo(group_var2))
  print(quo_is_null(enquo(group_var2)))
  if(quo_is_null(enquo(group_var2))) group_var2 <- missing_arg()

  res <- dt %>%
    count({{group_var1}}, {{group_var2}})
  print(res)
  ggplot(aes(x=a, y=n), data = res)+
    geom_col()+
    facet_grid(row={{group_var2_orig}})
    # group_by({{group_var1}}, !!enquo(group_var2)) %>%
    # summarise(mean = mean(value))
}

f2(dt, group_var1 = a)
f2(dt, group_var1 = a, group_var2=NULL)
f2(dt, group_var1 = a, group_var2=b)


## wait for:
