#' Compute means by lm, tidy, unnest
#'
#' This returns a clean df of means for each group
#' @param df_nest a nested df
#' @param val_name name of value column
#' @param clean Clean the broom colnames?
#' @examples
#' library(magrittr)
#' data(iris_tb)
#' iris_tb %>%
#'   tidyr::nest(-Species) %>%
#'   mat_lm_means_tidy(Petal.Width)
#' @export
mat_lm_means_tidy <-  function(df_nest, val_name = value, clean = TRUE) {
  val_name_pr <-  rlang::enquo(val_name)

  res <- df_nest %>%
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
#' @examples
#' reg <- lm(freeny)
#' out_1 <- broom::tidy(reg)
#' out_2 <- broom::glance(reg)
#' mat_tidy_clean(out_1)
#' mat_tidy_clean(out_2)
mat_tidy_clean <- function(df) {
  df %>%
    setNames(stringr::str_replace_all(colnames(df), "\\.", "_")) %>%
    mutate_at(vars(mat_one_of_quiet('term')),  ~str_replace(., "\\(Intercept\\)", "Intercept"))
}


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


#' Use tidy() with robust vcov directly
#' @param x object for which hopfully there is a coeftst/vcov method
#' @param conf.int return confindence interval? Determined by level
#' @param df,parm,level,vcov. arguments to \code{\link{coeftest}}
#' @param \ldots passed to vcov (well to coeftest who does the jo)
#' @examples
#' library(sandwich)
#' reg <-  lm(freeny)
#' standard_me <- mat_tidy_robust(reg)
#' standard_br <- broom::tidy(reg, conf.int = TRUE)
#' all.equal(as.data.frame(standard_me), as.data.frame(standard_br))

#' mat_tidy_robust(reg, vcov. = vcovHC(reg))
#' mat_tidy_robust(reg, vcov. = vcovHC)
#' @export
mat_tidy_robust <-  function(x, vcov. = NULL, conf.int = TRUE, df = NULL, parm = NULL, level = 0.95, ...) {
  res <- lmtest::coeftest(x=x, vcov. = vcov., df = df, ...) %>%
    broom::tidy()

  if(conf.int) {
    ci <- lmtest::coefci(x=x, parm = parm, level = level, vcov. = vcov., df = df, ...) %>%
      as_tibble()
    colnames(ci) <- c("conf.low",  "conf.high")
    res  <- res %>%
      dplyr::bind_cols(ci)
  }
  res
}

#' Tidy on a df of regs with col reg_out
#'
#' @param df data
#' @param reg_col name of column with regs?
#' @param \ldots futher arguments passed to tidy
#' @export
#' @examples
#' library(purrr)
#' library(dplyr)
#' library(tidyr)
#'
#' iris_regs <- nest(iris, data=-Species) %>%
#' mutate(reg_out = map(data, ~lm(Petal.Width~Petal.Length, data=as_tibble(.)))) %>%
#' select(-data)
#'
#' mat_tidy_do(df=iris_regs)
mat_tidy_do <- function(df, reg_col = reg_out, ...) {
  df %>%
    mat_add_row_num(col_name = "n_reg") %>%
    mutate(coef_out = map(!!rlang::enquo(reg_col), function(x) broom::tidy(x, conf.int=TRUE, ...))) %>%
    select(-!!enquo(reg_col)) %>%
    unnest(.data$coef_out) %>%
    mat_tidy_clean() %>%
    select("n_reg", tidyselect::everything())
}


