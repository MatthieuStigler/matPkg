#' compute correlations among all variables
#'
#' @param df data
#' @param add_stars Add column \code{p_value_chr} with stars?
#' @param digits if \code{add_stars}, digits to round?
#' @param \ldots further arguments passed to \code{\link{cor.test}}
#' @examples
#' x1 <- rnorm(100)
#' x2 <- 0.2 * x1 + rnorm(100)
#' x3 <- 0.9 * x1 + rnorm(100, sd=0.1)
#' x4 <- rnorm(100)
#' x <- data.frame(x1, x2, x3, x4)
#'
#' mat_cor_do(x)
#' mat_cor_do(x, add_stars = TRUE)[, c(1,2,3,4,5,11)]
#' mat_cor_do(x, method = "kendall")
#' mat_cor_do(x, method = "kendall", alternative = "greater", add_stars = TRUE)
#'
#' ## plot
#' library(ggplot2)
#' ggplot(aes(x = variable, y= variable2, fill = estimate), data = mat_cor_do(x)) +
#'    geom_tile() +
#'    geom_text(aes(label = estimate_pval)) +
#'    scale_fill_gradient2()

#' @export
#' @seealso \code{\link{mat_cor_to_df}}
mat_cor_do <- function(df, add_stars = TRUE, digits = 2, ...){


  ## get df of variable pairs
  cols <-  colnames(df)
  cols_df <- t(utils::combn(cols, 2)) %>%
    magrittr::set_colnames(c("variable2", "variable")) %>%
    as_tibble() %>%
    select(.data$variable, .data$variable2)

  ## compute cor.test() for each, tidy, unnest
  cor_test_out <- cols_df %>%
    mutate(cor = purrr::map2(.data$variable, .data$variable2, function(x, y) stats::cor.test(x=pull(df, x),
                                                                    y=pull(df, y), ...) %>%
                        broom::tidy() %>%
                        mat_tidy_clean())) %>%
    unnest(.data$cor) %>%
    mutate_at(c("variable", "variable2"), ~factor(., ordered= TRUE, levels = cols))

  if(add_stars) {
    cor_test_out <-  cor_test_out %>%
      mutate(estimate_pval = paste0(round(.data$estimate, digits),
                                    stats::symnum(.data$p_value, corr = FALSE, na = FALSE,
                                                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                                  symbols = c("***", "**", "*", ".", " "))))
  }
  cor_test_out

}

if(FALSE) {
  library(tidyverse)
  library(matPkg)
  x <- data.frame(matrix(rnorm(500), ncol=5)) %>%
    as_tibble()
  colnames(x) <-  letters[1:5]

  mat_cor_do(x)
  mat_cor_do(x, method = "kendall")
  mat_cor_do(x, method = "kendall", alternative = "greater")


  # cor_fo(matrix(rnorm(100), ncol=2))
}
