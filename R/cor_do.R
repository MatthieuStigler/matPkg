#' compute correlations among all variables
#'
#' @param df data
#' @param \ldots further argumetns passed to \code{\link{cor.test}}
#' @examples
#' x <- data.frame(matrix(rnorm(500), ncol=5))
#' colnames(x) <-  letters[1:5]
#'
#' mat_cor_do(x)
#' mat_cor_do(x, method = "kendall")
#' mat_cor_do(x, method = "kendall", alternative = "greater")
#' @export
mat_cor_do <- function(df, ...){


  ## get df of variable pairs
  cols <-  colnames(df)
  cols_df <- t(utils::combn(cols, 2)) %>%
    magrittr::set_colnames(c("variable", "variable2")) %>%
    as_tibble()

  ## compute cor.test() for each, tidy, unnest
  cor_test_out <- cols_df %>%
    mutate(cor = purrr::map2(.data$variable, .data$variable2, function(x, y) stats::cor.test(x=pull(df, x),
                                                                    y=pull(df, y), ...) %>%
                        broom::tidy() %>%
                        mat_tidy_clean())) %>%
    unnest(.data$cor)
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
