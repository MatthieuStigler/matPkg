
#' Count each column separately
#'
#' @param df initial data
#' @param \ldots tidy selectors
#' @examples
#' df <- data.frame(ids = 1:100,
#'                  x1=sample(letters[1:5], 100, replace = TRUE),
#'                  x5=sample(c("TRUE", "FALSE"), 100, replace = TRUE))
#' mat_count_separate(df, starts_with("x"))
#' @export
mat_count_separate <- function(df, ...) {
  # need to find simpler way!
  cols <- dplyr::select(utils::head(df,2), ...) %>%
    colnames()
  map(cols, function(x) dplyr::select_at(df, x) %>%  dplyr::count(!!rlang::sym(x)) %>%
        rename(value=!!rlang::sym(x),
               !!rlang::sym(x):=n)) %>%
    purrr::reduce(dplyr::full_join, by = "value")
}


if(FALSE){
  library(tidyverse)
  N <-  50
  df <- data.frame(ids = 1:N,
                   x1=sample(letters[1:5], N, replace = TRUE),
                   x2=sample(letters[1:5], N, replace = TRUE),
                   x3=sample(letters[1:5], N, replace = TRUE),
                   x4=sample(letters[1:5], N, replace = TRUE),
                   x5=sample(c("TRUE", "FALSE"), N, replace = TRUE))
  mat_count_separate(df, starts_with("x"))

}
