#' Unest safely
#'
#' @param df data
#' @param col_name which column contains the safely output?
#' @param result_name How to call output column with result?
#' @export
#' @examples
#' library(purrr)
#' library(dplyr)
#' df <- data.frame(a=c(1, NA, -2))
#' df_out <- mutate(df, out= map(a, ~safely(~log2(., arg = FALSE))(.)))
#' mat_safely_unnest(df_out, out)
#'
mat_safely_unnest <-  function(df, col_name, result_name = "result") {
  col_name <- rlang::enquo(col_name)
  result_namei <-  rlang::enquo(result_name)

  df %>%
    bind_cols(purrr::transpose(pull(., !!col_name)) %>%  as_tibble) %>%
    select(-!!col_name) %>%
    mutate(has_error = map_lgl(.data$error, ~ length(.)>0),
           error = map_chr(.data$error, ~if(length(.)==0) NA_character_ else to_1(.))) %>%
    rename(!!result_namei := .data$result)
}

to_1 <-  function(x) paste(unique(x), collapse = " AND ")
