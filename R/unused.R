as.tbl.data.table <- function(x) as_tibble(data.table::setDT(x))

# add_sum <- function(x, ..., .name =n) {
#   group_var <- quos(...)
#   .name2 = rlang::enquo(.name)
#
#   if(dplyr::is_grouped_df(x)) {
#     warning("Data already grouped, not over-writing!")
#     res <- x %>%
#       mutate(n = sum(!!.name2))
#   } else {
#     res <- x %>%
#       group_by(!!! group_var) %>%
#       mutate(n = sum(!!.name2)) %>%
#       ungroup()
#   }
#   res
# }



quietly_unnest <-  function(x, col_name) {
  col_name <- rlang::enquo(col_name)

  x %>%
    bind_cols(purrr::transpose(pull(., !!col_name)) %>%  as_tibble) %>%
    select(-!!col_name) %>%
    mutate(warnings = map_chr(.data$warnings, ~if(length(.)==0) NA_character_ else to_1(.)),
           messages = map_chr(.data$messages, ~if(length(.)==0) NA_character_ else to_1(.)))
  # n_warn = map_int(warnings, length),
  # n_mess = map_int(messages, length
}

safely_unnest <-  function(x, col_name) {
  col_name <- rlang::enquo(col_name)

  x %>%
    bind_cols(purrr::transpose(pull(., !!col_name)) %>%  as_tibble) %>%
    select(-!!col_name) %>%
    mutate(error = map_chr(.data$error, ~if(length(.)==0) NA_character_ else to_1(.)))
}


to_1 <-  function(x) paste(unique(x), collapse = " AND ")
cleaner_vec <- function(x) map_chr(x, ~if(length(.)==0) NA_character_ else to_1(.))
one_of_quiet <- function(x) quietly(one_of)(x, .vars= tidyselect::peek_vars())$result

collat_unnest <-  function(x, col_name) {
  col_name <- rlang::enquo(col_name)

  output <-  purrr::transpose(pull(x, !!col_name)) %>%  as_tibble
  cols_out <-  colnames(output)
  x %>%
    bind_cols(output) %>%
    select(-!!col_name) %>%
    mutate_at(dplyr::vars(one_of_quiet(c("warnings", "messages", "error"))), cleaner_vec)

}

if(FALSE) {
  tibble(value = -1:1) %>%
    mutate(out = map(value, ~quietly(log)(.)))  %>%
    quietly_unnest(col_name = out)

  tibble(value = -1:1) %>%
    mutate(out = map(value, ~quietly(log)(.)))  %>%
    collat_unnest(col_name = out)

  tibble(value = list("a", -1, 0, 1)) %>%
    mutate(output = map(value, ~safely(log)(.)) ) %>%
    safely_unnest(col_name = output)
}
