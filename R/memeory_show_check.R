#' Show memory usage, by object
#' @export
#' @examples
#' mat_show_mem()
mat_show_mem <-  function() {
  tibble(obj = ls(.GlobalEnv)) %>%
    mutate(obj_size = map(.data$obj, ~pryr::object_size(get(.))),
           size = map_dbl(.data$obj_size, ~.),
           unit = map_chr(.data$obj_size, ~class(.)),
           class_1 = map_chr(.data$obj, ~ class(get(.))[1])) %>%
    arrange(desc(.data$size))
}


