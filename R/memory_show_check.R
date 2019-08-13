#' Show memory usage, by object
#' @param df_input input df. If empty, will evaluate ls()
#' @export
#' @examples
#' mat_show_mem()
#' mat_show_mem(freeny)
#' library(purrr)
#' library(dplyr)
#' library(tidyr)
#'
#' iris_regs <- nest(iris, data=-Species) %>%
#'   mutate(reg_out = map(data, ~lm(Petal.Width~Petal.Length, data=as_tibble(.)))) %>%
#'   select(-data)
#' mat_show_mem(iris_regs$reg_out)
mat_show_mem <-  function(df_input) {

  if(missing(df_input)) {
    df_size <- tibble(object = ls(.GlobalEnv)) %>%
      mutate(obj_size = map(.data$object, ~pryr::object_size(get(., envir = .GlobalEnv)))) %>%
      mutate(class_first = map_chr(.data$object, ~ class(get(., envir = .GlobalEnv))[1]))
  } else {
    if(is.data.frame(df_input)) {
      nam <- colnames(df_input)
    } else if(is.list(df_input)){
      nam <- names(df_input)
      if(is.null(nam)) nam <- paste("object", 1:length(df_input), sep="_")
    }
    df_size <- tibble(object = nam,
                      class_first = map_chr(df_input, ~class(.)[1]),
                      obj_size = map(df_input , pryr::object_size))
  }

  df_size %>%
    mutate(object_size = map_dbl(.data$obj_size, ~.),
           unit = map_chr(.data$obj_size, ~class(.)),
           size = map_chr(.data$obj_size, ~utils::capture.output(.))) %>%
    arrange(desc(.data$object_size)) %>%
    select(.data$object, .data$size, .data$object_size, .data$unit, .data$class_first)
}


