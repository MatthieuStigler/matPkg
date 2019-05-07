#' Crossing with helpers
#'
#' @param df data
#' @param args a list, names, with values. Special are .each, and .all
#' @param wide to wide?
#' @export
#' @examples
#' mat_cross_helper(warpbreaks, list(wool=c("A"),
#'                            tension = c("L", "H")))
#' mat_cross_helper(warpbreaks, list(wool=c("A"),
#'                            tension = c(".each")))

mat_cross_helper <- function(df, args, wide = FALSE) {

  vars <- names(args)
  if(any(is.na(vars))) stop("Give me names")

  ## 1 get value table
  vals_table <- mat_vars_uniques(df, vars, wide=FALSE)
  vars

  ## as tibble
  args_df <- tibble(variable=names(args),
                  values=map(args, as_tibble)) %>%
    unnest(.data$values)

  ## replace now
  args_df_add <- args_df %>%
    mutate(values_new = map2(.data$variable, value , ~ check_replace(.y, filter(vals_table, .data$variable==.x) %>%
                                                                 pull(.data$values)) %>%
                               tibble::enframe(value = "values_new"))) %>%
    unnest(.data$values_new) %>%
    select(-.data$name)

  ## to list
  args_add_li <- args_df_add %>%
    select(-value) %>%
    dplyr::distinct(.data$variable, .data$values_new) %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::mutate(n_obs = 1:n()) %>%
    dplyr::ungroup() %>%
    spread(.data$variable, .data$values_new) %>%
    dplyr::select(-.data$n_obs) %>%
    as.list() %>%
    map( ~ .[!is.na(.)])

  res <-  purrr::lift(tidyr::crossing)(args_add_li)
  if(!wide) { res <- res %>%
    mutate(n_restr = 1:n()) %>%
    gather("variable", "value", -.data$n_restr) %>%
    dplyr::arrange(.data$n_restr)
  }

  res
}


check_replace <- function(x_target, x_values ) {
  if(x_target ==".each") {
    x <- x_values
  } else if(x_target ==".all") {
    x <- paste(x_values, collapse = "|")
  } else {
    check <- x_target%in%x_values
    if(!check) warning(paste(x_target, "Not found"))
    x <- x_target
  }
  x
}


if(FALSE) {

mat_cross_helper(warp, list(wool=c(".each"),
                     tension = c(".all", ".each")), wide=TRUE) %>%
  mutate(data = map2(tension, wool, ~filter(warp, str_detect(tension, .x) & str_detect(wool, .y))),
         n_wool = map_int(data, ~dplyr::n_distinct(.$wool)),
         n_tension = map_int(data, ~dplyr::n_distinct(.$tension)))

}
