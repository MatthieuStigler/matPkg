#' Crossing with helpers
#'
#' @param df data
#' @param args a list, names, with values. Special are .each, and .all
#' @param wide to wide?
#' @param remove_NA Are NAs in unequal data lenghts to be removed?
#' @param replace_all Value for the .all column
#' @export
#' @examples
#' mat_cross_helper(df=warpbreaks, args =list(wool=c("A"),
#'                            tension = c("L", "H")), wide=FALSE)
#' mat_cross_helper(warpbreaks, list(wool=c("A"),
#'                            tension = c(".each", ".all")), wide=FALSE)
#' mat_cross_helper(df=warpbreaks, args = list(wool=c("A"),
#'                            tension = c(".each", ".all")),
#'                            wide=TRUE)

mat_cross_helper <- function(df, args, wide = TRUE, remove_NA=TRUE, replace_all ="all") {

  vars <- names(args)
  names(vars) <-  vars # stupid, but for mutate_at
  if(any(is.na(vars))) stop("Give me names")

  ## get table of actual distinct values in df
  vals_table <- mat_vars_uniques(df, vars, wide=FALSE)

  ## args as tibble
  args_df <- tibble(variable=names(args),
                  values=map(args, as_tibble)) %>%
    unnest(.data$values)

  ## replace now
  args_df_add <- args_df %>%
    mutate(values_new = map2(.data$variable, value , ~ check_replace(.y, filter(vals_table, .data$variable==.x) %>%
                                                                       pull(.data$values),
                                                                     remove_NA=remove_NA) %>%
                               tibble::enframe(value = "values_new"))) %>%
    tidyr::unnest(.data$values_new) %>%
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

  ## now do the crossing!
  res <-  purrr::lift(tidyr::crossing)(args_add_li)

  ## wide eventually
  if(!wide) {
    res <- res %>%
      mutate(n_restr = 1:n()) %>%
      gather("variable", "value", -.data$n_restr) %>%
      dplyr::arrange(.data$n_restr)
  } else {
    renamit <- function(x, replace = "none") dplyr::if_else(stringr::str_detect(x, "\\|"), replace, x)
    res <- res %>%
      mutate_at(vars,  list(vals= function(x) x)) %>%
      mutate_at(vars, renamit, replace=replace_all)
  }

  res
}

## too complicated...
# mat_cross_clean <- function(df, ..., values) {
#   modif_vars  <-  enquos(...)
#   new_names <- map_chr(modif_vars, ~paste0(quo_name(.), "_vals"))
#
#   df <- df %>%
#     mutate(new_names := !!!modif_vars)
#   df
#   #
#   #   rename_at(vars(!!!modif_vars, ~paste(., "val", sep="_"))) %>%
#     # mutate(exclude = if_else(str_detect(exclude_vals, "\\|"), "none", exclude_vals),
#
# }
#
# mat_cross_clean(res, tension, wool)


# Internal function, replace .all and .each
check_replace <- function(x_target, x_values, remove_NA=TRUE) {
  if(remove_NA) x_values <- x_values[!is.na(x_values)]
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
