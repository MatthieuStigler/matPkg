#' Trim a df by quantiles
#'
#' @param df The df
#' @param .value_var the column of values
#' @param \ldots the group_by variables
#' @param .probs quantile probs
#' @param .rem_quants remove the quantile (i.e. trim only)?
#' @param na.rm Remove NA when computing quantile?
#' @examples
#' mat_df_trim_quant(iris, Petal.Width, Species)
#' @export
mat_df_trim_quant <- function(df, .value_var, ..., .probs =c(0.02, 0.98), na.rm = TRUE, .rem_quants = TRUE) {
  by_vars <- rlang::quos(...)
  value_vari <- rlang::enquo(.value_var)

  res <- df %>%
    # nest(-!!!by_vars) %>%
    group_by(!!!by_vars) %>%
    mutate(q_L = quantile(!!value_vari, probs = .probs[1], na.rm = na.rm),
           q_H = quantile(!!value_vari, probs = .probs[2], na.rm = na.rm)) %>%
    ungroup() %>%
    filter(  .data$q_L < !!value_vari & !!value_vari < .data$q_H)

  if(.rem_quants) res <-  res %>%
    select(-.data$q_L, -.data$q_H)
  res

}

#' Add row of total
#'
#' Add either sum, or Total (if not numeric)
#' @param df the data-frame
#' @export
#' @examples
#' data(iris)
#' iris$Species <- as.character(iris$Species)
#' mat_add_total_row(iris[1:5,])
mat_add_total_row <- function(df) {
  df %>%
    bind_rows(summarise_all(df, funs(if(is.numeric(.)) sum(., na.rm=TRUE) else if(is.logical(.)) NA else "Total")))
}



#' Spread data with TRUE/FALSE (from count)
#' @param df data
#' @param col col to spread
#' @param n_col name of n column
#' @examples
#' library(dplyr)
#' iris %>%
#'   count(is_low_6=Sepal.Length<6, Species) %>%
#'   mat_spread_TR_FALSE(is_low_6)
#' @export
mat_spread_TR_FALSE <- function(df, col, n_col = n) {
  col_here <- rlang::enquo(col)
  n_col_here <- rlang::enquo(n_col)

  T_name <- paste(rlang::quo_name(col_here), "TRUE", sep="_")
  F_name <- paste(rlang::quo_name(col_here), "FALSE", sep="_")

  df_w <- df %>%
    mutate(!!col_here := factor(as.character(!!col_here), levels = c("TRUE", "FALSE"))) %>%
    spread(!!col_here, !!n_col_here, fill = 0)
  if(nrow(df_w) == nrow(df) & length(unique(pull(df, !!col_here)))!=1) warning("Problem spreading: should remove variable?")

  if(!"TRUE" %in% colnames(df_w)) df_w <- df_w %>%
    mutate(`TRUE`=0)
  if(!"FALSE" %in% colnames(df_w)) df_w <- df_w %>%
    mutate(`FALSE`=0)
  df_w %>%
    mutate(perc = 100 * .data$`TRUE` /(.data$`TRUE`+ .data$`FALSE`)) %>%
    rename(!! T_name := .data$`TRUE`,
           !! F_name := .data$`FALSE`)

}


#' @param nval_max how many values allowed?
#' @param wide logical TRUE, if nval_max > 1, show in wide format?
#' @param show_na default TRUE. Should show all NA?
#' @export
#' @rdname mat_remo_cols_1val
mat_show_cols_1val <-  function(df, nval_max = 1, wide = TRUE, show_na = TRUE) {
  res <- df %>%
    select(which(map_int(df, dplyr::n_distinct) %in%  seq_len(nval_max))) %>%
    dplyr::distinct() %>%
    gather("var", "val", dplyr::everything()) %>%
    mutate("has_na" = is.na(.data$val)) %>%
    dplyr::arrange(.data$has_na, .data$var) %>%
    select(-.data$has_na) %>%
    dplyr::distinct()
  if(wide) {
    res <-  res %>%
      group_by(.data$var) %>%
      mutate(n_rep = paste("val", 1:n(), sep = "_"),
             n_tot = n(),
             n_na = sum(is.na(.data$val))) %>%
      ungroup() %>%
      spread(.data$n_rep, .data$val) %>%
      arrange(dplyr::desc(.data$n_tot), .data$n_na, .data$var) %>%
      select(-.data$n_tot, -.data$n_na)

  }
  if(!show_na) {
    res <-  res %>%
      dplyr::filter_at(vars(dplyr::starts_with("val")), dplyr::any_vars(!is.na(.)))
  }
  res
}


#' Remove columns with only one value
#' @param df data
#' @param \ldots variables to keep
#' @examples
#' data(quick_stats)
#' mat_show_cols_1val(quick_stats)
#' mat_show_cols_1val(quick_stats, nval_max = 2)
#' mat_show_cols_1val(quick_stats, nval_max = 5, show_na = FALSE)
#'
#' ## now remove
#' mat_remo_cols_1val(quick_stats)
#' mat_remo_cols_1val(quick_stats, geo_level)
#' @export
mat_remo_cols_1val <-  function(df, ...) {
  keep_var <- rlang::enquos(...)
  df %>%
    select(-which(map_int(df, dplyr::n_distinct)==1), !!!keep_var)
}

#' Add percentage column
#' @param df data
#' @param \ldots variables to group for
#' @param .name Name of n
#' @param warn_grouped Should warn that already grouped? Default TRUE
#' @export
#' @examples
#'  library(tibble)
#'  library(magrittr)
#'  library(dplyr)
#'   df <- tibble(group = rep(letters[1:2], each = 2),
#'   n = c(3, 2,3, 5))
#'   df %>% mat_add_perc()
#'   df %>% mat_add_perc(group)
#'   df %>% rename(N=n) %>% mat_add_perc(.name = N)
#'   df %>% rename(N=n) %>% mat_add_perc(group, .name = N)
mat_add_perc <- function(df, ..., .name =n, warn_grouped = TRUE) {
  group_var <- rlang::quos(...)
  .name2 = rlang::enquo(.name)

  if(dplyr::is_grouped_df(df)) {
    if(warn_grouped) warning("Data already grouped, not over-writing!")
    res <- df %>%
      mutate(perc = 100 * !!.name2/sum(!!.name2))
  } else {
    res <- df %>%
      group_by(!!! group_var) %>%
      mutate(perc = 100 * !!.name2/sum(!!.name2)) %>%
      ungroup()
  }
  res
}


#' Add row number
#'
#'@param df data
#'@param \ldots group by variables
#'@export
#'@examples
#'df <- data.frame(group = rep(letters[1:2], each=3), values= 1:6)
#'mat_add_row_num(df)
#'mat_add_row_num(df, group)

mat_add_row_num <- function(df, ...) {
  group_vars <- rlang::enquos(...)
  if(length(group_vars)!=0) {
    if(dplyr::is_grouped_df(df)) stop("Data already grouped, stop.")
    df  <-  df %>%
      dplyr::group_by(!!!group_vars)
  }

  df %>%
    dplyr::mutate(n_row = 1:n()) %>%
    dplyr::ungroup()

}




#' Table of values of variables
#'
#' @param df data-frame
#' @param wide Wide the dataset?
#' @param \ldots variables to keep
#' @export
#' @examples
#' mat_vars_uniques(iris)
#' data_test <- tidyr::crossing(a=letters[1:3], b=c("blue", "green"))
#' mat_vars_uniques(data_test)
#' mat_vars_uniques(df=data_test, wide=FALSE)

mat_vars_uniques <- function(df, wide=TRUE, ...) {
  group_vars <- rlang::enquos(...)
  if(length(group_vars)!=0) {
    df  <-  df %>%
      select(!!!group_vars)
  }
  df <- df %>%
    dplyr::distinct() %>%
    {tibble(variable = colnames(.), class = map(., ~tibble(values=as.character(unique(.))) %>%
                                              mutate(n_row = 1:n())))} %>%
    tidyr::unnest(class)
  if(wide) df <- df %>%
        spread(.data$variable, .data$values )

  df %>%
    select(-.data$n_row)
}



#' Use one_of() quietly
#' @param \ldots One or more character vectors.
#' @export
#' @examples
#' library(magrittr)
#' library(dplyr)
#' iris_tb  %>%
#'   select(mat_one_of_quiet(c("Sepal.Length", "caca")))
mat_one_of_quiet <- function(...) quietly(one_of)(..., .vars= tidyselect::peek_vars())$result


#' Compare list of names
#'
#' @param list_df List of df
#' @param logi return TRUE instead of class
#' @examples
#' freeny_2 <-  dplyr::rename(freeny, price.index2 = price.index)
#' li <- list(freeny, freeny_2)
#' mat_li_comp_cols(list =li)
#'@export
mat_li_comp_cols <-  function(list_df, logi = FALSE) {

  if(is.null(names(list_df))) names(list_df) <-  letters[1:length(list_df)]
  same_cols <- tibble(data =list_df,
                      dataset = names(list_df)) %>%
    mutate(col_df = map(.data$data, ~tibble(name = colnames(.), class = map_chr(., ~class(.)[1]) ))) %>%
    select(-.data$data) %>%
    unnest(.data$col_df) %>%
    spread(.data$name, class)


  same_cols2 <- same_cols %>%
    gather("variable", "class", -.data$dataset) %>%
    group_by(.data$variable) %>%
    mutate(all_there = sum(!is.na(class)),
           n_class = length(unique(class))) %>%
    ungroup() %>%
    spread(.data$dataset, class) %>%
    arrange(.data$all_there, desc(.data$n_class), .data$variable)  %>%
    select(-.data$all_there, -.data$n_class)

  if(logi) same_cols2 <- same_cols2 %>%
    mutate_at(-1, funs(ifelse(is.na(.), ., TRUE)))

  same_cols2
}

li_comp_cols <-  function(x) .Deprecated("mat_li_comp_cols")

#' Slice by group
#' @param df data
#' @param N (max) number per group
#' @param \ldots group variables
#' @export
#' @examples
#' mat_slice_by(iris, N = 10, Species)
mat_slice_by <- function(df, N=100, ...) {
  group_var <- rlang::enquos(...)
  df %>%
    group_by(!!!group_var) %>%
    dplyr::slice(1:N) %>%
    ungroup()
}

