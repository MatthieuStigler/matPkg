#' What are combinations of dataset?
#'
#'Check if the data is uniquely determiend by (n=1) for given variables
#' @param df The df
#' @param \ldots the group_by variables
#' @param .print Print bad output?
#' @examples
#' mat_is_unique_combo(iris, Sepal.Length, Sepal.Width, Petal.Width, Petal.Length, Species)
#' @export
mat_is_unique_combo <- function(df, ..., .print=TRUE) {
  by_quos <- rlang::enquos(..., .named = TRUE)
  df_c <- df %>%
    dplyr::add_count(!!!by_quos, name = "n_occur")
  is_unique <- all(df_c$n_occur==1)
  if(!is_unique) {
    cat("Not unique! Some combinations have n>1\n")
    res <- dplyr::filter(df_c, .data$n_occur>1) %>%
      select(.data$n_occur, everything()) %>%
      mat_remo_cols_1val(.data$n_occur)

    ## first group
    res_groups <- dplyr::filter(df_c, .data$n_occur>1) %>%
      mutate(group = dplyr::group_indices(dplyr::group_by(., !!!by_quos))) %>%
      select(.data$group, everything()) %>%
      filter(.data$group==1) %>%
      mat_remo_cols_1val()

    if(.print) {
      print(res_groups)
      print(utils::head(res, 5))
    }
  } else {
    cat("Is unique!\n")
    res <- df %>%
      dplyr::count(!!!enquos(...), name = "n_occur")
  }
  invisible(is_unique)
}


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
#' @param fun function, Defaults to sum
#' @param total_name The name, default is "Total"
#' @export
#' @examples
#' data(iris)
#' iris$Species <- as.character(iris$Species)
#' mat_add_total_row(iris[1:5,])
mat_add_total_row <- function(df, fun=sum, total_name="Total") {
  df %>%
    bind_rows(summarise_all(df, list(~if(is.numeric(.)) fun(., na.rm=TRUE) else if(is.logical(.)) NA else total_name)))
}


#' Add rowSums over some variables
#'
#' If not specified, over all numeric columns
#' @param df data
#' @param \ldots variables
#' @examples
#' library(dplyr)
#' df <- data.frame(Char = c("A", "B"),
#'                  var_1 = c(1,2),
#'                  var_2=c(1,2),
#'                  other = c(9,10))
#' mat_add_total_col(df)
#' mat_add_total_col(df, var_1)
#' mat_add_total_col(df, -var_1, -Char)
#' mat_add_total_col(df, starts_with("var"))
#' mat_add_total_col(df, starts_with("not found"))
#' @export
mat_add_total_col <- function(df, ...) {

  if(length(rlang::quos(...))>0){
    vars <- tidyselect::vars_select(dplyr::tbl_vars(df), !!!rlang::enquos(...))
    df_sum <- df %>%
      select(vars)
  } else {
    df_sum <- df %>%
      dplyr::select_if(is.numeric)
  }
  if(ncol(df_sum)==0) {
    warning("No variable selected!?")
  }
  df %>%
    mutate(Total = rowSums(df_sum))
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
    select(which(map_int(df, dplyr::n_distinct) %in%  seq_len(nval_max)))%>%
    dplyr::distinct()

  if(ncol(res)==0) return(res)

  res2 <-  res %>%
    gather("var", "val", dplyr::everything()) %>%
    mutate("has_na" = is.na(.data$val)) %>%
    dplyr::arrange(.data$has_na, .data$var) %>%
    select(-.data$has_na) %>%
    dplyr::distinct()

  if(wide) {
    res2 <-  res2 %>%
      group_by(.data$var) %>%
      mutate(n_rep = paste("val", 1:n(), sep = "_"),
             n_tot = n(),
             n_na = sum(is.na(.data$val))) %>%
      ungroup() %>%
      mutate(val = dplyr::if_else(is.na(.data$val), "NA_value", .data$val)) %>%
      spread(.data$n_rep, .data$val, fill="") %>%
      arrange(dplyr::desc(.data$n_tot), .data$n_na, .data$var) %>%
      select(-.data$n_tot, -.data$n_na) %>%
      dplyr::mutate_all(~dplyr::if_else(.== "NA_value", NA_character_, .))

  }
  if(!show_na) {
    res2 <-  res2 %>%
      dplyr::filter_at(vars(dplyr::starts_with("val")), dplyr::any_vars(!is.na(.)))
  }
  res2
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

#' Change column names
#'
#' @param df data
#' @param pattern,replacement See str_replace
#' @export
#' @examples
#' library(dplyr)
#' iris %>%
#'   mat_cols_change("Sepal", "pal") %>%
#'   head()
mat_cols_change <- function(df, pattern, replacement) {
  colnames(df) <-  str_replace(colnames(df), pattern, replacement)
  df
}


#' Add row number
#'
#'@param df data
#'@param \ldots group by variables
#'@param col_name Name of the new col, defaults to n_row
#'@export
#'@examples
#'df <- data.frame(group = rep(letters[1:2], each=3), values= 1:6)
#'mat_add_row_num(df)
#'mat_add_row_num(df, group)

mat_add_row_num <- function(df, ..., col_name = "n_row") {
  col_namei = rlang::enquo(col_name)
  group_vars <- rlang::enquos(...)
  if(length(group_vars)!=0) {
    if(dplyr::is_grouped_df(df)) stop("Data already grouped, stop.")
    df  <-  df %>%
      dplyr::group_by(!!!group_vars)
  }

  df %>%
    dplyr::mutate(!!col_namei := 1:n()) %>%
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

mat_vars_uniques <- function(df, ..., wide=TRUE) {
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

#' Compare if datasets are equal
#'
#'Merge, and for all identical variables, compare
#' @param df1,df2 The two df to compare
#' @param by the key variables
#' @param join_fun the type of join
#' @param tol tolerance value
#' @examples
#' library(dplyr)
#' data(iris_tb)
#' iris_orig <- mutate(iris_tb, row_num = 1:n())
#' iris_new <- mutate(iris_orig, Sepal.Length =Sepal.Length+0.000001)
#' mat_join_compare(df1=iris_orig, df2=iris_new, by = c("row_num", "Species"))
#' mat_join_compare(df1=iris_orig, df2=iris_new, by =c("row_num", "Species"), tol = 0.00001)
#' ## compare identical but smaller subset with inner_join:
#' iris_smaller <- subset(iris_orig, Species!="setosa")
#' mat_join_compare(df1=iris_orig, df2=iris_smaller, by = c("row_num", "Species"),
#'                  join_fun = dplyr::full_join)
#' @export
mat_join_compare <- function(df1, df2, by=NULL, join_fun = dplyr::inner_join, tol = 0.00000001) {
  commonCols <- base::intersect(colnames(df1), colnames(df2))
  new_cols <- setdiff(commonCols, by)
  if(length(new_cols)==0) warning("New remaining common columns?")

  df1 %>%
    join_fun(df2, by = by) %>%
    tidyr::pivot_longer(tidyselect::matches("\\.x$|\\.y$"),
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(x$|y$)") %>%
    filter(abs(.data$x-.data$y) >tol|!sum(c(is.na(.data$x), is.na(.data$y)))%in% c(0,2))
}

#' Compare if col_1 and col_2 equal
#'
#' Add a col diff
#' @param df data
#' @param col_1,col_2 The columns
#' @param tol tolerance level
#' @param filter should keep only rows with diff?
#' @examples
#' data(iris_tb)
#' iris_tb$Sepal.Length2 <- iris_tb$Sepal.Length+rep(c(0.000001, 0), 75)
#' mat_col_check_same(iris_tb, Sepal.Length2, Sepal.Length)
#' mat_col_check_same(iris_tb, Sepal.Length2, Sepal.Length, filter=TRUE)
#' @export
mat_col_check_same <- function(df, col_1, col_2, tol = 0.00000001, filter=FALSE) {
  df <- df %>%
    mutate(diff = abs({{col_1}}- {{col_2}}) ,
           is_same = diff < tol)
  if(filter) df <- filter(df, !is_same |is.na(is_same))
  df
}


#' enframe wide
#'
#' Add a col diff
#' @param x a vector
#' @param names vector of names
#' @examples
#' mat_enframe_wide(x=c(a=1,b=22))
#' mat_enframe_wide(x=c(1,22))
#' mat_enframe_wide(x=c(a=1,22, c=9, 8))
#' @export
mat_enframe_wide <- function(x, names=NULL){
  x_names <- if(!is.null(names)) names else  names(x)
  if(is.null(x_names)) x_names <- paste("col", 1:length(x), sep="_")
  if(any(x_names=="")) x_names[x_names==""] <- paste("col", which(x_names==""), sep="_")
  names(x) <- x_names
  x %>%
    tibble::enframe() %>%
    tidyr::pivot_wider(names_from = "name",
                       values_from="value", names_sort=FALSE)
}

#' Take first groups
#'
#' @param df data
#' @param ... grouping vars
#' @param n_head Number of groups
#' @param slice Whether \code{n_head} indicates row position
#' @examples
#' df <- data.frame(group_a = rep(LETTERS[1:3], each=4),
#'                  group_b = rep(letters[1:2], 6),dat = 1:12)
#' mat_head_group(df, group_a, n_head=1)
#' mat_head_group(df, group_a, group_b, n_head=1)
#'
#' ## Use as slice: select second group only, not up to 2:
#' mat_head_group(df, group_a, n_head=2, slice=TRUE)
#' mat_head_group(df, group_a, n_head=c(2,3), slice=TRUE)
#' @export
mat_head_group <- function(df, ..., n_head=5, slice = FALSE){
  .group_vars <- rlang::enquos(...)
  nm = unname(sapply(rlang::enexprs(...), as.character))

  if(slice==FALSE) {
    if(length(n_head)>1) stop("'n_head' has length >1. Set 'slice=TRUE'?")
    n_head <- 1:n_head
  }
  df %>%
    dplyr::semi_join(df %>%
                       dplyr::distinct(!!!.group_vars) %>%
                       dplyr::slice(n_head),
                     by = nm)
}
