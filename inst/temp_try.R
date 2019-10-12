library(tidyverse)

mat_remo_cols_1val <-  function(df, ...) {
  keep_var <- rlang::enquos(...)
  rem_vars <- which(map_int(df, dplyr::n_distinct)==1)
  vars <- tidyselect::vars_select(tbl_vars(df), !!!enquos(...))
  print(vars)
  df %>%
    select(-rem_vars, !!!keep_var)
}


data(quick_stats)
mat_remo_cols_1val(quick_stats)
mat_remo_cols_1val(quick_stats, geo_level)

dplyr:::select.data.frame
