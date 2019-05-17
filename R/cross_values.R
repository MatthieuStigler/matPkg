# library(matPkg)
# library(tidyverse)
#
# data_test <- tidyr::crossing(var1=letters[1:3], var2=c("blue", "green"))
# mat_vars_uniques(df=data_test)
# mat_vars_uniques(data_test, wide=FALSE)
# mat_vars_uniques(df=data_test, wide=FALSE)


# get_vals <-
get_any <- function(df, x) {
  unique(df %>% pull(!!rlang::enquo(x))) %>%
    paste(collapse = "|")
}

cross_values <- function(df, args){
  vars <- names(args)
  val_table <- mat_vars_uniques(df, wide=FALSE, !!vars)
  val_table
  vars_df <- tibble(variable=vars, values = args) %>%
    unnest(.data$values) %>%
    mutate(values_exp = NA)

  for(i in 1:nrow(vars_df)) {
    val_i <- vars_df[i,"values", drop=TRUE]
    if( val_i ==".all") {
      vars_df[i,"values_exp"] <- get_any(df, vars_df[i,"variable", drop=TRUE])
    } else if(!is.na(val_i)) {
      val_avail <- val_table[val_table$variable==vars_df[i,"variable", drop=TRUE], "values", drop=TRUE]
      is_there <- val_i %in% val_avail
      if(!is_there) warning("Prolem")
      vars_df[i,"values_exp"] <- val_i
    }
  }
}

 # a <-  vars_df %>%
 #    select(variable, values_exp) %>%
 #    nest(data=-variable) %>%
 #    mutate(data = map(data, c)) %>%
 #   unlist()
 #
 # a

#  map2(a$variable, a$data, ~unlist(.y)) %>%
#    set_names(a$variable) %>%
#    crossing()
#  as.list()
#  a[1,2]
#
#  a %>%
#     split(.variable)
#
#   crossing(a=c(1,2), b=c(3,4))
#
#   vars_df %>%
#     mutate(values_dat = map2(variable, values,
#                              ~filter(vars_df, !!quo(.x)==.y)))
#
#
#   as_tibble(args) %>%
#     gather(variable, values) %>%
#     nest(data=-variable)
#   vars_df %>%
#     mutate(vals = map(variable, ~filter(val_table, variable==.)))
#
#   vars_df %>%
#     mutate(vals = map(variable, ~filter(val_table, variable==.)))
#   map(args, ~tibble(variable= names(.), a=1 ))
# }
#
# cross_values(data_test,
#              args = list(var1=c("a", ".all"),
#                          var2=".all"))
#
# get_any(df, var1)
