% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/memory_show_check.R
\name{mat_show_mem}
\alias{mat_show_mem}
\title{Show memory usage, by object, or from workspace}
\usage{
mat_show_mem(df_input, is_ls = FALSE, all.names = TRUE)
}
\arguments{
\item{df_input}{input data. Can be a df, a list of objects, or a vector of characters. If empty, will evaluate ls()}

\item{is_ls}{Is 'df_input' actually a list of characters (if yes, runs get() first)}

\item{all.names}{Whether should use all.names when calling ls()}
}
\description{
Show memory usage, by object, or from workspace
}
\examples{
mat_show_mem()
mat_show_mem(freeny)
library(purrr)
library(dplyr)
library(tidyr)

iris_regs <- nest(iris, data=-Species) \%>\%
  mutate(reg_out = map(data, ~lm(Petal.Width~Petal.Length, data=as_tibble(.)))) \%>\%
  select(-data)
# DOES NOT WORK ANYMORE!?
# mat_show_mem(iris_regs$reg_out)

## Check a workspace
#mat_show_mem(ls(), is_ls=TRUE)
}
