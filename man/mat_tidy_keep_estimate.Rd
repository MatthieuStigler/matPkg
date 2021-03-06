% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/broom_tidy.R
\name{mat_tidy_keep_estimate}
\alias{mat_tidy_keep_estimate}
\title{Remove reg details}
\usage{
mat_tidy_keep_estimate(df)
}
\arguments{
\item{df}{data}
}
\description{
Remove reg details
}
\examples{
library(purrr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

iris_regs <- nest(iris, data=-Species) \%>\%
mutate(reg_out = map(data, ~lm(Petal.Width~Petal.Length, data=as_tibble(.)))) \%>\%
select(-data)

coefs_out <- mat_tidy_do(df=iris_regs)
mat_tidy_keep_estimate(coefs_out)
mat_tidy_keep_estimate(df= freeny)
}
