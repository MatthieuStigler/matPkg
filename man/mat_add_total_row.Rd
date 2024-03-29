% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{mat_add_total_row}
\alias{mat_add_total_row}
\title{Add row of total}
\usage{
mat_add_total_row(df, fun = sum, total_name = "Total")
}
\arguments{
\item{df}{the data-frame}

\item{fun}{function, Defaults to sum}

\item{total_name}{The name, default is "Total"}
}
\description{
Add either sum, or Total (if not numeric)
}
\examples{
data(iris)
iris$Species <- as.character(iris$Species)
mat_add_total_row(iris[1:5,])
}
