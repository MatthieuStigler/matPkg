% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/99_run_check.R
\name{mat_99_list_Rfiles}
\alias{mat_99_list_Rfiles}
\alias{mat_list_Rfiles}
\alias{mat_99_run_Rfiles}
\alias{mat_run_Rfiles}
\alias{mat_99_showErr}
\alias{mat_99_check_there_update}
\alias{mat_99_check_there}
\alias{mat_99_write}
\alias{mat_99_add_info_last}
\alias{mat_99_arrange_by_last}
\title{Run-check files 999 machinery}
\usage{
mat_99_list_Rfiles(
  dir_path = "code_setup",
  no_old = TRUE,
  recursive = FALSE,
  keep_field = NULL
)

mat_list_Rfiles(dir_path, no_old = TRUE, recursive = FALSE)

mat_99_run_Rfiles(
  scripts_file,
  echo = FALSE,
  runMat_true_only = TRUE,
  run_function = c("internal", "external"),
  tmp_dir = NULL,
  run_cmd_vanilla = FALSE
)

mat_run_Rfiles(scripts_file, echo = FALSE)

mat_99_showErr(scripts_file_runned)

mat_99_check_there_update(dir_path = "code_setup", overwrite = TRUE)

mat_99_check_there(dir_path = "code_setup", overwrite = TRUE)

mat_99_write(
  scripts_file_runned,
  dir_path = "code_setup",
  append = NULL,
  cols_extra = NULL
)

mat_99_add_info_last(scripts_file_runned, dir_path = "code_setup", warn = TRUE)

mat_99_arrange_by_last(scripts_file_runned, dir_path = "code_setup")
}
\arguments{
\item{dir_path}{directory of files}

\item{no_old}{Avoid scripts in directory old?}

\item{recursive}{Look into recursive folders?}

\item{keep_field}{Additional YAML fields to keep}

\item{scripts_file}{file of R scripts with path to run, from mat_99_list_Rfiles()}

\item{echo}{print which file done, and gc?}

\item{runMat_true_only}{run only the ones with runMat: TRUE}

\item{run_function}{use either source (internal) or R CMD BATCH (external)}

\item{tmp_dir}{temporary dir to hold file when using \code{run_function="external"}}

\item{run_cmd_vanilla}{when \code{run_function="external"}, should \code{--vanilla} be added?}

\item{scripts_file_runned}{data from mat_99_run_Rfiles}

\item{overwrite}{should overwrite data?}

\item{append}{Overwrite existing or append?}

\item{cols_extra}{Extra columns to keep}

\item{warn}{Should it warn if file is actually missing?}
}
\description{
Run-check files 999 machinery
}
\examples{
library(matPkg)
library(readr)

## read
path_rscripts <- system.file("r_scripts_fake", package = "matPkg")
path_temp <- tempdir()
dir_dat <- mat_99_list_Rfiles(path_rscripts)
dir_dat

## run
out <- mat_99_run_Rfiles(dir_dat)
out
mat_99_showErr(out)

## save
mat_99_write(out, dir = path_temp)

## check output
read_csv(paste(path_temp, "999_CHECK_RUN_report.csv", sep="/"), col_types = cols())

## re-write
mat_99_write(out, dir = path_temp)
read_csv(paste(path_temp, "999_CHECK_RUN_report.csv", sep="/"), col_types = cols())

## check format
mat_99_check_there_update(path_temp, overwrite=FALSE)
mat_99_check_there_update(path_temp, overwrite=TRUE)
}
