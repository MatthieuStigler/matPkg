#' @importFrom utils getFromNamespace
read_utf8 = getFromNamespace("read_utf8", "rmarkdown")
parse_yaml_front_matter = getFromNamespace("parse_yaml_front_matter", "rmarkdown")

#' Read YAML header to list
#' @param file_path The file path
#' @param encoding just in case
#' @export
mat_parse_yaml <- function(file_path, encoding = getOption("encoding")){
  input_lines <- read_utf8(file_path, encoding)
  # input_lines <- rmarkdown:::read_lines_utf8(file_path, encoding)
  if (identical(tolower(tools::file_ext(file_path)), "r"))
    input_lines <- knitr::spin(text = input_lines, knit = FALSE)
  yaml_front_matter <- parse_yaml_front_matter(input_lines)
  yaml_front_matter

}


#' List yaml of R scripts
#'
#' @param dir_path directory of files
#' @param no_old Avoid scripts in directory old?
#' @param recursive Look into recursive folders?
#' @examples
#' mat_list_Rfiles(system.file("r_scripts_fake", package = "matPkg"))
#'
#' @export
mat_list_Rfiles <- function(dir_path, no_old = TRUE, recursive=FALSE) {
  dir_df <- mat_list_dir(dir_path, pattern="\\.R", add_ext = TRUE, recursive = recursive)

  res <- dir_df %>%
    mutate(number_char = str_extract(.data$filename, "([0-9]_)+") %>%
             str_replace("_$", ""),
           number = .data$number_char %>%
             str_replace("_", "\\.") %>%
             str_replace_all("_", "") %>%
             as.numeric(),
           yaml = map(.data$full_path, ~purrr::safely(mat_parse_yaml)(.))) %>%
  mat_safely_unnest(col_name=.data$yaml, result_name="yaml") %>%
    rename(has_error_parse=.data$has_error,
           error_parse=.data$error) %>%
    mutate(has_yaml=map_lgl(.data$yaml, ~ !rlang::is_empty(.)),
           has_runMat  = map2_lgl(.data$has_yaml, .data$yaml, ~if(.x)  "runMat" %in% names(.y) else FALSE ),
           runMat_val = map2_lgl(.data$has_runMat, .data$yaml, ~if(.x)  .y$runMat else FALSE )) %>%
    select(-.data$full_path, .data$full_path)
  if(no_old) res <-  res %>%
    filter(!stringr::str_detect(.data$full_path, "old.*/"))
  res
}

#' Run files from a df-df
#'
#' @param df the df of files, from mat_list_Rfiles()
#' @param echo print shich file done, and gc?
#' @examples
#' files_df <- mat_list_Rfiles(system.file("r_scripts_fake", package = "matPkg"))
#' mat_run_Rfiles(files_df, echo=TRUE)
#' @export
mat_run_Rfiles <- function(df, echo=FALSE) {
  df %>%
    mutate(try = map(.data$full_path, ~purrr::safely(~source_throw(., echo=echo))(.))) %>%
    mutate(error=map(.data$try, ~.[["error"]]),
           has_error= map_lgl(.data$error, ~!is.null(.)),
           error=map_chr(.data$error, ~ifelse(is.null(.), NA, .) %>% as.character),
           timing = map2(.data$try, .data$has_error, ~if(.y) df_null else .x$result)) %>%
    unnest(.data$timing) %>%
    select("filename", "has_error", "error",
           "user.self", "sys.self", "elapsed", "user.child", "sys.child",
           tidyselect::everything())

}

#' Show memory usage, by object
#' @export
#' @examples
#' mat_show_mem()
mat_show_mem <-  function() {
  tibble(obj = ls(.GlobalEnv)) %>%
    mutate(obj_size = map(.data$obj, ~pryr::object_size(get(.))),
           size = map_dbl(.data$obj_size, ~.),
           unit = map_chr(.data$obj_size, ~class(.)),
           class_1 = map_chr(.data$obj, ~ class(get(.))[1])) %>%
    arrange(desc(.data$size))
}


## utilities (from: )
df_null <-  data.frame("user.self" = NA, "sys.self" = NA, "elapsed" = NA, "user.child" = NA, "sys.child" = NA)

as.data.frame.proc_time <-  function(x, ...) t(data.matrix(x)) %>%  as.data.frame(...)

source_throw <- function(path, echo=TRUE) {
  if(echo) cat(paste("\nDoing file: ", basename(path)))
  env_random <-  new.env()
  sys <- system.time(sys.source(path, envir = env_random))
  ls_env <- ls(envir = env_random)
  rm(list = ls_env, envir = env_random)
  rm(env_random)
  a <- gc()
  if(echo) {
    print(as.character.bytes(pryr::mem_used()))
    cat(paste("Done with file", path, "\n"))
  }
  t(data.matrix(sys)) %>%
    as.data.frame() %>%
    as_tibble()
}

#' As character for bytes
#'
#' Simply copied from pryr:::print.bytes
#' @noRd
as.character.bytes <- function (x, digits = 3, ...) {
  power <- min(floor(log(abs(x), 1000)), 4)
  if (power < 1) {
    unit <- "B"
  }
  else {
    unit <- c("kB", "MB", "GB", "TB")[[power]]
    x <- x/(1000^power)
  }
  formatted <- format(signif(x, digits = digits), big.mark = ",",
                      scientific = FALSE)
  paste(formatted,  unit)
}

#' Show problems in 999 file
#' @param df data from 999
#' @export
mat_99_showErr <- function(df) {
  df_probs <- df %>%
    dplyr::filter(.data$has_error)

  print(df_probs %>%
    dplyr::select(.data$filename, .data$error) )

  print(df_probs %>%
          dplyr::pull(.data$error))
}

#' Write output of 999 file
#' @param df data from 999
#' @export
mat_99_check_there <- function(dir) {
  file_out <- paste(dir, "999_CHECK_RUN_report.csv", sep="/") %>%
    str_replace("//", "/")
  is_there <- file.exists(file_out)
  if(is_there) {
    cols_need <- c("session", "filename", "has_error", "error", "user.self", "sys.self",
                   "elapsed", "user.child", "sys.child", "ext", "number_char", "number",
                   "error_parse", "has_error_parse", "has_yaml", "has_runMat", "runMat_val",
                   # "elapsed_before", "first_num",
                   "date", "time")
    file_old <- read_csv(file_out, col_types = cols())
    problems(file_old)
    if(!all(cols_need %in% colnames(file_old))){
      cols_miss <- cols_need[!cols_need %in% colnames(file_old)]
      warning("Problems in data! Missing: ", paste(cols_miss, collapse = " "))
      file_old[cols_miss] <- NA
      file_old <- file_old %>%
        select(cols_need)
      write_csv(file_old, dir)
    }
  }
  is_there
}

#' Write output of 999 file
#' @param df data from 999
#' @param dir Directory
#' @export
mat_99_write <- function(df, dir) {
  file_out <- paste(dir, "999_CHECK_RUN_report.csv", sep="/") %>%
    str_replace("//", "/")
  is_there <- file.exists(file_out)
  today <- Sys.Date() %>% as.character()
  time <- Sys.time() %>%
    format(format="%H:%M:%S") %>%
    as.character()

  df %>%
    dplyr::select_if(~!is.list(.)) %>%
    mutate(session = today,
           session = dplyr::if_else(duplicated(.data$session), "", session),
           date = today,
           time = Sys.time() %>% as.character()) %>%
    select(.data$session, everything()) %>%
    readr::write_csv(file_out, append=is_there)

}


if(FALSE) {
  library(matPkg)
  library(tidyverse)
  mat_show_mem()
  dir <- mat_list_Rfiles("inst/r_scripts_fake")
  dir
  out <- mat_run_Rfiles(dir)
  out
  mat_99_showErr(out)
  mat_99_write(out, dir = "inst")
  out %>%
    filter(has_error) %>%
    select(filename, error)


  rm(heavy_vec)
  file.remove("inst/999_CHECK_RUN_report.csv")

}
