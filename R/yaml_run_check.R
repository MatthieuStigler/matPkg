#' @importFrom utils getFromNamespace
read_utf8 = getFromNamespace("read_utf8", "rmarkdown")
parse_yaml_front_matter = getFromNamespace("parse_yaml_front_matter", "rmarkdown")

#' Read YAML header to list
#' @param file_path The file path
#' @export
mat_parse_yaml <- function(file_path){
  input_lines <- read_utf8(file_path)
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
#' @rdname mat_run_Rfiles
mat_99_showErr <- function(df) {
  df_probs <- df %>%
    dplyr::filter(.data$has_error)

  print(df_probs %>%
    dplyr::select(.data$filename, .data$error) )

  print(df_probs %>%
          dplyr::pull(.data$error))
}

#' Write output of 999 file
#' @param dir main directory
#' @param overwrite should overwrite data?
#' @export
#' @rdname mat_run_Rfiles
mat_99_check_there <- function (dir, overwrite=TRUE) {
  file_out <- paste(dir, "999_CHECK_RUN_report.csv", sep = "/") %>%
    str_replace("//", "/")
  is_there <- file.exists(file_out)
  if (is_there) {
    cols_need <- c("session", "session_time",
                   "filename", "has_error", "error",
                   "user.self", "sys.self", "elapsed", "user.child",
                   "sys.child", "ext", "number_char", "number", "error_parse",
                   "has_error_parse", "has_yaml", "has_runMat", "runMat_val",
                   "full_path",
                   "date", "time")
    file_old <- readr::read_csv(file_out, col_types = readr::cols())

    ## add columns if needed
    if (!all(cols_need %in% colnames(file_old))) {
      cols_miss <- cols_need[!cols_need %in% colnames(file_old)]
      if("session" %in% cols_miss && !"date" %in% cols_miss) {
        cols_miss <-  cols_miss[-which(cols_miss=="session")]
        file_old <-  file_old %>%
          mutate(session=date,
                 session = mat_keep_first(as.character(.data$session)))
      }
      warning("Problems in data! Missing: ", paste(cols_miss,
                                                   collapse = " "))
      file_old[cols_miss] <- NA
      file_old <- file_old %>% select(cols_need)
      print(file_old)
      if(overwrite) readr::write_csv(file_old, file_out)
    }
    ## reorder columns if needed
    if(!all(cols_need == colnames(file_old))){
      print("Change order")
      file_old <- file_old %>%
        select(cols_need)
      if(overwrite)  readr::write_csv(file_old, file_out)
    }
    ## add session_time if not there
    if(any(!is.na(file_old$session) & is.na(file_old$session_time))) {
      print("Some session_time missing?")

      file_old <- file_old %>%
        group_by(.data$time) %>%
        mutate(session_time = ifelse(!is.na(.data$session_time),
                                     .data$session_time,
                                     sum(.data$elapsed, na.rm=TRUE)),
               session_time = mat_keep_first(.data$session_time)) %>%
        ungroup()
      if(overwrite)  readr::write_csv(file_old, file_out)
    }
  }
  is_there
}

#' Write output of 999 file
#' @param df data from 999
#' @param dir Directory
#' @export
#' @rdname mat_run_Rfiles
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
           session = dplyr::if_else(duplicated(.data$session), "", .data$session),
           session_time = sum(.data$elapsed, na.rm=TRUE),
           date = today,
           time = Sys.time() %>% as.character()) %>%
    select(.data$session, .data$session_time, everything()) %>%
    readr::write_csv(file_out, append=is_there)

}


#' If only appendix, arrange file by previous timings
#' @param path_out path of where 999_CHECK_RUN_report os
#' @param dat_files current file with path to run
#' @export
#' @rdname mat_run_Rfiles
mat_99_arrange_by_last <- function(path_out, dat_files) {

  if(file.exists(path_out)) {
    df_out <- readr::read_csv(path_out) %>%
      filter(.data$time==max(.data$time))

    dat_files <- dat_files %>%
      left_join(df_out %>%
                  select(.data$filename, .data$elapsed),
                by = "filename") %>%
      mutate(first_num = str_extract(.data$filename, "^[0-9]") %>% as.integer) %>%
      arrange(.data$first_num, .data$elapsed) %>%
      # rename(elapsed_before=elapsed)
      select(-.data$elapsed, -.data$first_num)
  }
  dat_files
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
  mat_99_check_there("inst/", overwrite=FALSE)
  mat_99_check_there("inst/", overwrite=TRUE)
  mat_99_write(out, dir = "inst")
  read_csv("inst/999_CHECK_RUN_report.csv")

  out %>%
    filter(has_error) %>%
    select(filename, error)


  rm(heavy_vec)
  file.remove("inst/999_CHECK_RUN_report.csv")

}
