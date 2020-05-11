################################
### Internal utilities
################################

intrnl_session_clean <- function(.data, var) dplyr::if_else(duplicated(.data$time), "", as.character({{var}}))
intrnl_dir_to_file <- function(dir) paste(dir, "999_CHECK_RUN_report.csv", sep = "/") %>%
  str_replace("//", "/")

intrnl_cols_need <- c("session", "session_time",
                     "user_node",
                     "filename", "folder", "subfolder",
                     "error", "has_error",
                     "elapsed", "memory_used_mb",
                     "user.self", "sys.self", "user.child",
                     "sys.child", "ext", "number_char", "number", "error_parse",
                     "has_error_parse", "has_yaml", "has_runMat", "runMat_val",
                     "date", "time")


intrnl_time_format <- function(x) {
  if(is.na(x)) return(NA_character_)
  if(x<1) return("<1 Sec")
  x <-  x/60
  if(x<1) return("<1 Min")
  if(x >60) {
    H <- x %/% 60
    Min <- x %% 60
    H_Min <- paste0(H, ":", stringr::str_pad(round(Min),width = 2, pad = "0"))
    return(paste0(H_Min, " Hour"))
  }
  return(paste(round(x), "Min"))
}

intrnl_time_format_vec <- function(x) sapply(x, intrnl_time_format)

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

################################
### Step 1: list R scripts
################################


#' @param dir_path directory of files
#' @param no_old Avoid scripts in directory old?
#' @param recursive Look into recursive folders?
#' @export
#' @rdname mat_99_run_Rfiles
mat_99_list_Rfiles <- function(dir_path="code_setup", no_old = TRUE, recursive=FALSE) {
  dir_df <- mat_list_dir(dir_path, pattern="\\.R$", add_ext = TRUE, recursive = recursive) %>%
    mutate(full_path = str_replace_all(.data$full_path, "//", "/"),
           folder = dirname(.data$full_path),
           subfolder = basename(dirname(.data$full_path)))

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

#' @rdname mat_99_run_Rfiles
#' @export
mat_list_Rfiles <- function(dir_path, no_old = TRUE, recursive=FALSE) {
  warning("Deprecated: use rather mat_99_list_Rfiles")
  mat_99_list_Rfiles(dir_path, no_old = no_old, recursive=recursive)
}

#' Run-check files 999 machinery
#'
#' @param echo print which file done, and gc?
#' @param runMat_true_only run only the ones with runMat: TRUE
#' @examples
#' library(matPkg)
#' library(readr)
#'
#' ## read
#' path_rscripts <- system.file("r_scripts_fake", package = "matPkg")
#' path_temp <- tempdir()
#' dir_dat <- mat_99_list_Rfiles(path_rscripts)
#' dir_dat
#'
#' ## run
#' out <- mat_99_run_Rfiles(dir_dat)
#' out
#' mat_99_showErr(out)
#'
#' ## save
#' mat_99_write(out, dir = path_temp)
#'
#' ## check output
#' read_csv(paste(path_temp, "999_CHECK_RUN_report.csv", sep="/"), col_types = cols())
#'
#' ## re-write
#' mat_99_write(out, dir = path_temp)
#' read_csv(paste(path_temp, "999_CHECK_RUN_report.csv", sep="/"), col_types = cols())
#'
#' ## check format
#' mat_99_check_there_update(path_temp, overwrite=FALSE)
#' mat_99_check_there_update(path_temp, overwrite=TRUE)
#' @export
mat_99_run_Rfiles <- function(scripts_file, echo=FALSE, runMat_true_only=TRUE) {
  if(runMat_true_only) scripts_file <- scripts_file %>%
      filter(.data$runMat_val)

  scripts_file %>%
    mutate(try = map(.data$full_path, ~purrr::safely(~source_throw(., echo=echo))(.))) %>%
    mutate(error=map(.data$try, ~.[["error"]]),
           has_error= map_lgl(.data$error, ~!is.null(.)),
           error=map_chr(.data$error, ~ifelse(is.null(.), NA, .) %>% as.character),
           timing = map2(.data$try, .data$has_error, ~if(.y) df_null else .x$result)) %>%
    unnest(.data$timing) %>%
    select("filename", "has_error", "error",
           "elapsed", "memory_used_mb",
           "user.self", "sys.self", "user.child", "sys.child",
           tidyselect::everything())

}

#' @rdname mat_99_run_Rfiles
#' @export
mat_run_Rfiles <- function(scripts_file, echo=FALSE) {
  warning("Deprectaed, use rather 'mat_99_run_Rfiles'")
  mat_99_run_Rfiles(scripts_file=scripts_file, echo=echo)
}

## utilities (from: )
df_null <-  data.frame("user.self" = NA, "sys.self" = NA, "elapsed" = NA, "user.child" = NA, "sys.child" = NA)

as.data.frame.proc_time <-  function(x, ...) t(data.matrix(x)) %>%  as.data.frame(...)

source_throw <- function(path, echo=TRUE) {
  if(echo) cat(paste("\nDoing file: ", basename(path), "\n"))
  gc()
  mem_before <- pryr::mem_used()
  pkgs_before <- .packages()
  env_random <-  new.env()
  sys <- system.time(sys.source(path, envir = env_random))
  mem_after <- pryr::mem_used()
  pkgs_after <- .packages()
  ls_env <- ls(envir = env_random)

  ## clean
  rm(list = ls_env, envir = env_random)
  rm(env_random)
  pkgs_to_remove <- pkgs_after[!pkgs_after%in% c(pkgs_before, "matPkg")]
  # unloadNamespace(pkgs_to_remove)
  detach(paste0("package:",pkgs_to_remove),  unload=FALSE, character.only =TRUE) # otherwise try: devtools::unload("sf")
  gc()

  # memory count
  mem_final <- pryr::mem_used()
  mems_info <- c(mem_before=mem_before, mem_after=mem_after,
                 mem_diff=mem_after-mem_before,1,
                 mem_final=mem_final)
  if(echo) {
    mems_info_char <- as.character.bytes(mems_info, unit="MB")
    mems_info_char2 <- paste(stringr::str_remove(names(mems_info), "mem_"), mems_info_char, sep=": ")
    cat("Memory: ", paste(mems_info_char2, collapse = ", "), "\n")
    cat(paste("Done with file", path, "\n"))
  }
  t(data.matrix(sys)) %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(memory_used_mb = as.numeric.bytes(mems_info["mem_diff"], unit = "MB")) %>%
    mutate_at(c("memory_used_mb", "elapsed"), round, 1)
}

#' As character for bytes
#'
#' Simply copied from pryr:::print.bytes
#' @noRd
as.character.bytes <- function (x, digits = 3, unit=NULL, ...) {

  if(is.null(unit)) {
    power <- min(floor(log(abs(x), 1000)), 4)
    unit <- c("B", "kB", "MB", "GB", "TB")[power+1]
  } else {
    unit <- match.arg(unit, choices=c("B", "kB", "MB", "GB", "TB"))
  }
  x <-  as.numeric.bytes(x, unit=unit)
  formatted <- format(signif(x, digits = digits), big.mark = ",",
                      scientific = FALSE)
  paste(formatted,  unit)
}

#' As numeric for bytes, add argument unit
#'
#' @noRd
as.numeric.bytes <- function (x, unit = c("B", "kB", "MB", "GB", "TB")) {
  unit <-  match.arg(unit)
  power <-  match(unit, c("B", "kB", "MB", "GB", "TB")) -1
  as.numeric(x/(1000^power))
}

## Cechk bytes
if(FALSE) {
  val_bytes <- 122661728
  class(val_bytes) <- "bytes"
  val_bytes
  as.character(val_bytes)
  as.numeric(val_bytes)

  sapply(c("B", "kB",  "MB", "GB"), function(unit) as.numeric.bytes(val_bytes, unit))
  sapply(c("B", "kB",  "MB", "GB"), function(unit) as.character.bytes(val_bytes, unit=unit))

}


#' @export
#' @rdname mat_99_run_Rfiles
mat_99_showErr <- function(scripts_file_runned) {
  df_probs <- scripts_file_runned %>%
    dplyr::filter(.data$has_error)

  print(df_probs %>%
    dplyr::select(.data$filename, .data$error) )

  print(df_probs %>%
          dplyr::pull(.data$error))
}

#' @param overwrite should overwrite data?
#' @export
#' @rdname mat_99_run_Rfiles
mat_99_check_there_update <- function (dir_path="code_setup", overwrite=TRUE) {
  file_out <- intrnl_dir_to_file(dir_path)

  is_there <- file.exists(file_out)
  if (is_there) {

    file_old <- readr::read_csv(file_out, col_types = readr::cols())

    ## add columns if needed
    if (!all(intrnl_cols_need %in% colnames(file_old))) {
      cols_miss <- intrnl_cols_need[!intrnl_cols_need %in% colnames(file_old)]
      if("session" %in% cols_miss && !"date" %in% cols_miss) {
        cols_miss <-  cols_miss[-which(cols_miss=="session")]
        file_old <-  file_old %>%
          mutate(session=date,
                 session = mat_keep_first(as.character(.data$session)))
      }
      warning("Problems in data! Missing: ", paste(cols_miss,
                                                   collapse = " "))
      file_old[cols_miss] <- NA
      file_old <- file_old %>%
        select(tidyselect::all_of(intrnl_cols_need))
      print(file_old)
      if(overwrite)  {
        readr::write_csv(file_old, file_out)
        file_old <- readr::read_csv(file_out, col_types = readr::cols())
      }
    }

    ## Check too many columns?
    if (!all(colnames(file_old) %in% intrnl_cols_need )) {
      cols_old_superflous <- colnames(file_old)[!colnames(file_old) %in% intrnl_cols_need]
      cols_old_necessary <- colnames(file_old)[colnames(file_old) %in% intrnl_cols_need]
      warning("Supplementary columns: ", paste(cols_old_superflous, collapse = ", "))
      file_old <- file_old %>%
        select(tidyselect::all_of(cols_old_necessary))
    }

    ## reorder columns if needed
    if(!all(intrnl_cols_need == colnames(file_old))){
      print("Change order of columns")
      file_old <- file_old %>%
        select(tidyselect::all_of(intrnl_cols_need))
      if(overwrite)  {
        readr::write_csv(file_old, file_out)
        file_old <- readr::read_csv(file_out, col_types = readr::cols())
      }
    }

    ## compute session_time if relevant columns there
    if(all(c("time", "elapsed") %in% colnames(file_old))) {
      file_old <- file_old %>%
        group_by(.data$time) %>%
        mutate(session_time = sum(.data$elapsed, na.rm=TRUE)) %>%
        ungroup() %>%
        mutate(session_time = intrnl_time_format_vec(.data$session_time))

      if(overwrite)  {
        readr::write_csv(file_old, file_out)
        # file_old <- readr::read_csv(file_out, col_types = readr::cols())
      }
    } else {
      warning("Missing columns 'time' and 'elapsed'")
    }

    ## last run make sure clean session column
    if(overwrite)  {
      readr::read_csv(file_out, col_types = readr::cols()) %>%
        mutate(session_id = paste(.data$session, .data$session_time, sep="_"),
               session = dplyr::if_else(duplicated(.data$session_id) | is.na(.data$session), "", as.character(.data$session))) %>%
        select(-.data$session_id) %>%
        readr::write_csv(file_out)
    }
  }
  is_there
}

#' @export
#' @rdname mat_99_run_Rfiles
mat_99_check_there <- function (dir_path="code_setup", overwrite=TRUE) {
  warning("Deprecated, use rather 'mat_99_check_there_update'")
  mat_99_check_there_update(dir_path=dir_path, overwrite=overwrite)
}

#' @param scripts_file_runned data from mat_99_run_Rfiles
#' @param append Overwrite existing or append?
#' @export
#' @rdname mat_99_run_Rfiles
mat_99_write <- function(scripts_file_runned, dir_path="code_setup",
                         append=TRUE) {
  file_out <- intrnl_dir_to_file(dir_path)
  today <- Sys.Date() %>% as.character()
  time <- Sys.time() %>%
    format(format="%H:%M:%S") %>%
    as.character()
  user <- paste(Sys.info()[c("user", "nodename")], collapse = "_")

  ## add session info, etc
  df_new <- scripts_file_runned %>%
    dplyr::select_if(~!is.list(.)) %>%
    mutate(session = today,
           session = dplyr::if_else(duplicated(.data$session), "", .data$session),
           session_time = sum(.data$elapsed, na.rm=TRUE) %>%
             intrnl_time_format_vec(),
           date = today,
           time = Sys.time() %>% as.character(),
           folder = dirname(.data$full_path),
           subfolder = basename(dirname(.data$full_path)),
           user_node =user) %>%
    select(tidyselect::all_of(intrnl_cols_need))

  readr::write_csv(df_new, file_out, append=append)

}

#' @param warn Should it warn if file is actually missing?
#' @export
#' @rdname mat_99_run_Rfiles
mat_99_add_info_last <- function(scripts_file_runned, dir_path="code_setup", warn=TRUE) {
  file_out <- intrnl_dir_to_file(dir_path)

  if(file.exists(file_out)) {
    df_out <- readr::read_csv(file_out) %>%
      filter(.data$time==max(.data$time))

    scripts_file_runned <- scripts_file_runned %>%
      left_join(df_out %>%
                  select(.data$filename, .data$elapsed, .data$has_error),
                by = "filename") %>%
      rename(elapsed_last=.data$elapsed,
             has_error_last=.data$has_error)
  } else {
    if(warn) warning("File not already there!")
  }
  scripts_file_runned
}

#' @param scripts_file file of R scripts with path to run, from mat_99_list_Rfiles()
#' @export
#' @rdname mat_99_run_Rfiles
mat_99_arrange_by_last <- function(scripts_file_runned, dir_path="code_setup") {

  if(file.exists(dir_path)) {
    df_out <- readr::read_csv(dir_path) %>%
      filter(.data$time==max(.data$time))

    scripts_file_runned <- scripts_file_runned %>%
      left_join(df_out %>%
                  select(.data$filename, .data$elapsed),
                by = "filename") %>%
      mutate(first_num = str_extract(.data$filename, "^[0-9]") %>% as.integer) %>%
      arrange(.data$first_num, .data$elapsed) %>%
      # rename(elapsed_before=elapsed)
      select(-.data$elapsed, -.data$first_num)
  }
  scripts_file_runned
}


if(FALSE) {
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

  ## rerun
  out2 <- mat_99_run_Rfiles(dir_dat)
  mat_99_check_there_update(path_temp, overwrite=FALSE)
  mat_99_write(out2, dir = path_temp)
  read_csv(paste(path_temp, "999_CHECK_RUN_report.csv", sep="/"), col_types = cols())

  ## check format
  mat_99_check_there_update(path_temp, overwrite=FALSE)
  mat_99_check_there_update(path_temp, overwrite=TRUE)

  rm(heavy_vec)

}
