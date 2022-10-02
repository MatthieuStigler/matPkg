#' @param debug internal to show more
#' @noRd
source_throw <- function(path, echo=TRUE, all.names=TRUE, debug=FALSE) {
  if(echo) cat(paste("\nDoing file: ", basename(path), "\n"))
  gc()
  mem_before <- pryr::mem_used()
  pkgs_before <- .packages()
  env_random <-  new.env(parent = .GlobalEnv)
  ## main oline: source file, measuring time and memory!
  # mem_change <- pryr::mem_change(sys <- system.time(suppressMessages(sys.source(path, envir = env_random,
  #                                                                               keep.source=FALSE, keep.parse.data=FALSE))))
  time_before <- Sys.time()
  if(!debug){
    sys <- purrr::safely(~system.time(suppressMessages(sys.source(., envir = env_random,
                                                                  keep.source=FALSE,
                                                                  keep.parse.data=FALSE))))(path)
  } else {
    print(pkgs_before)
    sys <- purrr::safely(~system.time(sys.source(., envir = env_random,
                                                 keep.source=FALSE,
                                                 keep.parse.data=FALSE)))(path)
  }
  time_after <- Sys.time()
  mem_after <- pryr::mem_used()
  pkgs_after <- .packages()
  ls_env <- ls(envir = env_random, all.names = all.names)


  ## errors:
  has_error <- !is.null(sys$error)

  ## clean
  ggplot2::set_last_plot(NULL) ## this will remove the ".last_plot" from ggplot, see https://stackoverflow.com/questions/64654252/r-deleting-ggplot2-object-does-not-free-space-possible-memory-leakage
  rm(list = ls_env, envir = env_random)
  rm(env_random)
  pkgs_to_remove <- pkgs_after[!pkgs_after%in% c(pkgs_before, "matPkg")]
  # unloadNamespace(pkgs_to_remove)
  # detach(paste0("package:",pkgs_to_remove),  unload=FALSE, character.only =TRUE) # otherwise try:
  # purrr::walk(c("tsDyn", "plm"), ~purrr::safely(pkgload::unload)(), quiet=TRUE)
  gc()

  # memory count
  mem_final <- pryr::mem_used()
  mems_info <- c(mem_before=mem_before, mem_after=mem_after,
                 mem_diff=mem_after-mem_before,
                 mem_final=mem_final)
  if(echo) {
    mems_info_char <- as.character.bytes(x=mems_info, unit="MB")
    mems_info_char2 <- paste(stringr::str_remove(names(mems_info), "mem_"), mems_info_char, sep=": ")
    cat("\t-Memory: ", paste(mems_info_char2, collapse = ", "), "\n")
    cat("\t-Approx time: ", intrnl_time_format(time_after-time_before), "\n")
    if(has_error){
      cat("\t-ERROR: ", intrnl_err_to_chr(sys$error), "\n")
      # message(sys$error)
      # cat("\n")
    }
    cat(paste("\t-Done with file", path, "\n"))
  }
  # if(has_error) {
  #   return(sys)
  # } else {
  #   sys <- sys$result
  # }
  if(!has_error) {
    sys$result <- t(data.matrix(sys$result)) %>%
      as.data.frame() %>%
      as_tibble() %>%
      mutate(memory_used_mb = as.numeric.bytes(mems_info["mem_diff"], unit = "MB")) %>%
      mutate_at(c("memory_used_mb", "elapsed"), round, 1)
  }
  sys
}


source_rcmd_batch <- function(path, echo=TRUE){

  if(echo) cat(paste("\nDoing file: ", basename(path), "\n"))

  ## run file external
  tmp_file <- tempfile()
  cmd <- paste("R CMD BATCH ", path, tmp_file)
  time_before <- Sys.time()
  out <- system(cmd)
  time_after <- Sys.time()

  ## read
  out_file <- readLines(tmp_file)
  proc_line <- grepl("proc.time\\(\\)$", out_file)
  has_normal_end <- any(proc_line)
  res <- list(result=NULL, error=NULL)

  if(has_normal_end){
    ok_lines <- out_file[which(proc_line):length(out_file)]
    time_line <- ok_lines[grepl("([0-9]+ )+", ok_lines)]
    times <- stringr::str_extract_all(time_line, "[0-9\\.]+")[[1]] %>% as.numeric()
    res$result <- mat_enframe_wide(times, names = c("user.self", "sys.self", "elapsed")) %>%
      mutate(user.child=NA_real_, sys.child=NA_real_,  memory_used_mb=NA_real_)
  } else {
    error_line_pos <- grepl("^Error(:| in)", out_file)
    if(all(!error_line_pos)) {
      warning("Issue, did not detect string 'Error' in the output script")
      res$error <- "UNDETECTED ERROR"
    } else {
      backtrace_line_pos <- grepl("^Backtrace:", out_file)
      if(!any(backtrace_line_pos)) backtrace_line_pos[length(out_file)+1] <- TRUE
      error_lines_short <- out_file[which(error_line_pos):(which(backtrace_line_pos)-1)]
      error_lines_all <- out_file[which(error_line_pos):length(out_file)]
      res$error <- error_lines_all
    }
  }

  if(echo) {
    cat("\t-Memory: no info\n")
    cat("\t-Approx time: ", intrnl_time_format(time_after-time_before), "\n")
    if(!has_normal_end){
      cat("\t-ERROR: ", error_lines_short, "\n")
      # message(sys$error)
      # cat("\n")
    }
    cat(paste("\t-Done with file", path, "\n"))
  }

  res
}


#### TEST ####
if(FALSE){
  path_rscripts <- system.file("r_scripts_fake", package = "matPkg")
  path_temp <- tempdir()
  dir_dat <- mat_99_list_Rfiles(path_rscripts)

  ##
  matPkg:::source_throw(path=dir_dat$full_path[[1]])
  matPkg:::source_rcmd_batch(path=dir_dat$full_path[[1]])

  matPkg:::source_throw(dir_dat$full_path[[3]])
  matPkg:::source_rcmd_batch(path=dir_dat$full_path[[3]])

  matPkg:::source_throw(path=dir_dat$full_path[[8]])
  matPkg:::source_rcmd_batch(path=dir_dat$full_path[[8]])

  ## all
  out <- dir_dat %>%
    # head(2) %>%
    mutate(source_intern = map(full_path, matPkg:::source_throw),
           source_extern = map(full_path, matPkg:::source_rcmd_batch))

  out2 <- out %>%
    select(filename, starts_with("source")) %>%
    gather(fo, output, -filename) %>%
    mutate(output = map(output, ~map(., ~if(is.null(.)) "NULL" else .))) %>%
    unnest(output)
  # out2$output[[1]] %>% as_tibble()
  # unnest(c(source_intern, source_extern))

}


### sources nicest
## probably with: callr
# options <- rscript_process_options(script = dir_dat$full_path[[3]])
# rp <- rscript_process$new(options)
# rp$get_exit_status()
# rp$get_error_file()
# rp$get_start_time()
# rp$read_all_error()
# rp$read_all_error_lines()

