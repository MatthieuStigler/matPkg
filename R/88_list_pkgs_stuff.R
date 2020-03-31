
################################
## Utilities
################################

intrl_get_pkg <- function(x) {
  ## extract if has library()
  out_lib <- str_extract_all(x, "(?<=library\\()[aA-zZ0-9\\.]+(?=\\))")

  ## extract if has ::
  out_dot2 <- str_extract_all(x, "[aA-zZ0-9\\.]+(?=\\:\\:)")

  ## get both, clean
  out_both <- c(out_lib, out_dot2)
  unlist(out_both[sapply(out_both, length)>0])
}

intrl_get_path <- function(x) {
  out <- str_extract_all(x, "(data_[a-z]+|output)/.+\\.[a-z]{2,3}")
  unlist(out[sapply(out, length)>0])
}


################################
### List packages
################################

#' List required packages
#'
#' @param scripts_file data from \code{\link{mat_99_run_Rfiles}}
#' @param warn_missing Should (yes) show warning about missing packages?
#' @param unique Should (yes) return only distinct packages. Otherwise returns also script name.
#' @examples
#' path_rscripts <- system.file("r_scripts_fake", package = "matPkg")
#' dir_dat <- mat_99_list_Rfiles(path_rscripts)
#' mat_88_list_pkgs(dir_dat)
#' mat_88_list_paths(dir_dat)
#' @export
mat_88_list_pkgs <- function(scripts_file, warn_missing = TRUE, unique =TRUE) {

  pkgs_here <- as_tibble(utils::installed.packages())

  out <- scripts_file %>%
    mutate(script = map(.data$full_path, ~readLines(., warn=FALSE)),
           pkgs = map(.data$script, ~intrl_get_pkg(.) %>%
                        tibble::enframe(name=NULL, value="package") %>%
                        mutate(package = as.character(package)))) %>%
    select(.data$filename, .data$pkgs) %>%
    unnest(.data$pkgs) %>%
    mutate(is_installed = map_lgl(.data$package, ~. %in% pkgs_here$Package))

  ## Distinct value
  if(unique){
    out <- distinct(out, .data$package, .data$is_installed)
  }

  ## Tell which ones missing?
  if(warn_missing & any(!out$is_installed)) {
    miss <- out %>%
      filter(!.data$is_installed)
    cmd_inst_R <- paste0("install.packages(c('", paste(miss$package, collapse = "', '"), "'))")
    cmd_inst_apt <- paste0("sudo apt install --ignore-missing ", paste(paste0("r-cran-", miss$package), collapse = " "))
    warning("Missing packages! Run:\n\t", cmd_inst_R,"\n\t",  cmd_inst_apt)
  }
  out
}


#' @export
#' @rdname mat_88_list_pkgs
mat_88_check_pkgs <- function(scripts_file) {
  out <- mat_88_list_pkgs(scripts_file=scripts_file, warn_missing = TRUE, unique =TRUE) %>%
    filter(!.data$is_installed)
  if(nrow(out)==0) print("Ok!")
}


#' @param dir_path additional directory
#' @param file_ignore files to ignore. NULL
#' @export
#' @rdname mat_88_list_pkgs
mat_88_list_paths <- function(scripts_file, warn_missing = TRUE, unique =TRUE, dir_path=".",
                              file_ignore=NULL) {

  # Arg check
  dir_path <- str_remove(dir_path, "/$")

  ## remove some files
  if(!is.null(file_ignore)) {
    scripts_file <- scripts_file %>%
      filter(!str_detect(.data$filename, file_ignore))
  }

  # Main call
  out <- scripts_file %>%
    mutate(script = map(.data$full_path, ~readLines(., warn=FALSE)),
           path_data = map(.data$script, ~tibble::enframe(intrl_get_path(.), value = "path", name=NULL))) %>%
    select(.data$filename, .data$path_data) %>%
    mutate(n_path = map_int(.data$path_data, nrow)) %>%
    filter(.data$n_path>0)

  if(nrow(out)==0) return(out)
  out <- out %>%
    unnest(.data$path_data) %>%
    mutate(base_path = dirname(.data$path),
           exists = map_lgl(.data$base_path, ~file.exists(paste(dir_path, ., sep="/"))))

  if(unique){
    out <-  out %>%
      distinct(.data$base_path, .data$exists)
  }

  if(warn_missing & nrow(filter(out, !.data$exists))) {
    miss <- filter(out, !.data$exists)
    warning("Some missing folders?")
    print(miss)
  }
  out
}

#' @export
#' @rdname mat_88_list_pkgs
mat_88_check_paths <- function(scripts_file,  dir_path=".", file_ignore=NULL, unique=TRUE) {
  out <- mat_88_list_paths(scripts_file=scripts_file, warn_missing = TRUE, unique =unique, dir_path=dir_path, file_ignore = file_ignore) %>%
    filter(!.data$exists)
  if(nrow(out)==0) print("Ok!")
}

################################
### TEST
################################

if(FALSE){
  library(matPkg)
  ## read stuff
  path_rscripts <- system.file("r_scripts_fake", package = "matPkg")
  dir_dat <- mat_99_list_Rfiles(path_rscripts)

  ##
  mat_88_list_pkgs(scripts_file = dir_dat)
  mat_88_list_paths(scripts_file = dir_dat)


  mat_88_list_paths(dir_dat, dir_path = "/home/matifou/Dropbox/Documents/Uni/Stanford/Crop Insurance Project/analysis/cropIns_gCloud") %>%
    filter(!exists)



}
