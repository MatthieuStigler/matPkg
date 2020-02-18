
################################
## Utilities
################################

intrl_get_pkg <- function(x) {
  out <- str_extract_all(x, "library\\(.+\\)")
  out2 <- unlist(out[sapply(out, length)>0])
  str_remove_all(out2, "library\\(|\\)")
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
#' @export
mat_88_list_pkgs <- function(scripts_file, warn_missing = TRUE, unique =TRUE) {

  pkgs_here <- as_tibble(utils::installed.packages())

  out <- scripts_file %>%
    mutate(script = map(.data$full_path, readLines),
           pkgs = map(.data$script, ~intrl_get_pkg(.) %>%  tibble::enframe(name=NULL, value="package"))) %>%
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

#' @param dir_path additional directory
#' @export
#' @rdname mat_88_list_pkgs
mat_88_list_paths <- function(scripts_file, warn_missing = TRUE, unique =TRUE, dir_path=".") {

  # Argcheck
  dir_path <- str_remove(dir_path, "/$")

  #
  out <- scripts_file %>%
    mutate(script = map(.data$pfull_path, readLines),
           path_data = map(.data$pscript, ~tibble::enframe(intrl_get_path(.), value = "path", name=NULL))) %>%
    select(.data$filename, .data$path_data) %>%
    mutate(n_path = map_int(.data$path_data, nrow)) %>%
    filter(.data$n_path>0) %>%
    unnest(.data$path_data) %>%
    mutate(base_path = dirname(.data$path),
           exists = map_lgl(.data$base_path, ~file.exists(paste(dir_path, ., sep="/"))))

  if(unique){
    out <-  out %>%
      distinct(.data$base_path, .data$exists)
  }

  if(warn_missing & nrow(filter(out, !.data$exists))) {
    miss <- filter(out, !.data$exists)
    warning("Some missing folders?", miss)
  }
  out
}


################################
### TEST
################################

if(FALSE){
  ## read stuff
  path_rscripts <- system.file("r_scripts_fake", package = "matPkg")
  path_rscripts <- "/home/matifou/gitReps/my_github/matPkg/inst/r_scripts_fake/"
  dir_dat <- mat_99_list_Rfiles(path_rscripts)

  ##
  mat_88_list_pkgs(dir_dat)
  mat_88_list_paths(dir_dat, dir_path = "/home/matifou/Dropbox/Documents/Uni/Stanford/Crop Insurance Project/analysis/cropIns_gCloud") %>%
    filter(!exists)

  dir_dat %>%
    mutate(script = map(full_path, readLines),
           pkgs = map(script, ~get_pkg(.) %>%  enframe(name=NULL, value="package"))) %>%
    select(filename, pkgs) %>%
    unnest(pkgs)


}
