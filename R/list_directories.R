intrl_URLdecode_vec <- function(x) Vectorize(utils::URLdecode)(x)


#' List files in a directory
#'
#' @param path dir path
#' @param pattern,recursive passed to list.files
#' @param add_ext Add column with extension?
#' @param \ldots Additional arguments to \code{\link{list.files}}
#'
#' @export
#' @examples
#' mat_list_dir(path = ".", pattern = "R")
mat_list_dir <- function(path, pattern = ".R", recursive = TRUE, add_ext = FALSE, ...) {

  ## check
  if(any(str_detect(path, "%20"))) path <- intrl_URLdecode_vec(path)
  if(!any(file.exists(path))) stop("Missing folder?")

  ## read
  li <- list.files(path, full.names = TRUE, pattern=pattern, recursive = recursive, ...)

  ## process
  res <- tibble(full_path=li) %>%
    mutate(filename= basename(.data$full_path))
  if(add_ext) res <-  res %>%
    mutate(ext = tools::file_ext(.data$filename))

  ## return
  res %>%
    dplyr::relocate("full_path", .after = everything())

}
