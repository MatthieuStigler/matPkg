
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
mat_list_dir <- function(path, pattern = "\\.tif", recursive = TRUE, add_ext = FALSE, ...) {

  ## check
  if(!file.exists(path)) stop("Missing folder?")
  if(str_detect(path, "%20")) path <- utils::URLdecode(path)

  ## read
  li <- list.files(path, full.names = TRUE, pattern=pattern, recursive = recursive, ...)

  ## process
  res <- tibble(full_path=li) %>%
    mutate(filename= basename(.data$full_path))
  if(add_ext) res <-  res %>%
    mutate(ext = tools::file_ext(.data$filename))

  ## return
  res %>%
    select(-.data$full_path, .data$full_path)

}
