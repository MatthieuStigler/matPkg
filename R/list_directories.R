
#' List files in a directory
#'
#' @param path dir path
#' @param pattern,recursive passed to list.files
#' @param add_ext Add column with extension?
#'
#' @export
#' @examples
#' mat_list_dir(path = ".", pattern = "R")
mat_list_dir <- function(path, pattern = "\\.tif", recursive = TRUE, add_ext = FALSE) {
  li <- list.files(path, full.names = TRUE, pattern=pattern, recursive = recursive)
  res <- tibble(full_path=li) %>%
    mutate(filename= basename(.data$full_path))
  if(add_ext) res <-  res %>%
    mutate(ext = tools::file_ext(filename))

  ## return
  res %>%
    select(-.data$full_path, .data$full_path)

}
