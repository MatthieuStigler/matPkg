
#' List files in a directory
#'
#' @param path dir path
#' @param pattern,recursive passed to list.files
#'
#' @export
mat_list_dir <- function(path, pattern = "\\.tif", recursive = TRUE) {
  list.files(path, full.names = TRUE, pattern=pattern, recursive = recursive) %>%
    tibble(full_path=.data$.) %>%
    mutate(filename= basename(.data$full_path)) %>%
    select(-.data$full_path, .data$full_path)
}
