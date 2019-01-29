
#' List files in a directory
#' @export
mat_list_dir <- function(path, pattern = "\\.tif", recursive = TRUE) {
  list.files(path, full.names = TRUE, pattern=pattern, recursive = recursive) %>%
    data_frame(full_path=.) %>%
    mutate(filename= basename(full_path)) %>%
    select(-full_path, full_path)
}
