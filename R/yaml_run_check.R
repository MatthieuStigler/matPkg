#' @importFrom utils getFromNamespace
read_utf8 = getFromNamespace("read_utf8", "rmarkdown")
parse_yaml_front_matter = getFromNamespace("parse_yaml_front_matter", "rmarkdown")

#' Read YAML header to list
#' @param file_path The file path
#' @param encoding just in case
#' @export
mat_parse_yaml <- function(file_path, encoding = getOption("encoding")){
  input_lines <- read_utf8(file_path, encoding)
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
#' @examples
#' mat_list_Rfiles("R")
#' @export
mat_list_Rfiles <- function(dir_path, no_old = TRUE) {
  dir_df <- mat_list_dir(dir_path, pattern="\\.R", add_ext = TRUE)

  res <- dir_df %>%
    mutate(number_char = str_extract(.data$filename, "([0-9]_)+") %>%
             str_replace("_$", ""),
           number = .data$number_char %>%
             str_replace("_", "\\.") %>%
             str_replace_all("_", "") %>%
             as.numeric(),
           yaml = map(.data$full_path, mat_parse_yaml),
           has_yaml=map_lgl(.data$yaml, ~ !rlang::is_empty(.)),
           has_runMat  = map2_lgl(.data$has_yaml, .data$yaml, ~if(.x)  "runMat" %in% names(.y) else FALSE ),
           runMat_val = map2_lgl(.data$has_runMat, .data$yaml, ~if(.x)  .y$runMat else FALSE )) %>%
    select(-.data$full_path, .data$full_path)
  if(no_old) res <-  res %>%
    filter(!stringr::str_detect(.data$full_path, "old/"))
  res
}

#' Run files from a df-df
#'
#' @param df the df of files, from mat_list_Rfiles()
#' @export
mat_run_Rfiles <- function(df) {
  df %>%
    mutate(try = map(.data$full_path, ~purrr::safely(base::source)(.))) %>%
    mutate(error=map(.data$try, ~.[["error"]]),
           has_error= map_lgl(.data$error, ~!is.null(.)),
           error=map_chr(.data$error, ~ifelse(is.null(.), NA, .) %>% as.character)) %>%
    select("filename", "try", "has_error", "error")
}
