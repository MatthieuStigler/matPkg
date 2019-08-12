#' Formula to character
#'
#' @param x formula
#' @param \ldots unused
#' @examples
#' f <-  aha ~ hih + oho
#' as.character(f)
#' @export
as.character.formula <- function (x, ...) {
  form <- paste(deparse(x), collapse = " ")
  form <- gsub("\\s+", " ", form, perl = FALSE)
  return(form)
}
