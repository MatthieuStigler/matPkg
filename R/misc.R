#' Converting to expression for plots
#'
#'Simply add annoying ~ as space, and converts as expression
#' @param x A single charracter string
#' @return An expression
#' @examples
#' plot(1)
#' title(mat_str_to_expr("This is R^2"))
#' @export
mat_str_to_expr <- function(x){
  parse(text=stringr::str_replace_all(x, " ", " ~ "))
}
