#' Converting to expression for plots
#'
#'Simply add annoying ~ as space, and converts as expression
#' @param x A single charracter string
#' @param echo Print before eval? For de-bugging
#' @return An expression
#' @examples
#' plot(1)
#' title(mat_str_to_expr("This is R^2"))
#' plot(1)
#' title(mat_str_to_expr(x="This is R^2 in 1%", echo=TRUE))
#' plot(1)
#' title(mat_str_to_expr(x="This is R^2 in %", echo=TRUE))
#' @export
mat_str_to_expr <- function(x, echo=FALSE){
  str_prep <- str_replace_all(x, " ", "~") %>%
    str_replace_all("(&|%)", "*'\\1'") %>%
    str_replace_all("in", "'in'") %>%
    str_replace_all("\\(\\*", "(") %>%
    str_replace_all("~\\*", "~")
  if(echo) print(str_prep)
  parse(text=str_prep)
}
