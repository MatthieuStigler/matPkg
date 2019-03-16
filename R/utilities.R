#' Convert Nan to NA
#' @param x Scalar/vector
#' @export
nan_to_na <-  function(x) ifelse(!is.finite(x), NA, x)

#' is_true: vectorized isTRUE
#' @param x vector
#'@export
is_true <-  function(x) map_lgl(x, isTRUE)

#' Check 0 rows
#' @param df data
#' @param message_ok the ok message
#' @examples
#' mat_check_0row(df=subset(iris, Species=="aaa"))
#'@export
mat_check_0row <-  function(df, message_ok ="OK") {
  if(nrow(df)==0) message_ok else stop("Does not have z rows!")
}


#' Show method called on object
#' @param generic the generic
#' @param \ldots the object on which to use the generic
#' @export
#' @examples
#' data(iris_tb)
#' mat_find_method(print, iris)
#' mat_find_method(print, iris_tb)
mat_find_method <- function(generic, ...) {
  ch <- deparse(substitute(generic))
  f <- X <- function(x, ...) UseMethod("X")
  for(m in utils::methods(ch)) assign(sub(ch, "X", m, fixed = TRUE), "body<-"(f, value = m))
  X(...)
}
