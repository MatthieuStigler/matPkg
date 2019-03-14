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
