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
#' @param warn_message message if only wants wanr
#' @param stop_message message when stopping
#' @param print_df print when error?
#' @examples
#' mat_check_0row(df=subset(iris, Species=="aaa"))
#' mat_check_0row(df=subset(iris, Species=="setosa"), warn_message ="make sure no setosa?")
#' \dontrun{
#'   mat_check_0row(df=subset(iris, Species=="setosa"))
#'   mat_check_0row(df=subset(iris, Species=="setosa"), stop_message ="Should not have setosa")
#' }
#'@export
mat_check_0row <-  function(df, message_ok ="OK", warn_message = NULL,
                            stop_message=NULL, print_df=TRUE) {
  if(nrow(df)==0) {
    return(message_ok)
  } else if(!is.null(warn_message)) {
    warning(warn_message)
  } else {
    if(is.null(stop_message)) stop_message <- "Does not have z rows!"
    if(print_df) print(df)
    stop(stop_message)
  }
}


#' remove duplicated in vector
#' @param x vector
#' @param fill which value to use
#' @param warn SHould warn for non-sorted?
#' @examples
#' mat_keep_first(rep(1:3, 1:3))
#' @export
mat_keep_first <- function(x, fill="", warn = TRUE) {
  if(is.factor(x)) x <-  as.character(x)
  if(!all(sort(x)==x) & warn) warning("Make sure sorted output?")
  ifelse(duplicated(x), fill, x)
}

2#' Show method called on object
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

#' Check number even
#'
#' @param x vector
#' @export
#' @examples
#' vec <-  1:6
#' names(vec) <-  vec
#' mat_check_even(vec)
mat_check_even <- function(x) !as.logical(round(x)%%2)

if(FALSE){
  # should round first?
  # abs(sqrt(x)^2%%2-0)<0.0000001
  vec <-  1:6
  names(vec) <-  vec

  setNames(mat_check_even(vec), names(vec))
  setNames(mat_check_even(sqrt(vec)^2), names(vec))
  abs(sqrt(vec)^2%%2-0)<0.0000001
}
