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
#' @param n_row number of rows to check for
#' @examples
#' mat_check_0row(df=subset(iris, Species=="aaa"))
#' mat_check_0row(df=subset(iris, Species=="setosa"), warn_message ="make sure no setosa?")
#' \dontrun{
#'   mat_check_0row(df=subset(iris, Species=="setosa"))
#'   mat_check_0row(df=subset(iris, Species=="setosa"), stop_message ="Should not have setosa")
#' }
#'@export
mat_check_0row <-  function(df, message_ok ="OK", warn_message = NULL,
                            stop_message=NULL, print_df=TRUE,
                            n_row = 0) {
  if(nrow(df)==n_row) {
    return(message_ok)
  } else if(!is.null(warn_message)) {
    warning(warn_message)
  } else {
    if(is.null(stop_message)) stop_message <- paste("Does not have", n_row, "rows!")
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
  # if(is.factor(x)) x <-  as.character(x)
  x_char <- as.character(x)
  if(!all(sort(x_char)==x_char) & warn) warning("Make sure sorted output?")
  ifelse(duplicated(x_char), fill, x_char)
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


#' Cor with NA
#' @param x,y,use,method as in cor
#' @export
#' @examples
#' iris2 <- iris[, 1:4]
#' iris2[c(3, 8),1] <-  NA
#' mat_cor_na(iris2)
#' with(iris2, mat_cor_na(Sepal.Length, Petal.Width))
#' with(iris2, mat_cor_na_df(Sepal.Length, Petal.Width))
mat_cor_na <- function(x, y = NULL, use = "pairwise.complete.obs", method = "pearson") {
  stats::cor(x=x, y = y, use = use, method = method)
}

#' @export
#' @rdname mat_cor_na
mat_cor_na_df <- function(x, y=NULL, use = "pairwise.complete.obs", method = "pearson") {
  if(is.null(y)) stop("Need 2 univariate x, y. Use instead mat_cor_na() %>% mat_cor_to_df")
  stats::cor(x=x, y = y, use = use, method = method) %>%
    tibble::enframe(name=NULL, value="cor")
}


#' Add p-value and paste
#' @param estimate Parameters of interest
#' @param p_value p-values for the estimate
#' @param digits rounding before pasting
#' @param \ldots Currently unused
#' @export
#' @seealso \code{\link{format.pval}}
#' @examples
#' p_vals <- c(0.01, 0.0001, 0.05, 0.09)
#' mat_pval_cat(p_vals, p_vals)
#' mat_pval_cat(p_vals, p_vals, digits=4)
mat_pval_cat <- function(estimate, p_value, digits=2, ...){
  p_stars <- intrl_p_vals(p_value, ...)
  paste(round(estimate, digits = digits), p_stars, sep="")
}

#' Add stars
#' just copied from gtools::stars.pval(), gtools 3.8.2
#' @noRd
intrl_p_vals <- function(p_value){
  unclass(stats::symnum(p_value, corr = FALSE, na = FALSE,
                        cutpoints = c(0,0.001, 0.01, 0.05, 0.1, 1),
                        symbols = c("***", "**", "*", ".", " ")))
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
