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


#' As data frame for data()
#'
#' @param x formula
#' @param \ldots unused
#' @param stringsAsFactors cf \code{\link{as.data.frame}}
#' @examples
#' datas <-  data()
#' head(as.data.frame(datas))
#' @export
as.data.frame.packageIQR <-  function(x, ..., stringsAsFactors = FALSE) {
  as.data.frame(x$results, ..., stringsAsFactors=stringsAsFactors)
}

