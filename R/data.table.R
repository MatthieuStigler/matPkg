#' Set (back) to tibble
#'
#' @param df initial data
#' @param value The old class
#' @examples
#' library(data.table)
#' iris_test <- copy(tibble::as_tibble(iris))
#' setDT(iris_test)
#' iris_test
#' setTb(iris_test)
#' iris_test
#' @export
setTb <- function(df, value=c("tbl_df", "tbl", "data.frame")) {
  data.table::setattr(df, "class", value=value)
}

if(FALSE){
  library(data.table)
  iris_test <- copy(tibble::as_tibble(iris))
  setDT(iris_test)
  iris_test
  setTb(iris_test)
  iris_test

  ## do emoty?
  iris_test
  setTb(iris_test)
  iris_test

}
