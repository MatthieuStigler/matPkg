
#' Read st silently
#'
#' Simpelr fun
#' @param dsn,quiet,stringsAsFactors check doc
#' @param \ldots passed too
#' @importFrom sf st_read
#' @export
mat_st_read <-  function(dsn, ..., quiet = TRUE, stringsAsFactors = FALSE) {
  st_read(dsn, ..., quiet = quiet, stringsAsFactors)
}
