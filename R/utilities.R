
#' @export
nan_to_na <-  function(x) ifelse(!is.finite(x), NA, x)

#'@export 
is_true <-  function(x) map_lgl(x, isTRUE)