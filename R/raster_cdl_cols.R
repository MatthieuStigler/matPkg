#' Get relevant CDL colors
#'
#' From a CDL raster, subset the included CDL colors, return them
#' @param CDL_raster a raster containign CDL values
#' @importFrom raster unstack unique
#' @export
mat_cdl_cols <- function(CDL_raster) {

  if(raster::nlayers(CDL_raster)>1)  {
    un_vals <- unique(unlist(lapply(unstack(CDL_raster), unique)))

  } else {
    un_vals <- unique(CDL_raster)
  }
  tibble(Value=un_vals) %>%
    left_join(select(matPkg:::CDL_colors, .data$Value, .data$Category, .data$CDL_cols), by = "Value") %>%
    arrange(Value)

}
