
#' Read st silently
#'
#' Simpelr fun
#' @param dsn,quiet,stringsAsFactors check doc
#' @param \ldots passed too
#' @importFrom sf st_read
#' @export
mat_st_read <-  function(dsn, ..., quiet = TRUE, stringsAsFactors = FALSE) {
  st_read(dsn, ..., quiet = quiet, stringsAsFactors = stringsAsFactors)
}

#' mat_st_to_df
#' @param shp shp
#'@export
mat_st_to_df <- function(shp) {
  shp %>%
    sf::st_set_geometry(NULL) %>%
    as_data_frame()

}

#' mat_st_to_df
#' @param df data frame
#' @param lat_var,lng_var name of cloumns
#' @param crs crs
#' @examples
#' df <- data.frame(x = c(1,2,3), y = c(3,4,2))
#' df_st <- mat_st_df_to_pt(df, lat_var = x, lng_var = y, crs = 4326)
#' plot(df_st)
#'@export
mat_st_df_to_pt <- function(df, lat_var, lng_var, crs=NULL) {
  if(is.null(crs)) stop("Specify CRS?")
  lat_vari <- rlang::enquo(lat_var)
  lng_vari <- rlang::enquo(lng_var)

  res_t <- df %>%
    mutate("geometry" = map2(!!lat_vari, !!lng_vari, ~sf::st_point(c(.y, .x)))) %>%
    select(-!!lat_vari, -!!lng_vari)
  res <- sf::st_sf(geometry=sf::st_sfc(res_t$geometry, crs = crs),
               select(res_t, -"geometry"))
  res
}
