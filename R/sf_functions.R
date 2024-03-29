
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
#' @param add_xy Logical. Add xy coordinates?
#' @examples
#' library(sf)
#' shp <- st_sf(a=c(3,2), geometry=st_sfc(st_point(1:2), st_point(2:3)))
#' mat_st_to_df(shp)
#' mat_st_to_df(shp, add_xy = TRUE)
#'@export
mat_st_to_df <- function(shp, add_xy = FALSE) {

  if(!inherits(shp, c("sf", "sfc"))) {
    return(as_tibble(shp))
  }

  res <-   shp %>%
    sf::st_set_geometry(NULL) %>%
    as_tibble()

  if(add_xy) {
    if(!all(sf::st_geometry_type(shp)=="POINT")) {
      warning("Not points!? Taking centroids!")
      shp <- sf::st_centroid(shp)
    }
    coords_df <-  as_tibble(sf::st_coordinates(shp)) %>%
      select("X", "Y")
    res <- res %>%
      dplyr::bind_cols(coords_df)
  }
  res

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

#' Extent to sf
#'
#' @param x raster Extent object
#' @param ... passed on to st_sf
#' @export
#' @importFrom sf st_as_sf
#' @importFrom methods as
#' @examples
#' library(raster)
#' library(sf)
#' r1 <- raster(nrows=108, ncols=21, xmn=0, xmx=100)
#' r2 <- raster(nrows=108, ncols=21, xmn=50, xmx=150)
#' st2 <- rbind(st_as_sf(extent(r1)),
#'              st_as_sf(extent(r2)))
#' plot(st2, border = 1:2)
#'
st_as_sf.Extent <- function(x, ...) {
  x %>%
    as("SpatialPolygons") %>%
    sf::st_as_sf(., ...) %>%
    sf::st_set_crs(sf::st_crs(x))
}
