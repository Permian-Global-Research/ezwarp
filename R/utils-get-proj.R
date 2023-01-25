#' Get Spatial Projection
#'
#' A class agnostic function to return the projection of a spatial object or 
#' source. returned projection uses wkt format.
#' 
#' @param x A spatial object, file path or source
#' @param ... Not used
#' @family spatial helpers (class agnostic)
#' @return A character - WKT projection string.
#' 
#' @examples
#' f <- system.file("ex/elev.tif", package="terra") 
#' get_proj(f)
#' get_proj(terra::rast(f))
#' f2 <- system.file("ex/lux.shp", package="terra") 
#' get_proj(f2)
#' 
#' @export
get_proj <- function(x, ...) {
  UseMethod("get_proj")
}

#' @rdname get_proj
#' 
#' @export
get_proj.SpatRaster <- function(x,...){
  # terra::crs(x)
  terra_crs(x)
}

#' @rdname get_proj
#' 
#' @export
get_proj.SpatVector <- function(x,...){
  # terra::crs(x)
  terra_crs(x)
}

#' @rdname get_proj
#' 
#' @export
get_proj.sf <- function(x,...){
  # sf::st_crs(x)$wkt
  attr(x[[attr(x, "sf_column")]], "crs")$wkt
}

#' @rdname get_proj
#' 
#' @export
get_proj.sfc <- function(x, ...){
  # sf::st_crs(x)$wkt
  attr(x, "crs")$wkt
}

#' @rdname get_proj
#' 
#' @export
get_proj.stars <- function(x, ...){
  # sf::st_crs(x)$wkt
  stars_crs(x)
}

#' @rdname get_proj
#' 
#' @export
get_proj.stars_proxy <- function(x, ...){
  stars_crs(x)
}

#' @rdname get_proj
#' 
#' @export
get_proj.ezgrid <- function(x, ...){
  x$projection
}

#' @rdname get_proj
#' 
#' @export
get_proj.character <- function(x, ...){
  read_spat_info(x, val='projection')
}



stars_crs <- function(x){
  d <- attr(x, "dimension")
  d[[1]]$refsys[["wkt"]]
}

terra_crs <- function(x){
  x@ptr$get_crs("wkt")
}