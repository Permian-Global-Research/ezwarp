#' get projection of spatial object
#'
#' get projection of spatial object
#' @title get_proj: get projection of a spatial object
#' @param x ...
#' @export
get_proj <- function(x, ...) {
  UseMethod("get_proj")
}

#' @rdname get_proj
#' 
#' @export
get_proj.SpatRaster <- function(x,...){
  terra::crs(x)
}

#' @rdname get_proj
#' 
#' @export
get_proj.SpatVector <- function(x,...){
  terra::crs(x)
}

#' @rdname get_proj
#' 
#' @export
get_proj.sf <- function(x,...){
  sf::st_crs(x)$wkt
}

#' @rdname get_proj
#' 
#' @export
get_proj.sfc <- function(x, ...){
  sf::st_crs(x)$wkt
}

#' @rdname get_proj
#' 
#' @export
get_proj.stars <- function(x, ...){
  sf::st_crs(x)$wkt
}

#' @rdname get_proj
#' 
#' @export
get_proj.stars_proxy <- function(x, ...){
  sf::st_crs(x)$wkt
}