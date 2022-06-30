#' get extent of spatial object
#'
#' get extent of spatial object
#' @title get_ext: get projection of a spatial object
#' @param x ...
#' @export
get_ext <- function(x, ...) {
  UseMethod("get_ext")
}

#' @rdname get_proj
#' 
#' @export
get_ext.SpatRaster <- function(x,...){
  terra::ext(x)
}

#' @rdname get_proj
#' 
#' @export
get_ext.SpatVector <- function(x,...){
  terra::ext(x)
}

#' @rdname get_proj
#' 
#' @export
get_ext.sf <- function(x,...){
  as.vector(sf::st_bbox(x))[c(1,3,2,4)]
}

#' @rdname get_proj
#' 
#' @export
get_ext.sfc <- function(x, ...){
  as.vector(sf::st_bbox(x))[c(1,3,2,4)]
}

#' @rdname get_proj
#' 
#' @export
get_ext.stars <- function(x, ...){
  as.vector(sf::st_bbox(x))[c(1,3,2,4)]
}

#' @rdname get_proj
#' 
#' @export
get_ext.stars_proxy <- function(x, ...){
  as.vector(sf::st_bbox(x))[c(1,3,2,4)]
}