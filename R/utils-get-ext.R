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
  # terra::ext(x)
  x@ptr$extent@.xData[["vector"]]
}

#' @rdname get_proj
#' 
#' @export
get_ext.SpatVector <- function(x,...){
  as.vector(terra::ext(x)) # Can't locate the correct slot so using the ext function.
}

#' @rdname get_proj
#' 
#' @export
get_ext.sf <- function(x,...){
  # as.vector(sf::st_bbox(x))[c(1,3,2,4)]
  attr(x[[attr(x, "sf_column")]], "bbox")[c("xmin", "xmax", "ymin", "ymax")]
}

#' @rdname get_proj
#' 
#' @export
get_ext.sfc <- function(x, ...){
  attr(x, "bbox")[c("xmin", "xmax", "ymin", "ymax")]
}

#' @rdname get_proj
#' 
#' @export
get_ext.stars <- function(x, ...){
  stars_ext(x)
}

#' @rdname get_proj
#' 
#' @export
get_ext.stars_proxy <- function(x, ...){
  stars_ext(x)
}

#' @rdname get_proj
#' 
#' @export
get_ext.ezgrid <- function(x, ...){
  x$extent
}

#' @rdname get_proj
#' 
#' @export
get_ext.character <- function(x, ...){
  read_spat_info(x, val='extent')
}


stars_ext <- function(x){
  d <- attr(x, "dimension")
  c(c(d[[1]]$from -1, d[[1]]$to) * d[[1]]$delta + d[[1]]$offset,
    c(d[[2]]$from - 1, d[[2]]$to) * -d[[2]]$delta - d[[2]]$offset)
}