#' Get spatial extent
#'
#' A class agnostic function to return the bounding extent (i.e. bounding box) of
#' a spatial object. 
#' 
#' @param x A spatial object, file path or source
#' @param ... Not used
#' @family spatial helpers (class agnostic)
#' @rdname get_ext
#' @return A numeric vector of length 4. Values are returned as: "xmin", "xmax", "ymin", "ymax"
#' @examples
#' f <- system.file("ex/elev.tif", package="terra") 
#' get_ext(f)
#' get_ext(terra::rast(f))
#' f2 <- system.file("ex/lux.shp", package="terra") 
#' get_ext(f2)
#' @export
get_ext <- function(x, ...) {
  UseMethod("get_ext")
}

#' @rdname get_ext
#' 
#' @export
get_ext.SpatRaster <- function(x,...){
  # terra::ext(x)
  x@ptr$extent@.xData[["vector"]]
}

#' @rdname get_ext
#' 
#' @export
get_ext.SpatVector <- function(x,...){
  as.vector(terra::ext(x)) # Can't locate the correct slot so using the ext function.
}

#' @rdname get_ext
#' 
#' @export
get_ext.sf <- function(x,...){
  # as.vector(sf::st_bbox(x))[c(1,3,2,4)]
  attr(x[[attr(x, "sf_column")]], "bbox")[c("xmin", "xmax", "ymin", "ymax")]
}

#' @rdname get_ext
#' 
#' @export
get_ext.sfc <- function(x, ...){
  attr(x, "bbox")[c("xmin", "xmax", "ymin", "ymax")]
}

#' @rdname get_ext
#' 
#' @export
get_ext.stars <- function(x, ...){
  stars_ext(x)
}

#' @rdname get_ext
#' 
#' @export
get_ext.stars_proxy <- function(x, ...){
  stars_ext(x)
}

#' @rdname get_ext
#' 
#' @export
get_ext.ezgrid <- function(x, ...){
  x$extent
}

#' @rdname get_ext
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