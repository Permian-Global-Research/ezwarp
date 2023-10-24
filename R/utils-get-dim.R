#' Get spatial Dimension
#'
#' A class agnostic function to return the dimensions of a spatial object. Only
#' applicable for rasters - spatial vectors will return NULL.
#'
#' @param x A spatial object, file path or source
#' @param ... Not used
#' @family spatial helpers (class agnostic)
#' @rdname get_dim
#'
#' @examples
#' f <- system.file("ex/elev.tif", package = "terra")
#' get_dim(f)
#' get_dim(terra::rast(f))
#' f2 <- system.file("ex/lux.shp", package = "terra")
#' get_dim(f2)
#'
#' @export
get_dim <- function(x, ...) {
  UseMethod("get_dim")
}

#' @rdname get_dim
#'
#' @export
get_dim.SpatRaster <- function(x, ...) {
  c(terra::ncol(x), terra::nrow(x))
}

#' @rdname get_dim
#'
#' @export
get_dim.SpatVector <- function(x, ...) {
  NULL
}

#' @rdname get_dim
#'
#' @export
get_dim.sf <- function(x, ...) {
  NULL
}

#' @rdname get_dim
#'
#' @export
get_dim.sfc <- function(x, ...) {
  NULL
}

#' @rdname get_dim
#'
#' @export
get_dim.stars <- function(x, ...) {
  stars_dim(x)
}

#' @rdname get_dim
#'
#' @export
get_dim.stars_proxy <- function(x, ...) {
  stars_dim(x)
}

#' @rdname get_dim
#'
#' @export
get_dim.character <- function(x, ...) {
  read_spat_info(x, val = "dimension")
}

#' @rdname get_dim
#'
#' @export
get_dim.ezgrid <- function(x, ...) {
  x$dimension
}

stars_dim <- function(x) {
  d <- attr(x, "dimension")
  c(d[[1]]$to - d[[1]]$from + 1, d[[2]]$to - d[[2]]$from + 1)
}
