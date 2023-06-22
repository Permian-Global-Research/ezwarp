#' Build a stars object from vapour vector
#'
#' @param p params from ezwarp
#' @param v a numeric vector with raster values
#'
#' @return a stars object
#' @noRd
build_stars <- function(p, v) {
  # TODO: Suboptimal - creating a SpatRaster and then converting to stars.
  stars::st_as_stars(build_SpatRaster(p, v))
}
