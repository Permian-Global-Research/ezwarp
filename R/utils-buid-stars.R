#' Build a stars object from vapour vector
#'
#' @param p params from ezwarp
#' @param v a numeric vector with raster values
#'
#' @return a stars object
build_stars <- function(p, v) {
  # aa <- array(unlist(v, use.names = FALSE), c(p$dimension[1], p$dimension[2], length(v)))[,p$dimension[2]:1, , drop = FALSE]
  # if (length(v) == 1) aa <- aa[,,1, drop = TRUE]
  # r <- stars::st_as_stars(sf::st_bbox(c(xmin =p$extent[1], ymin = p$extent[3], xmax =p$extent[2], ymax =p$extent[4])),
  #                         nx = p$dimension[1], ny = p$dimension[2], values = aa)
  # 
  # r <- sf::st_set_crs(r, p$projection)
  # r
  stars::st_as_stars(build_SpatRaster(p, v))
}