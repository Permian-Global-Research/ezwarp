#' Build a SpatRaster from vapour vector
#'
#' @param p params from ezwarp
#' @param v a numeric vector with raster values
#'
#' @return a SpatRaster
build_SpatRaster <- function(p, v){
  r <- terra::rast(
    terra::ext(p$extent),
    nrows = p$dimension[2],
    ncols = p$dimension[1],
    crs = p$projection
  )
  
  if (length(v) > 1)
    terra::nlyr(r) <- length(v)
  
  terra::setValues(r, do.call(cbind, v))
}