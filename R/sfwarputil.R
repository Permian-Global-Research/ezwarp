
#' Easy Warp using sf::gdalutils
#' 
#' stable and supports >memory rasters and cutline feature.
#'
#' @param x a raster source or SpatRaster
#' @param y a raster source, SpatRaster, sf, or sfc
#' @param res numeric. the resolution of the output SpatRaster.
#' @param bands integer. which bands to use from the source
#' @param resample resampling method
#' @param destination out put destination. if NUll then a tempfile is created
#' @param cutline an sf or ogr-readable spatial vector source to mask the output raster.
#' @param ... Not used.
#'
#' @return
#' @export
#'
#' @examples
sfwarp_util <- function(x,
                      y,
                      res,
                      bands = 1L,
                      resample = 'bilinear',
                      cutline = NULL,
                      destination = NULL,
                      dstnodata = -999,
                      ...) {
  
  params <- list_inputs(x, y, res) |>
    lapply(as.character)
  
  if (is.null(destination)) {
    destination <- tempfile(fileext = '.tif')
  }
  
  opts <- c(
    "-te",
    params$extent[c(1, 3, 2, 4)],
    "-ts",
    params$dimension,
    "-t_srs",
    params$projection,
    "-dstnodata",
    dstnodata,
    "-r",
    resample,
    "-overwrite"
  )
  
  if (!is.null(cutline)) {
    if (any(class(cutline) %in% c("sf", "sfc"))) {
      tf <- tempfile(fileext = '.gpkg')
      sf::write_sf(cutline, tf)
      cutline <- tf
    }
    opts <- c(c('-cutline', cutline),
              opts)
  }
  
  sf::gdal_utils(
    util = "warp",
    source = params$x,
    destination = destination,
    options = opts,
    quiet = FALSE
  )
  return(terra::rast(destination))
}

