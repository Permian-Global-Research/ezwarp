
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
#'
#' @examples
sf_warp_util <- function(params,
                        destination,
                        resample,
                        options,
                        ...) {
  
  if (is.null(destination)){
    destination <- tempfile(fileext = '.tif')
  }
  
  opts <- c(
    "-te", params$extent[c(1, 3, 2, 4)],
    "-ts", params$dimension,
    "-t_srs", params$projection,
    "-r", resample,
    "-overwrite",
    options
  )
  
  sf::gdal_utils(
    util = "warp",
    source = params$x,
    destination = destination,
    options = opts,
    quiet = FALSE
  )
  destination
}


#' vapour_warp_util
#'
#' @param params 
#' @param bands 
#' @param resample 
#' @param opts 
#' @param ... 
#'
#' @return
#'
#' @examples
vapour_warp_util <- function(params, bands, resample, opts, ...){
  v <- vapour::vapour_warp_raster(
    x = vapour::vapour_vrt(params$x),
    bands= bands,
    extent = params$extent,
    dimension = params$dimension,
    projection = params$projection,
    resample = resample,
    band_output_type = 'numeric',
    silent=FALSE,
    options = opts,
    ...
  )
}



