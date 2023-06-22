#' Easy Warp using sf::gdalutils
#'
#' stable and supports >memory rasters and cutline feature.
#' @param params A list containing the raster source(s), ext, dim and crs.
#' @param destination out put destination. if NUll then a tempfile is created
#' @param resample resampling method
#' @param compression the tif compression method to use e.g. "DEFLATE" or "LZW"
#' @param options character vector with gdal options.
#' @param ... Not used.

#'
#' @return a path to the output raster.
#' @noRd
sf_warp_util <- function(params,
                         destination,
                         resample,
                         compression,
                         options,
                         ...) {
  if (is.null(destination)) {
    destination <- tempfile(fileext = ".tif")
  }

  opts <- c(
    "-te", params$extent[c(1, 3, 2, 4)],
    "-ts", params$dimension,
    "-t_srs", params$projection,
    "-r", resample,
    "-overwrite",
    "-co", paste0("COMPRESS=", compression),
    options
  )

  sf::gdal_utils(
    util = "warp",
    source = params$x,
    destination = destination,
    options = opts,
    quiet = TRUE
  )
  destination
}


#' vapour_warp_util
#' @param params A list containing the raster source(s), ext, dim and crs.
#' @param resample resampling method
#' @param bands numeric veor of bands to include.
#' @param opts character vector with gdal options.
#' @param ... passed to `vapour::vapour_warp_raster`
#'
#' @return a vector of raster values.
#' @noRd
vapour_warp_util <- function(params, bands, resample, opts, ...) {
  v <- vapour::vapour_warp_raster(
    x = vapour::vapour_vrt(params$x),
    bands = bands,
    extent = params$extent,
    dimension = params$dimension,
    projection = params$projection,
    resample = resample,
    band_output_type = "numeric",
    silent = FALSE,
    options = opts,
    ...
  )
}
