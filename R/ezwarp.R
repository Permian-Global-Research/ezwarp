#' Easy Warper using vapour
#' 
#' super handy warper but limited in that returns in memory SpatRast. May cause
#' issues when creating a raster > memory. Also not clear how to implement 
#' the cutline feature.
#'
#' @param x a list or vector of raster source(s) or SpatRaster(s)
#' @param y a raster source, SpatRaster, sf, or sfc
#' @param res numeric. the resolution of the output SpatRaster.
#' @param bands numeric which bands to use from the source.
#' @param resample resampling method
#' @param mask an sf or ogr-readable spatial vector source to mask the output raster. see -cutline argument in gdalwarp
#' @param ... Additional args passed to `vapour::vapour_warp_raster`
#'
#' @return
#' @export
#'
#' @examples
ezwarp <- function(x,
                   y,
                   res,
                   bands = NULL,
                   resample = 'bilinear',
                   cutline = NULL,
                   nodata = NULL,
                   ...) {
  
  
  x <- check_in_form(x)
  y2 <- check_grid_form(y)
  
  
  params <- list_inputs(x, y2, res)
  
  check_res_form(y2, res)
  
  if (is.null(bands)){
    bands <- c(1:vapour::vapour_raster_info(params$x)$bands) ### FIX THIS
  }
  
  # save sf to file if cutine is TRUE
  
  opts <- ""
  if (!is.null(cutline)){
    cl <- check_source(y)
    opts <- c('-cutline', cl,
              '-crop_to_cutline')
  }
   
  if (!is.null(nodata)){
    opts <- c(opts,
              '-dstnodata',
              nodata)
  }   
  
  v <- vapour::vapour_warp_raster(
    x = params$x,
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
  
  r <- terra::rast(
    terra::ext(params$extent),
    nrows = params$dimension[2],
    ncols = params$dimension[1],
    crs = params$projection
  )
  
  if (length(v) > 1)
    terra::nlyr(r) <- length(v)
  
  terra::setValues(r, do.call(cbind, v))
}
