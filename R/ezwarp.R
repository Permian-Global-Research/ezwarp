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
#' @param cutline an sf or ogr-readable spatial vector source to mask the output raster. see -cutline argument in gdalwarp
#' @param ... Additional args passed to `vapour::vapour_warp_raster`
#' @param crop_to_cutline 
#' @param nodata 
#' @param out_class 
#' @param filename 
#' @param overwrite 
#' @param options 
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
                   crop_to_cutline = TRUE,
                   nodata = NULL,
                   out_class = c('SpatRaster', 'stars'),
                   filename=NULL,
                   overwrite=TRUE,
                   options = "",
                   engine=c("vapour", "sf"),
                   ...) {
  
  
  x <- check_in_form(x)
  y <- check_grid_form(y)
  check_res_form(y, res)
  
  params <- build_warp_inputs(x, y, res)
  
  if (is.null(bands)){
    bands <- c(1:vapour::vapour_raster_info(params$x[1])$bands) ### FIX THIS
  }
  
  # save sf to file if cutine is TRUE
  
  opts <- ""
  if (!is.null(cutline)){
    cl <- get_source(cutline)
    opts <- c('-cutline', cl)
    if (isTRUE(crop_to_cutline)){
      opts <- c(opts, '-crop_to_cutline')
    }
  }
   
  if (!is.null(nodata)){
    opts <- c(opts,
              '-dstnodata',
              nodata)
  }   
  opts <- c(opts, options)
  
  
  if (engine[1]=='vapour'){
    
    v <- vapour_warp_util(params, bands, resample, opts,...)
    
    if (is.null(filename)) {
      if (out_class[1] == 'SpatRaster') {
        return(build_SpatRaster(params, v))
      } else if (out_class[1] == 'stars') {
        return(build_stars(params, v))
      } else {
        warning(
          sprintf(
            "The requested `out_class` value '%s' not supported. Returning SpatRaster...",
            out_class
          )
        )
        return(build_SpatRaster(params, v))
      }
    } else {
      vapour_create(
        filename = filename,
        extent = params$extent,
        dimension = params$dimension,
        projection = params$projection,
        n_bands = length(bands),
        overwrite = overwrite,
      )
      
      vapour_write_raster_block(
        filename,
        data = v[[1]],
        offset = c(0L, 0L),
        dimension = params$dimension,
        band = bands,
        overwrite = overwrite
      )}

  } else if (engine[1]=='sf'){
    
    filename <- sf_warp_util(params,
                        filename,
                        resample,
                        opts,
                        ...)
    
  } else {
    stop("Engine not supported - choose from 'vapour' or 'sf'.")
  }
  
    if (out_class[1]=='SpatRaster'){
      return(terra::rast(filename))
    } else if (out_class[1]=='stars'){
      return(stars::read_stars(filename))
    } else {
      warning(sprintf("The requested `out_class` value '%s' not supported. Returning SpatRaster...", out_class))
      return(terra::rast(filename))
    }
    
  }

  
  
  

  

