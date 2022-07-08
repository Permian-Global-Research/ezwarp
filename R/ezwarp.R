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
#' @param resample resampling method. If raster source is categorical use 'nearest'
#' @param cutline an sf or ogr-readable spatial vector source to mask the output raster. see -cutline argument in gdalwarp
#' @param crop_to_cutline logical. If TRUE, then the output will be cropped to the limits of the mask given in cutline.
#' @param nodata Numeric. No data value to be used for output.
#' @param out_class either "SpatRaster" or "stars"
#' @param filename the filepath for the out raster. if given and "vapour" is used 
#' for the engine, then the output SpatRaster/stars object will have a source. 
#' If NULL then an in memory raster is returned. If the sf engine is used and 
#' filename is NULL then a tempfile is used.
#' @param overwrite logical - should a file be overwritten.
#' @param options gdal options. 
#' @param engine either "vapour" or "sf". choose which warper to use. Only vapour
#' supports in memory raster creation.
#' @param ... Additional args passed to `vapour::vapour_warp_raster`. Might be removed.
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
                   crop_to_cutline = FALSE,
                   nodata = NULL,
                   out_class = c('SpatRaster', 'stars'),
                   filename=NULL,
                   overwrite=TRUE,
                   options = "",
                   engine=c("vapour", "sf"),
                   ...) {
  
  check_options(options)
  
  x <- check_in_form(x)
  y <- check_grid_form(y)
  check_res_form(y, res)
  
  params <- build_warp_inputs(x, y, res)
  
  if (is.null(bands)){
    bands <- c(1:vapour::vapour_raster_info(params$x[1])$bands) 
  }
  
  
  # sort out the options.
  opts <- ""
  if (!is.null(cutline)){
    cl <- get_source(cutline)
    
    if (isTRUE(crop_to_cutline)){
      # internal cutline control
      check_terra()
      com<-""
      if ("-csql" %in% options){
        id <- match("-csql", options)
        com <-  options[id+1]
      }
      info <- vapour::vapour_layer_info(cl, sql = com)
      bound <- matrix(info$extent[c(1, 2, 2, 1, 1, 
                                    3, 3, 4, 4, 3)], ncol = 2)
      m <- terra::project(bound, from = info$projection$Wkt, to=params$projection)
      
      target_extent <- as.vector(apply(m, 2, range)) %>% 
        round_bbox(., res)
      target_dims <- dims_from_box(target_extent, res)
      
      params$extent <- target_extent
      params$dimension <- target_dims
    }
    
    opts <- c('-cutline', cl)
  }
   
  if (!is.null(nodata)){
    opts <- c(opts,
              '-dstnodata',
              nodata)
  }
  opts <- c(opts, options)
  

  # send inputs to the engine.
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
  
  # if in memory is not true or the sf engine is used then read the file.
  if (out_class[1] == 'SpatRaster') {
    return(terra::rast(filename))
  } else if (out_class[1] == 'stars') {
    return(stars::read_stars(filename))
  } else {
    warning(
      sprintf(
        "The requested `out_class` value '%s' not supported. Returning SpatRaster...",
        out_class
      )
    )
    return(terra::rast(filename))
  }
    
  }

  
  
  

  

