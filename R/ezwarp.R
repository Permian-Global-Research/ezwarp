#' An easy way to use gdal warp in R
#' 
#' super handy warper but limited in that returns in memory SpatRast. May cause
#' issues when creating a raster > memory. Also not clear how to implement 
#' the cutline feature.
#'
#' @param x a list or vector of raster source(s) or SpatRaster(s)
#' @param y a raster source, SpatRaster, sf, or sfc
#' @param res numeric. the resolution of the output SpatRaster.
#' @param bands numeric which bands to use from the source. Only used if `engine="vapour"`
#' @param resample resampling method. default is bilinear, see details.
#' @param cutline an sf, sfc, SpatVector or ogr-readable spatial vector source to mask the output raster. see -cutline argument in gdalwarp
#' @param crop_to_cutline logical. If TRUE, then the output will be cropped to the limits of the mask given in cutline.
#' @param nodata Numeric. No data value to be used for output.
#' @param out_class default is "SpatRaster". Can be either "SpatRaster", "stars", "matrix", "vector" 
#' @param filename the filepath for the out raster. if given and "vapour" is used 
#' for the engine, then the output SpatRaster/stars object will have a source. 
#' If NULL then an in memory raster is returned. If the sf engine is used and 
#' filename is NULL then a tempfile is used.
#' @param overwrite logical - should a file be overwritten.
#' @param compression default is "DEFLATE". character describing tif compression e.g. "LZW"
#' @param options gdal options. 
#' @param engine either "vapour" or "sf". choose which warper to use. Only vapour
#' supports in memory raster creation.
#' @param ... Additional args passed to `vapour::vapour_warp_raster`. Might be removed.
#' 
#' @details 
#' 
#' When selecting the resample method, choose from the following options:
#' 'bilinear' (the default), 'near', 'cubic', 'cubicspline', 'lanczos', 'average', 'mode',
#' 'max', 'min', 'med', 'q1', 'q3', 'sum'. For details on these methods see: https://gdal.org/programs/gdalwarp.html
#'  If raster source is categorical make sure to use 'nearest'.
#' 
#' If "matrix" is used for `out_class`, then a matrix is returned if only one band
#' is targeted. If multiple bands are targeted, then a n-dimensional array is returned.
#' The matrix is returned oriented North is up. 
#' 
#' If "vector" is used for `out_class`, a vector is returned for a single band target, 
#' and a list of vectors is returned for a multiband target.
#'
#' @return
#' @export
#'
#' @examples
#' f <- system.file("ex/elev.tif", package="terra") 
#' r.terra <- terra::rast(f)
#' r1a <- ezwarp(f,f, res=1e-4)
#' r1b <- ezwarp(r.terra,f, res=1e-4, engine = 'sf')

ezwarp <- function(x,
                   y,
                   res,
                   bands = NULL,
                   resample = 'bilinear',
                   cutline = NULL,
                   crop_to_cutline = FALSE,
                   nodata = NULL,
                   out_class = c('SpatRaster', 'stars','matrix', 'vector'),
                   filename=NULL,
                   overwrite=TRUE,
                   options = "",
                   compression = "DEFLATE",
                   engine=c("vapour", "sf"),
                   ...) {
  
  check_options(options)
  
  x <- check_in_form(x)
  y <- check_grid_form(y)
  check_res_form(y, res)
  check_logical(crop_to_cutline, "crop_to_cutline")
  check_engine(engine)
  
  params <- build_warp_inputs(x, y, res)
  
  # function to check R raster object bands
  bands_R_ras <- function(r){
    if (inherits(r, c("SpatRaster","stars_proxy"))){
      bands <- as.integer(dim(r)[3])
      if(is.na(bands)){ # to catch when stars proxy doesn't give the third dim.
        bands<-1
      }
    } else {
      bands <- max(c(1:vapour::vapour_raster_info(params$x[1])$bands)) 
    }
    return(bands)
  }
  
  # function to temp save R raster objects - used when multiple sources for 
  # single object i.e per band.
  save_R_ras <- function(r){
    if (inherits(r, c("SpatRaster"))){
      if (length(terra::sources(r))>1){
        return(get_source(r, force=TRUE))
      } else {
        return(sources(r))
      }
      
    } else if (inherits(r, c("stars_proxy"))){
      if (length(r[[1]])){
        return(get_source(r, force=TRUE))
      } else {
        return(r[[1]])
      }
      
    } else {
      return(r)
    }
  }
  
  if (is.null(bands)){
    b_list <- lapply(x, bands_R_ras)
    
    if (!length(unique(b_list))==1){
      stop("Input band numbers differ. You must provide a value for `bands` in this case.")
    }
    
    bands <- 1:b_list[[1]]
    
    if ((length(bands) > 1 & length(params$x)>1)[1]){
      params$x <- sapply(x, save_R_ras)
    }
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
      
      # catch for when a raster is used as a template and res isn't specified.
      if (missing(res)){
        res=(params$extent[2]-params$extent[1])/params$dimension[1]
      }
      
      target_extent <- as.vector(apply(m, 2, range)) |>  
        round_bbox(res)
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
      } else if(out_class[1] == 'matrix'){
        return(build_matrix(params, v))
      } else if(out_class[1] == 'vector'){
        return(build_vector(params, v))
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
      vapour::vapour_create(
        filename = filename,
        extent = params$extent,
        dimension = params$dimension,
        projection = params$projection,
        n_bands = length(bands),
        overwrite = overwrite,
      )
      
      vapour::vapour_write_raster_block(
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
                        compression,
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
  } else if(out_class[1] %in% c('matrix', 'vector')){
    #read from source
    v <- vapour_warp_util(params, bands, resample, opts,...)
    if (out_class[1] == "matrix") return(build_matrix(params, v))
    if (out_class[1] == "vector") return(build_vector(params, v))
  }  else {
    warning(
      sprintf(
        "The requested `out_class` value '%s' not supported. Returning SpatRaster...",
        out_class
      )
    )
    return(terra::rast(filename))
  }
    
  }

  
  
  

  

