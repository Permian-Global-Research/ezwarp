#' read a spatial url/path
#'
#' @param x A character vector - source for a raster or spatial vector
#'
#' @return list or vector with spatial attributes.
#' @noRd
read_spat_info <- function(x, val=NULL){
  
  x <- try_spat_info(x)
  
  if (is.null(val)){
    return(x)
  } else if (val=='extent'){
    return(x$extent)
  } else if (val=='dimension'){
    return(x$dimenson)
  } else if (val=='projection'){
    return(x$projection)
  } else if (val=='source'){
    return(x$source)
  } else {
    stop(sprintf("Can't return param %s from ezgrid", val))
  }
}


try_spat_info <- function(x) {
  x <- suppressWarnings(tryCatch({
    tst <- invisible(vapour::vapour_raster_info(x)) # invisible still prints message
    srcs <- tst$filelist
    if (length(srcs)==0) srcs <- x
    
    ezgrid(tst$extent, tst$dimXY, tst$projection, srcs)
  },
  error = function(e) {
    tryCatch({
      tst <- vapour::vapour_layer_info(x) 
      
      srcs <- tst$dsn
      if (length(srcs)==0) srcs <- x
      
      ezgrid(tst$extent, NULL, tst$projection$Wkt, tst$dsn)
    },
    error = function(e) {
      message(e)
      stop("check your `y` argument - the source can't be read by gdal or ogr")
    })
  }))
  return(x)
}



