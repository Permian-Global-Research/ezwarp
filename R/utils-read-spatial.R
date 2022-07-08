#' read a spatial url/path
#'
#' @param x A character vector - source for a raster or spatial vector
#'
#' @return
#'
#' @examples
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
  } else {
    stop(sprintf("Can't return param %s from ezgrid", val))
  }
}


try_spat_info <- function(x) {
  x <- suppressWarnings(tryCatch({
    tst <- invisible(vapour::vapour_raster_info(x)) # invisible still prints message
    ezgrid(tst$extent, tst$dimXY, tst$projection)
  },
  error = function(e) {
    tryCatch({
      tst <- vapour::vapour_layer_info(x) 
      ezgrid(tst$extent, NULL, tst$projection$Wkt)
    },
    error = function(e) {
      message(e)
      stop("check your `y` argument - the source can't be read by gdal or ogr")
    })
  }))
  return(x)
}



