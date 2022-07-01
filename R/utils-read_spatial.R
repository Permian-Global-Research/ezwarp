#' read a spatial url/path
#'
#' @param x A character vector - source for a raster or spatial vector
#'
#' @return
#'
#' @examples
read_spat_info <- function(x){
    suppressWarnings(tryCatch({
      tst <- vapour_raster_info(x) 
      x <- ezgrid(tst$extent, tst$dimXY, tst$projection)
    },
    error = function(e) {
      tryCatch({
        tst <- vapour_layer_info(x) 
        x <- ezgrid(tst$extent, NULL, tst$projection$Wkt)
      },
      error = function(e) {
        message(e)
        stop("check your `y` argument - the source can't be read by gdal or ogr")
      })
    }))
  x
}




