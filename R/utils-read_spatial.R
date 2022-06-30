#' read a spatial url/path
#'
#' @param x A character vector - source for a raster or spatial vector
#'
#' @return
#'
#' @examples
read_spat_chr <- function(x){
    suppressWarnings(tryCatch({
      x <- terra::rast(x)
    },
    error = function(c) {
      tryCatch({
       x <- terra::vect(x) %>% 
         terra::makeValid()
      },
      error = function(c) {
        message(e)
        stop("check your `y` argument - the source can't be read by {terra}")
      })
    }))
  x
}




