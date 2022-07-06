#' Check the source raster classes
#' 
#' If a SpatRaster is provided the source of the raster is used. if it is in 
#' memory then and error is thrown. if character provided then assumed to be 
#' correct.
#'
#' @param s a raster source - either character i.e a local source or remote. 
#' If remote, src must be prefixed with relevant gdal module. e.g. "/vsicurl/"
#'
#' @return character source.
#' @export
get_source <- function(s){
  UseMethod("get_source")
}


#' @rdname get_source
#' 
#' @export
get_source.SpatRaster <- function(s) {
  check_terra()
  # s.file <- terra::sources(s)
  s.file <- s@ptr$filenames
  if (identical(s.file, "")) {
    s.file <- tempfile(fileext = '.tif')
    terra::writeRaster(s, s.file)
  }
s.file
}

#' @rdname get_source
#' 
#' @export
get_source.stars <- function(s) {
  check_stars()
  s.file <- tempfile(fileext = '.tif')
  stars::write_stars(s, s.file)
  s.file
}

#' @rdname get_source
#' 
#' @export
get_source.stars_proxy <- function(s) {
  s[[1]]
} 

#' @rdname get_source
#' 
#' @export
get_source.character <- function(s){
  # if (is_url(s)){
  #   ##### fix this.
  # }
  s
} 

#' @rdname get_source
#' 
#' @export
get_source.sf <- function(s){
  sf_temp_save(s)
}

#' @rdname get_source
#' 
#' @export
get_source.sfc <- function(s){
  sf_temp_save(s)
}

#' @rdname get_source
#' 
#' @export
get_source.SpatVector <- function(s){
  check_terra()
  s.file <- tempfile(fileext = '.fgb')
  terra::writeVector(s, s.file, filetype='FlatGeobuf', options=NULL )
  s.file
}


#' save sf to temp source - return source
#'
#' @return
sf_temp_save <- function(s){
  check_sf()
  s.file <- tempfile(fileext = '.fgb')
  sf::write_sf(s, s.file)
  s.file
}
