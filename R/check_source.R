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
check_source <- function(s){
  UseMethod("check_source")
}


#' @rdname check_source
#' 
#' @export
check_source.SpatRaster <- function(s) {
  s.file <- terra::sources(s)
  if (identical(s.file, "")) {
    s.file <- tempfile(fileext = '.tif')
    terra::writeRaster(s, s.file)
  }
s.file
}

#' @rdname check_source
#' 
#' @export
check_source.stars <- function(s) {
  s.file <- tempfile(fileext = '.tif')
  stars::write_stars(s, s.file)
  s.file
}

#' @rdname check_source
#' 
#' @export
check_source.stars_proxy <- function(s) {
  s[[1]]
} 

#' @rdname check_source
#' 
#' @export
check_source.character <- function(s){
  # if (is_url(s)){
  #   ##### fix this.
  # }
  s
} 

#' @rdname check_source
#' 
#' @export
check_source.sf <- function(s){
  sf_temp_save(s)
}

#' @rdname check_source
#' 
#' @export
check_source.sfc <- function(s){
  sf_temp_save(s)
}

#' @rdname check_source
#' 
#' @export
check_source.SpatVector <- function(s){
  s.file <- tempfile(fileext = '.fgb')
  writeVector(s, s.file, filetype='FlatGeobuf', options=NULL )
  s.file
}


#' save sf to temp source - return source
#'
#' @return
sf_temp_save <- function(s){
  s.file <- tempfile(fileext = '.fgb')
  sf::write_sf(s, s.file)
  s.file
}
