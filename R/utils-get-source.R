#' Get the source of a spatial object
#' 
#' A class agnostic function to return or create the source of a spatial object.
#'
#' @param x a raster source - either character i.e a local source or remote. 
#' If remote, src must be prefixed with relevant gdal module. e.g. "/vsicurl/"
#' @param force Logical to determine if the SpatRaster or stars_proxy object should
#' be re-saved even if on disk sources exist.
#' @family spatial helpers (class agnostic)
#' @rdname get_source
#' @return character source.
#' 
#' @details 
#' If a SpatRaster is provided the source of the raster is used. 
#' 
#' @examples
#' f <- system.file("ex/elev.tif", package="terra") 
#' get_source(f)
#' get_source(terra::rast(f), force=TRUE)
#' f2 <- system.file("ex/lux.shp", package="terra") 
#' get_source(f2)
#' 
#' @export
get_source <- function(x, force=FALSE){
  UseMethod("get_source")
}


#' @rdname get_source
#' 
#' @export
get_source.SpatRaster <- function(x, force=FALSE) {
  check_terra()
  
  t.ter <- function(r){
    tf <- tempfile(fileext = '.tif')
    terra::writeRaster(r, tf)
    tf
  }
  
  if (isFALSE(force)){
    s.file <- terra::sources(x)
    # s.file <- s@ptr$filenames
    if ("" %in% s.file) {
      s.file <- t.ter(x)
    }
  } else {
    s.file <- t.ter(x)
  }

s.file
}


t.star <- function(r){
  ts <- tempfile(fileext = '.tif')
  stars::write_stars(r, ts)
  ts
}

#' @rdname get_source
#' 
#' @export
get_source.stars <- function(x) {
  check_stars()
  t.star(x)
}

#' @rdname get_source
#' 
#' @export
get_source.stars_proxy <- function(x, force=FALSE) {
  if (isFALSE(force)){
    return(x[[1]])
  } else (
    return(t.star(x))
  )
} 

#' @rdname get_source
#' 
#' @export
get_source.character <- function(x){
  read_spat_info(x, val='source')
} 

#' @rdname get_source
#' 
#' @export
get_source.sf <- function(x){
  sf_temp_save(x)
}

#' @rdname get_source
#' 
#' @export
get_source.sfc <- function(x){
  sf_temp_save(x)
}

#' @rdname get_source
#' 
#' @export
get_source.SpatVector <- function(x){
  check_terra()
  s.file <- tempfile(fileext = '.fgb')
  terra::writeVector(x, s.file, filetype='FlatGeobuf', options=NULL )
  s.file
}


#' save sf to temp source - return source
#' @noRd
sf_temp_save <- function(x){
  check_sf()
  s.file <- tempfile(fileext = '.fgb')
  sf::write_sf(x, s.file)
  s.file
}
