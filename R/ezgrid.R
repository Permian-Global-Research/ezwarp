#' Create an ezgrid object
#' 
#' Create an ezgrid object to define a desired extent, projection and crs. Use this
#' as the `y` argument in `ezwarp()`.
#'
#' @param extent numeric vector with the following form: `c(xmin, xmax, ymin, ymax)`
#' @param dimension numeric vector, length 2. define the XY dimension of the grid: `c(x.dim, y.dim)`
#' @param projection projection of warped raster (in Well-Known-Text, or any projection string accepted by GDAL)
#'
#' @return an ezgrid object
#' @export
#'
#' @examples
#' ezgrid(extent=c(-180, 180, -90, 90), 
#'   dimension = c(180, 90), 
#'   projection = 'EPSG:4326')
#' 
ezgrid <- function(extent, dimension, projection){
  
  x <- is_grid_valid(extent, dimension, projection)
  
  ezg <- structure(list(
    extent=x$extent,
    dimension=x$dimension,
    projection=x$projection
  ), class = "ezgrid")
  ezg
}

# check extent, dimension and proj validity... 
# check extent and projection are valid. from {gdlaio}
# taken from: https://github.com/hypertidy/gdalio/blob/main/R/default_grid.R
is_grid_valid <- function(extent, dimension, projection){
  x <- list(extent=extent, dimension=dimension, projection=projection)
  
  has_extent <- is.numeric(x[["extent"]]) && length(x[["extent"]] == 4) && all(!is.na(x[["extent"]])) &&
    diff(x[["extent"]][1:2]) > 0 && diff(x[["extent"]][3:4]) > 0
  if (!has_extent) stop("invalid extent")
  if ("dimXY" %in% names(x)) {
    x[["dimension"]] <- x[["dimXY"]]
  }
  
  if (!is.null(dimension)){
    has_dim <- is.numeric(x[["dimension"]]) && length(x[["dimension"]]) ==2  && all(!is.na(x[["dimension"]])) && all(x[["dim"]] > 0)
    if (!has_dim) stop("invalid dimension")
  }

  
  has_proj <- is.character(x[["projection"]]) && length(x[["projection"]]) == 1 && nchar(x[["projection"]]) > 0 && !is.na(x[["projection"]])
  if (!has_proj) stop("invalid projection")
  x[c("extent", "dimension", "projection")]
}
