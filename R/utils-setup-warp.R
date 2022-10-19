
#' Round to nearest value
#'
#' A literal copy of `plyr::round_any()`
#'
#' @param x numeric
#' @param accuracy numeric. Target multiple to round to.
#' @param f
#'
#' @return
round_nearest = function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' round a bbox to desired interval
#'
#' @param .box 
#' @param .res 
#'
#' @return vector with bbox dims
round_bbox <- function(.box, .res) {
  big <- round_nearest(.box[c(2, 4)], .res, f = ceiling)
  small <- round_nearest(.box[c(1, 3)], .res, f = floor)
  c(small[1], big[1], small[2], big[2])
}

#' get the target x y dimensions from bbox and desired res.
#'
#' @param .box 
#' @param .res 
#'
#' @return numeric vector xy dims
dims_from_box <- function(.box, .res) {
  x = .box[2] - .box[1]
  y = .box[4] - .box[3]
  c(x, y) / .res
}

#' list raster sources
#'
#' @param x 
#' @param y 
#' @param res 
#'
#' @return
build_warp_inputs <- function(x, y, res) {
  
  x <- build_sources(x)
  
  y <- build_template(y, res)
  
  return(list(
    x = x,
    extent = y$extent,
    dimension = y$dimension,
    projection = y$projection
  ))
}


build_sources <- function(x){
  x <- lapply(x, get_source) |>
    unlist()
}

build_template <- function(y, res){
  
  if (inherits(y, 'character')) {
    y <- read_spat_info(y)
    
  } else {
    y <- ezgrid(
      extent = get_ext(y),
      projection = get_proj(y),
      dimension = get_dim(y)
    )
  }
  
  if (!missing(res)) {
    y$extent = round_bbox(y$extent, res)
    y$dimension = dims_from_box(y$extent, res)
  } else {
    if (is.null(y$dimension))
      no_res_vec_err()
  }
  return(y)
}
