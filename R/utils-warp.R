
#' Round to nearest value
#'
#' A literal copy of `plyr::round_any()`
#'
#' @param x numeric
#' @param accuracy
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
list_inputs <- function(x, y, res) {
  x <- lapply(x, check_source) |>
    unlist()
  
  if (class(y)[1]=='character') {
    y <- read_spat_chr(y)
  }
  
  extent = get_ext(y)
  
  if (!missing(res)) {
    extent = round_bbox(extent, res)
    dimension = dims_from_box(extent, res)
  } else {
    if (any(class(y) %in% c("sf", "sfc", "SpatVector"))) {
      
      i <- vapour::vapour_raster_info(x[1])
      
      dims <- i$dimXY
      src.ext <- i$extent
      x.res <- abs(src.ext[1]-src.ext[2])/dims[1]
      y.res <- abs(src.ext[2]-src.ext[3])/dims[2]
      
      dims_from_box()
      
      dimension <-vapour::vapour_raster_info(x[1])$dimXY
    } else {
      dimension <- dim(y)[2:1]
    }
    
  }
  projection = get_proj(y)
  
  return(list(
    x = x,
    extent = extent,
    dimension = dimension,
    projection = projection
  ))
}




