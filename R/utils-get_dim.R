#' get projection of spatial object
#'
#' get projection of spatial object
#' @title get_proj: get projection of a spatial object
#' @param x ...
#' @export
get_dim <- function(x, ...) {
  UseMethod("get_dim")
}

#' @rdname get_dim
#' 
#' @export
get_dim.SpatRaster <- function(x,...){
  c(x@ptr$ncol(), x@ptr$nrow())
}

#' @rdname get_dim
#' 
#' @export
get_dim.stars <- function(x, ...){
  stars_dim(x)
}

#' @rdname get_dim
#' 
#' @export
get_dim.stars_proxy <- function(x, ...){
  stars_dim(x)
}

#' @rdname get_dim
#' 
#' @export
get_dim.chracter <- function(x, ...){
  tst <- try(vapour::vapour_raster_info(x))
  if (inherits(tst, "try-error")) {
    stop("Unable to read the dimension of the soure")
  }
  tst$dimXY
}


#' @rdname get_dim
#' 
#' @export
get_dim.ezgrid <- function(x, ...){
  x$dimension
}

stars_dim <- function(x){
  d <- attr(x, "dimension")
  c(d[[1]]$to -  d[[1]]$from + 1, d[[2]]$to -  d[[2]]$from  + 1)
}
