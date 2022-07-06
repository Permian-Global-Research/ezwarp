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
get_dim.SpatVector <- function(x,...){
  NULL
}

#' @rdname get_dim
#' 
#' @export
get_dim.sf <- function(x, ...){
  NULL
}

#' @rdname get_dim
#' 
#' @export
get_dim.sfc <- function(x, ...){
  NULL
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
  read_spat_info(x, val='dimension')
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
