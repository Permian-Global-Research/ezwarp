#' check input source format/length
#' 
#' ensures that a list is provided
#'
#' @param x 
#'
#' @return list
check_in_form <- function(x){
  if (length(x)>1){
    if (class(x)!="list"){
      stop("When more than one source is provided for `x`, it must be as a list.
  For example: list(src1, src2, src3)")
    }
  } else {
    x <- list(x)
  }
  x
}


#' check input template source format/length
#' 
#' ensures template is of length 1
#'
#' @param x 
#'
#' @return list
check_grid_form <- function(x) {
  if (length(list(x)) > 1) {
    stop("Argument y must have a length of 1.")
  }
  x
}


#' check that res is provided if template is a spatial vector
#' 
#' ensures res is provided where spatial vector provided
#'
#' @param x 
#'
#' @return list
check_res_form <- function(x, .res){
  if (missing(.res) & inherits(x, c("sf", "sfc", "SpatVector"))){
    no_res_vec_err()
  } else if (missing(.res) & inherits(x, "ezgrid")) {
    if (is.null(x$dimension)){
      no_res_vec_err()
    }
  }
}

no_res_vec_err <- function(){
  stop("When `y` is a spatial vector source, `res` must be provided.")
}

