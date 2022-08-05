#' Internal function which is used to rotate matrices and arrays.
#'
#' @param x Matrix
#'
#' @return Rotated matrix
#' @keywords internal
#'
rotate <- function(x){
  t(x[nrow(x):1,])
}

#' Internal function which is used to form a matrix from single vector band
#'
#' @param .v vector
#' @param .p ezgrid object
#'
#' @return Rotated matrix
#' @keywords internal
#'
matrix_thing <- function(.v, .p){
  m <- matrix(.v, .p[1])#[,g$dimension[2]:1, drop = F]
  rotate(m)
}

#' Build a SpatRaster from vapour vector
#'
#' @param p ezgrid object
#' @param v list of numeric vectors from vapour
#'
#' @return matrix
build_matrix <- function(p, v){
  m <- matrix(v[[1]], p$dimension[1])[,p$dimension[2]:1, drop = F]
  if (length(v) > 1) {
    v2 <- lapply(v, matrix_thing, p)
    a <- matrix(NA, p$dimension[2],p$dimension[1])
    m <- array(c(unlist(v2, use.names = FALSE), a), c(p$dimension[2], p$dimension[1], 4))[,p$dimension[1]:1,  drop = FALSE]
    m <- m%>%
      scales::rescale(.,to=c(0,1))
    
    m[,,4] <- alpha
  } else {
    m <- rotate(rotate(m)) %>%
      apply(2,rev)
  }
  
  attributes(m)$extent <- p$extent
  attributes(m)$dimension <- p$dimension
  attributes(m)$projection <- p$projection
  
  return(m)
}