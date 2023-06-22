#' Internal function which is used to rotate matrices and arrays.
#'
#' @param x Matrix
#'
#' @return Rotated matrix
#' @keywords internal
#' @noRd
rotate <- function(x) {
  t(x[nrow(x):1, ])
}

#' Internal function which is used to form a matrix from single vector band
#'
#' @param .v vector
#' @param .p ezgrid object
#'
#' @return Rotated matrix
#' @keywords internal
#' @noRd
matrix_thing <- function(.v, .p) {
  m <- matrix(.v, .p$dimension) # [,g$dimension[2]:1, drop = F]
  rotate(m)
}

#' Build a Matrix from vapour vector
#'
#' @description This function is used to build a matrix from a vapour vector that
#' conforms to the requirements of {rayshader}
#'
#' @param p ezgrid object
#' @param v list of numeric vectors from vapour
#'
#' @return matrix
#' @noRd
build_rayshader <- function(p, v) {
  if (length(v) > 1) {
    v2 <- lapply(v, matrix_thing, .p = p)
    a <- matrix(NA, p$dimension[2], p$dimension[1])
    m <- array(
      c(unlist(v2, use.names = FALSE), a),
      c(p$dimension[2], p$dimension[1], length(v2))
    )[, p$dimension[1]:1, , drop = FALSE]
  } else {
    m <- matrix(v[[1]], p$dimension[1])[, p$dimension[2]:1, drop = F]
    m <- rotate(rotate(m)) |>
      apply(2, rev)
  }

  attributes(m)$extent <- p$extent
  attributes(m)$dimension <- p$dimension
  attributes(m)$projection <- p$projection

  return(m)
}



#' Build a list of vectors from format
#'
#' @param p ezgrid object
#' @param v list of numeric vectors from vapour
#'
#' @return matrix
#' @noRd
build_vector <- function(p, v) {
  attributes(v)$extent <- p$extent
  attributes(v)$dimension <- p$dimension
  attributes(v)$projection <- p$projection

  return(v)
}
