#' read a spatial url/path
#'
#' @param x A character vector - source for a raster or spatial vector
#'
#' @return list or vector with spatial attributes.
#' @noRd
read_spat_info <- function(x, val = NULL) {
  x <- try_spat_info(x)

  if (is.null(val)) {
    return(x)
  } else if (val == "extent") {
    return(x$extent)
  } else if (val == "dimension") {
    return(x$dimenson)
  } else if (val == "projection") {
    return(x$projection)
  } else if (val == "source") {
    return(x$source)
  } else {
    missing_param_error(val)
  }
}


try_spat_info <- function(x) {
  x <- suppressWarnings(tryCatch(
    {
      tst <- .quiet(vapour::vapour_raster_info(x)) # still prints message
      srcs <- tst$filelist
      if (length(srcs) == 0) srcs <- x

      ezgrid(tst$extent, tst$dimXY, tst$projection, srcs)
    },
    error = function(e) {
      tryCatch(
        {
          tst <- vapour::vapour_layer_info(x)

          srcs <- tst$dsn
          if (length(srcs) == 0) srcs <- x

          ezgrid(tst$extent, NULL, tst$projection$Wkt, tst$dsn)
        },
        error = function(e) {
          cli::cli_alert_danger(e)
          spat_source_error(x)
        }
      )
    }
  ))
  return(x)
}


#' @title make source for in-memory raster
#' @description create source for in memory R raster type object
#' @param r R raster trype object, e.g. SpatRaster or stars_proxy
#' @return source for in memory R raster type object
#' @noRd
save_r_ras <- function(r) {
  if (inherits(r, c("SpatRaster"))) {
    if (length(terra::sources(r)) > 1) {
      return(get_source(r, force = TRUE))
    } else {
      return(terra::sources(r))
    }
  } else if (inherits(r, c("stars_proxy"))) {
    if (length(r[[1]])) {
      return(get_source(r, force = TRUE))
    } else {
      return(r[[1]])
    }
  } else {
    return(r)
  }
}


#' function to check R raster object bands
#' @param r R raster trype object, e.g. SpatRaster or stars_proxy
#' @return numeric defining bands to warp.
#' @noRd
bands_r_ras <- function(r, params) {
  if (inherits(r, c("SpatRaster", "stars_proxy"))) {
    bands <- as.integer(dim(r)[3])
    if (is.na(bands)) { # to catch when stars proxy doesn't give the 3rd dim.
      bands <- 1
    }
  } else {
    bands <- max(c(1:vapour::vapour_raster_info(params$x[1])$bands))
  }
  return(bands)
}
