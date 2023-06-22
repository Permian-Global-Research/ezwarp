#' Round to nearest value
#'
#' A literal copy of `plyr::round_any()`
#'
#' @param x numeric
#' @param accuracy numeric. Target multiple to round to.
#' @param f function. Default is `round`
#'
#' @return numeric
#' @noRd
round_nearest <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' round a bbox to desired interval
#'
#' @param .box numeric. Bounding box.
#' @param .res  numeric. Desired resolution.
#'
#' @return vector with bbox dims
#' @noRd
round_bbox <- function(.box, .res) {
  big <- round_nearest(.box[c(2, 4)], .res, f = ceiling)
  small <- round_nearest(.box[c(1, 3)], .res, f = floor)
  c(small[1], big[1], small[2], big[2])
}

#' get the target x y dimensions from bbox and desired res.
#'
#' @param .box numeric. Bounding box.
#' @param .res numeric. Desired resolution.
#'
#' @return numeric vector xy dims
#' @noRd
dims_from_box <- function(.box, .res) {
  x <- .box[2] - .box[1]
  y <- .box[4] - .box[3]
  c(x, y) / .res
}

#' list raster sources
#'
#' @param x A raster type object, e.g. SpatRaster, stars or file path
#' @param y A spatial object for the template, e.g. SpatRaster, stars, sf
#' or file path
#' @param res numeric. Desired resolution.
#'
#' @return list with extent, dimension, projection
#' @noRd
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


build_sources <- function(x) {
  x <- lapply(x, get_source) |>
    unlist()
}

build_template <- function(y, res) {
  if (inherits(y, "character")) {
    y <- read_spat_info(y)
  } else {
    y <- ezgrid(
      extent = get_ext(y),
      projection = get_proj(y),
      dimension = get_dim(y)
    )
  }

  if (!missing(res)) {
    y$extent <- round_bbox(y$extent, res)
    y$dimension <- dims_from_box(y$extent, res)
  } else {
    if (is.null(y$dimension)) {
      no_res_vec_error()
    }
  }
  return(y)
}

#' Adjust the extent and dimension of warp params
#' Used when crop to cutline is used to adjust the extent and dimension of the
#' target grid
#' @param cl cutline
#' @param params warp params
#' @return warp params
#' @noRd
crop_cutline_adjust <- function(cl, params, res, options) {
  # internal cutline control
  check_terra()
  com <- ""
  if ("-csql" %in% options) {
    id <- match("-csql", options)
    com <- options[id + 1]
  }
  info <- vapour::vapour_layer_info(cl, sql = com)

  bound <- matrix(info$extent[c(
    1, 2, 2, 1, 1,
    3, 3, 4, 4, 3
  )], ncol = 2)

  m <- terra::project(
    bound,
    from = info$projection$Wkt,
    to = params$projection
  )

  # catch for when a raster is used as a template and res isn't specified.
  if (missing(res)) {
    res <- (params$extent[2] - params$extent[1]) / params$dimension[1]
  }

  target_extent <- as.vector(apply(m, 2, range)) |>
    round_bbox(res)
  target_dims <- dims_from_box(target_extent, res)

  params$extent <- target_extent
  params$dimension <- target_dims

  return(params)
}

#' Process options for warp
#' @param cutline cutline
#' @param crop_to_cutline crop to cutline option
#' @param nodata nodata value
#' @param options options arg
#' @param params warp params
#' @param res resolution
#' @return list with options and params
#' @noRd
process_options <- function(
    cutline, crop_to_cutline,
    nodata, options, params, res) {
  opts <- ""
  if (!is.null(cutline)) {
    cl <- get_source(cutline)

    if (isTRUE(crop_to_cutline)) {
      params <- crop_cutline_adjust(cl, params, res, options)
    }

    opts <- c("-cutline", cl)
  }

  if (!is.null(nodata)) {
    opts <- c(
      opts,
      "-dstnodata",
      nodata
    )
  }
  opts <- c(opts, options)

  return(list(
    opts = opts,
    params = params
  ))
}


process_bands <- function(x, bands, params) {
  if (is.null(bands)) {
    b_list <- lapply(x, bands_r_ras, params = params)

    check_n_bands(b_list)

    bands <- 1:b_list[[1]]

    if ((length(bands) > 1 && length(params$x) > 1)[1]) {
      params$x <- sapply(x, save_r_ras)
    }
  }

  return(list(
    bands = bands,
    params = params
  ))
}
