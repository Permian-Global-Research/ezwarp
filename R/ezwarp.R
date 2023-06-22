#' An easy way to use gdal warp in R
#'
#' super handy warper but limited in that returns in memory SpatRast. May cause
#' issues when creating a raster > memory. Also not clear how to implement
#' the cutline feature.
#'
#' @param x a list or vector of raster source(s) or SpatRaster(s)
#' @param y a raster source, SpatRaster, sf, or sfc
#' @param res numeric. the resolution of the output SpatRaster.
#' @param bands numeric which bands to use from the source. Only used if
#' `engine="vapour"`
#' @param resample resampling method. default is bilinear, see details.
#' @param cutline an sf, sfc, SpatVector or ogr-readable spatial vector source
#' to mask the output raster. see -cutline argument in gdalwarp
#' @param crop_to_cutline logical. If TRUE, then the output will be cropped to
#' the limits of the mask given in cutline.
#' @param nodata Numeric. No data value to be used for output.
#' @param out_class default is "SpatRaster". Can be either "SpatRaster",
#' "stars", "path", "rayshader", or "vector".
#' @param filename the filepath for the out raster. if given and "vapour" is
#' used for the engine, then the output SpatRaster/stars object will have a
#' source. If NULL then an in memory raster is returned. If the sf engine is
#' used and filename is NULL then a tempfile is used.
#' @param overwrite logical - should a file be overwritten.
#' @param compression default is "DEFLATE". character describing tif
#' compression e.g. "LZW"
#' @param options gdal options.
#' @param engine either "vapour" or "sf". choose which warper to use.
#' Only vapour supports in memory raster creation.
#' @param ... Additional args passed to `vapour::vapour_warp_raster`.
#' Might be removed.
#'
#' @family warp_functions
#'
#' @details
#'
#' When selecting the resample method, choose from the following options:
#' 'bilinear' (the default), 'near', 'cubic', 'cubicspline', 'lanczos',
#' 'average', 'mode', 'max', 'min', 'med', 'q1', 'q3', 'sum'. For details on
#' these methods see: https://gdal.org/programs/gdalwarp.html. If raster source
#' is categorical make sure to use 'nearest'.
#'
#' If "rayshader" is used for `out_class`, then a matrix is returned if only one
#' band is targeted. If multiple bands are targeted, then a n-dimensional array
#' is returned. The matrix is returned oriented North is up.
#'
#' If "vector" is used for `out_class`, a vector is returned for a single band
#' target, and a list of vectors is returned for a multiband target.
#'
#' If "path" is used for `out_class`, a path to the output raster is returned.
#'
#' @return one of the following as defined in `out_class`: 'SpatRaster',
#' 'stars','matrix', 'vector'
#'
#' @examples
#' f <- system.file("ex/elev.tif", package = "terra")
#' r.terra <- terra::rast(f)
#'
#' ezwarp(f, f, res = 1e-4)
#'
#' ezwarp(r.terra, f, res = 1e-4, engine = "sf", out_class = "stars")
#'
#' v <- ezwarp(r.terra, f, res = 1e-4, out_class = "vector")[[1]]
#' hist(v)
#' @export
ezwarp <- function(x,
                   y,
                   res,
                   bands = NULL,
                   resample = "bilinear",
                   cutline = NULL,
                   crop_to_cutline = FALSE,
                   nodata = NULL,
                   out_class = c("SpatRaster", "stars", "path", "rayshader", "vector"),
                   filename = NULL,
                   overwrite = TRUE,
                   options = "",
                   compression = "DEFLATE",
                   engine = c("vapour", "sf"),
                   ...) {
  check_options(options)

  x <- check_in_form(x)
  y <- check_grid_form(y)
  check_res_form(y, res)
  check_logical(crop_to_cutline, "crop_to_cutline")
  check_engine(engine)

  params <- build_warp_inputs(x, y, res)

  band_set <- process_bands(x, bands, params)
  bands <- band_set$bands
  params <- band_set$params

  # sort out the options.
  get_options <- process_options(
    cutline, crop_to_cutline,
    nodata, options, params, res
  )
  params <- get_options$params
  opts <- get_options$opts


  # send inputs to the engine.
  if (engine[1] == "vapour") {
    v <- vapour_warp_util(params, bands, resample, opts, ...)

    if (is.null(filename) && out_class[1] != "path") {
      return(in_memory_build_composer(v, params, out_class))
    } else {
      v_write <- write_vapour_raster(v, params, filename, bands, overwrite)
      check_vapour_write(v_write$write_ok)
      filename <- v_write$filename
    }
  } else if (engine[1] == "sf") {
    filename <- sf_warp_util(
      params,
      filename,
      resample,
      compression,
      opts,
      ...
    )
  } else {
    engine_error(engine[1])
  }
  # if in memory is not true or the sf engine is used then read the file.
  return(on_disk_build_composer(
    filename, out_class, params, v,
    bands, resample, opts, ...
  ))
}

#' @title in memory raster composer
#' @description builds the output raster in memory.
#' @param v the vapour output object.
#' @param params the parameters used to build the output raster.
#' @param out_class the output class.
#' @return the output raster.
#' @noRd
in_memory_build_composer <- function(v, params, out_class) {
  if (out_class[1] == "SpatRaster") {
    return(build_SpatRaster(params, v))
  } else if (out_class[1] == "stars") {
    return(build_stars(params, v))
  } else if (out_class[1] == "rayshader") {
    return(build_rayshader(params, v))
  } else if (out_class[1] == "vector") {
    return(build_vector(params, v))
  } else {
    out_class_warn(out_class[1])
    return(build_SpatRaster(params, v))
  }
}

#' @title on disk raster composer
#' @description builds the output raster on disk.
#' @param filename the filename of the output raster.
#' @param out_class the output class.
#' @param params the parameters used to build the output raster.
#' @param v the vapour output object.
#' @param bands the bands to be read.
#' @param resample the resampling method.
#' @param opts the options used to build the output raster.
#' @return the output raster.
#' @noRd
on_disk_build_composer <- function(
    filename, out_class, params, v,
    bands, resample, opts, ...) {
  if (out_class[1] == "SpatRaster") {
    return(terra::rast(filename))
  } else if (out_class[1] == "stars") {
    return(stars::read_stars(filename))
  } else if (out_class[1] %in% c("rayshader", "vector")) {
    # read from source
    v <- vapour_warp_util(params, bands, resample, opts, ...)
    if (out_class[1] == "rayshader") {
      return(build_rayshader(params, v))
    }
    if (out_class[1] == "vector") {
      return(build_vector(params, v))
    }
  } else if (out_class[1] == "path") {
    return(filename)
  } else {
    out_class_warn(out_class[1])
    return(terra::rast(filename))
  }
}
