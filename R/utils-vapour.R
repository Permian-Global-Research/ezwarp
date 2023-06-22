#' @title Write vector to raster source
#' @description Write a vector to an on-disk raster using vapour
#' @param v the vapour output numeric.
#' @param params the parameters used to build the output raster.
#' @param filename the filename of the output raster.
#' @param bands the bands to be read.
#' @param overwrite whether to overwrite the output raster.
#' @return a list with the filename and whether the write was successful.
#' @noRd
write_vapour_raster <- function(v, params, filename, bands, overwrite) {
    if (is.null(filename)) {
        filename <- tempfile(fileext = ".tif")
    }
    .quiet(vapour::vapour_create(
        filename = filename,
        extent = params$extent,
        dimension = params$dimension,
        projection = params$projection,
        n_bands = length(bands),
        overwrite = overwrite,
    ))

    write_ok <- vapour::vapour_write_raster_block(
        filename,
        data = v[[1]],
        offset = c(0L, 0L),
        dimension = params$dimension,
        band = bands,
        overwrite = overwrite
    )

    return(list(
        filename = filename,
        write_ok = write_ok
    ))
}
