# -------------------------EZWARP TESTING -------------------------------------
# ================FILE IMPORTS==
f <- system.file("ex/elev.tif", package = "terra")
info <- vapour::vapour_raster_info(f)
r_stars <- stars::read_stars(f)
r_stars_prox <- stars::read_stars(f, proxy = TRUE)
r_terra <- terra::rast(f)

sub_terra <- function(r, v, return_vector = TRUE) {
  r[r > v] <- NA
  if (return_vector) {
    return(terra::as.polygons(r))
  }
  return(r)
}

vect_obj300 <- sub_terra(r_terra, 300)
vect_obj200 <- sub_terra(r_terra, 200)
.file <- tempfile(fileext = ".gpkg")
terra::writeVector(vect_obj200, .file, options = NULL)

rast_obj200 <- sub_terra(r_terra, 200, FALSE)
ras_file <- tempfile(fileext = ".tif")
terra::writeRaster(rast_obj200, ras_file)


# ------------------------------ BEGIN TESTS ----------------------------------#
test_that("ezwarp-basic-vapour", {
  r1a <- ezwarp(f, f, res = 1e-4)

  expect_gt(sum(r1a[], na.rm = TRUE), 0)
})


test_that("ezwarp-basic-vapour-ezgrid", {
  eg <- ezgrid(
    extent = info$extent,
    projection = info$projection
  )
  r1a <- ezwarp(f, eg, res = 1e-4)

  expect_gt(sum(r1a[], na.rm = TRUE), 0)
})

test_that("ezwarp-basic-vapour-ezgrid-path", {
  eg <- ezgrid(
    extent = info$extent,
    projection = info$projection
  )
  r1a <- ezwarp(f, eg, res = 1e-4, out_class = "path")

  expect_gt(sum(terra::rast(r1a)[], na.rm = TRUE), 0)
})

test_that("ezwarp-basic-vapour-ezgrid2", {
  eg <- ezgrid(
    extent = info$extent,
    projection = info$projection,
    dimension = info$dimension
  )
  r1a <- ezwarp(f, eg)

  expect_gt(sum(r1a[], na.rm = TRUE), 0)
})

test_that("ezwarp-basic-sf", {
  r1b <- ezwarp(f, f,
    res = 1e-4, engine = "sf", out_class = "stars",
    nodata = -999, filename = tempfile(fileext = ".tif")
  )

  expect_gt(sum(r1b[[1]], na.rm = TRUE), 0)
})

test_that("ezwarp-basic-sf-path", {
  r1b <- ezwarp(f, f,
    res = 1e-4, engine = "sf", out_class = "path",
    nodata = -999, filename = tempfile(fileext = ".tif")
  )

  expect_gt(sum(terra::rast(r1b)[], na.rm = TRUE), 0)
})


test_that("ezwarp-basic-vapour-stars-tofile", {
  r1b <- ezwarp(f, f,
    res = 1e-4, out_class = "stars",
    nodata = -999, filename = tempfile(fileext = ".tif")
  )

  expect_gt(sum(r1b[[1]], na.rm = TRUE), 0)
})

test_that("ezwarp-basic-vapour-stars", {
  r1b <- ezwarp(f, f, res = 1e-4, out_class = "stars")

  expect_gt(sum(r1b[[1]], na.rm = TRUE), 0)
})

test_that("ezwarp-basic-vapour-stars-prox", {
  r1b <- ezwarp(r_stars_prox, f, res = 1e-4, out_class = "stars")

  expect_gt(sum(r1b[[1]], na.rm = TRUE), 0)
})

test_that("ezwarp-basic-vapour-matrix", {
  r1c <- ezwarp(f, f, res = 1e-4, out_class = "rayshader")

  expect_gt(sum(r1c, na.rm = TRUE), 0)
})

test_that("ezwarp-basic-vapour-vector", {
  r1d <- ezwarp(f, f, res = 1e-4, out_class = "vector")

  expect_gt(sum(r1d[[1]], na.rm = TRUE), 0)
})


test_that("ezwarp-basic2-vapour", {
  r2 <- ezwarp(r_stars, r_terra, cutline = vect_obj300)

  expect_lt(sum(r2[], na.rm = TRUE), sum(r_terra[], na.rm = TRUE))
})

test_that("ezwarp-basic2-sf", {
  r2 <- ezwarp(r_stars, r_terra,
    cutline = vect_obj300, engine = "sf",
    crop_to_cutline = TRUE
  )

  expect_lt(sum(r2[], na.rm = TRUE), sum(r_terra[], na.rm = TRUE))
})

test_that("ezwarp-basic3-vapour", {
  r2 <- ezwarp(r_terra, r_stars,
    cutline = sf::st_as_sf(vect_obj200),
    crop_to_cutline = TRUE
  )

  expect_lt(sum(r2[], na.rm = TRUE), sum(r_terra[], na.rm = TRUE))
})

test_that("ezwarp-basic3-sf", {
  r2 <- ezwarp(r_terra, r_stars, cutline = vect_obj200, engine = "sf")

  expect_lt(sum(r2[], na.rm = TRUE), sum(r_terra[], na.rm = TRUE))
})


test_that("ezwarp-multisoure-test", {
  r.terra3 <- c(r_terra, sqrt(r_terra))

  r2 <- ezwarp(r.terra3, r_terra)
  d <- dim(r.terra3)[3]

  expect_equal(d, 2)
})



test_that("ezwarp-vect-set", {
  r2 <- ezwarp(r_terra, vect_obj200, res = terra::res(r_terra)[1])

  expect_lt(sum(r2[], na.rm = TRUE), sum(r_terra[], na.rm = TRUE))
})


test_that("ezwarp-vectfile-set", {
  r2 <- ezwarp(r_terra, .file, res = terra::res(r_terra)[1])

  expect_lt(sum(r2[], na.rm = TRUE), sum(r_terra[], na.rm = TRUE))
})



test_that("ezwarp-list-vapour-mix", {
  .l <- list(f, rast_obj200)

  r1 <- ezwarp(rast_obj200, .file, res = 0.009)
  r2 <- ezwarp(.l, .file, res = 0.009)

  expect_lt(sum(r1[], na.rm = TRUE), sum(r2[], na.rm = TRUE))
})

test_that("ezwarp-list-files", {
  .l <- list(f, ras_file)

  r1 <- ezwarp(rast_obj200, .file, res = 0.009)
  r2 <- ezwarp(.l, .file, res = 0.009, engine = "sf")

  expect_lt(sum(r1[], na.rm = TRUE), sum(r2[], na.rm = TRUE))
})

test_that("ezwarp-vector-error", {
  .l <- c(f, ras_file)

  expect_error(ezwarp(.l, .file, res = 0.009))
})

test_that("ezwarp-ENGINE-error", {
  expect_error(ezwarp(f, .file, res = 0.009, engine = "loopy"))
})

test_that("ezwarp-OUTCLASS-warn", {
  expect_warning(ezwarp(f, .file, res = 0.009, out_class = "SLIMJIM"))
})



test_that("ezwarp-band-conflict-error", {
  r.terra3 <- c(r_terra, sqrt(r_terra))
  .l <- list(r.terra3, rast_obj200)
  expect_error(ezwarp(.l, r_terra))
})

test_that("ezwarp-band-conflict-pass", {
  r.big <- (r_terra) * 1000
  r.terra3 <- c(r_terra, r.big)
  r.terra3b <- c(r_terra, sqrt(r_terra), sqrt(r_terra) * 10)
  .l <- list(r.terra3b, r.terra3)

  x <- ezwarp(.l, r_terra, bands = 2)
  expect_lt(sum(x[], na.rm = TRUE), sum(r.terra3[], na.rm = TRUE))
})



test_that("ezwarp-basic3-vapour", {
  r1 <- ezwarp(f, f,
    cutline = .file,
    crop_to_cutline = TRUE
  )

  r2 <- ezwarp(r_terra, r_stars,
    cutline = .file,
    crop_to_cutline = TRUE,
    options = c(
      "-csql",
      paste0(
        "SELECT * FROM ", terra::vector_layers(.file),
        " WHERE elevation >= 170"
      )
    )
  )

  expect_lt(sum(r2[], na.rm = TRUE), sum(r1[], na.rm = TRUE))
})
