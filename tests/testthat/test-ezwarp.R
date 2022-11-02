# -------------------------EZWARP TESTING -------------------------------------
#================FILE IMPORTS==
f <- system.file("ex/elev.tif", package="terra") 
info<-vapour::vapour_raster_info(f)
r.stars <- stars::read_stars(f)
r.stars.prox <- stars::read_stars(f, proxy = TRUE)
r.terra <- terra::rast(f)

sub_terra <- function(r, v, return.vector=TRUE){
  r[r>v] <- NA
  if (return.vector){
    return(terra::as.polygons(r))
  }
  return(r)
}

vect.obj300 <-  sub_terra(r.terra, 300)
vect.obj200 <-  sub_terra(r.terra, 200)
.file <- tempfile(fileext = ".gpkg")
terra::writeVector(vect.obj200, .file)

rast.obj200 <-  sub_terra(r.terra, 200, FALSE)
ras.file <- tempfile(fileext = ".tif")
terra::writeRaster(rast.obj200, ras.file)


# ------------------------------ BEGIN TESTS ----------------------------------#
test_that("ezwarp-basic-vapour", {
  
  r1a <- ezwarp(f,f, res=1e-4)
  
  expect_gt(sum(r1a[], na.rm=TRUE), 0)
})

test_that("ezwarp-basic-vapour-ezgrid", {
  
  eg <- ezgrid(extent = info$extent,
               projection = info$projection)
  r1a <- ezwarp(f,eg, res=1e-4)
  
  expect_gt(sum(r1a[], na.rm=TRUE), 0)
})

test_that("ezwarp-basic-vapour-ezgrid2", {
  
  eg <- ezgrid(extent = info$extent,
               projection = info$projection,
               dimension = info$dimension)
  r1a <- ezwarp(f,eg)
  
  expect_gt(sum(r1a[], na.rm=TRUE), 0)
})

test_that("ezwarp-basic-sf", {
  
  r1b <- ezwarp(f,f, res=1e-4, engine = 'sf', out_class = 'stars',
                nodata = -999, filename = tempfile(fileext = ".tif"))
  
  expect_gt(sum(r1b[[1]], na.rm=TRUE), 0)
})

test_that("ezwarp-basic-vapour-stars-tofile", {
  
  r1b <- ezwarp(f,f, res=1e-4, out_class = 'stars',
                nodata = -999, filename = tempfile(fileext = ".tif"))
  
  expect_gt(sum(r1b[[1]], na.rm=TRUE), 0)
})

test_that("ezwarp-basic-vapour-stars", {
  
  r1b <- ezwarp(f,f, res=1e-4, out_class = 'stars')
  
  expect_gt(sum(r1b[[1]], na.rm=TRUE), 0)
})

test_that("ezwarp-basic-vapour-stars-prox", {
  
  r1b <- ezwarp(r.stars.prox,f, res=1e-4, out_class = 'stars')
  
  expect_gt(sum(r1b[[1]], na.rm=TRUE), 0)
})

test_that("ezwarp-basic-vapour-matrix", {
  
  r1c <- ezwarp(f,f, res=1e-4, out_class = "matrix")
  
  expect_gt(sum(r1c, na.rm=TRUE), 0)
})

test_that("ezwarp-basic-vapour-vector", {
  
  r1d <- ezwarp(f,f, res=1e-4, out_class = "vector")
  
  expect_gt(sum(r1d[[1]], na.rm=TRUE), 0)
})


test_that("ezwarp-basic2-vapour", {
  
  r2 <- ezwarp(r.stars, r.terra, cutline=vect.obj300)
  
  expect_lt(sum(r2[], na.rm = TRUE), sum(r.terra[], na.rm = TRUE))
})

test_that("ezwarp-basic2-sf", {
  
  r2 <- ezwarp(r.stars, r.terra, cutline=vect.obj300, engine = 'sf',
               crop_to_cutline = TRUE)
  
  expect_lt(sum(r2[], na.rm = TRUE), sum(r.terra[], na.rm = TRUE))
})

test_that("ezwarp-basic3-vapour", {
  

  r2 <- ezwarp(r.terra, r.stars, cutline=sf::st_as_sf(vect.obj200),
               crop_to_cutline = TRUE)
  
  expect_lt(sum(r2[], na.rm = TRUE), sum(r.terra[], na.rm = TRUE))
})

test_that("ezwarp-basic3-sf", {
  
  r2 <- ezwarp(r.terra, r.stars, cutline=vect.obj200, engine='sf')
  
  expect_lt(sum(r2[], na.rm = TRUE), sum(r.terra[], na.rm = TRUE))
})


test_that("ezwarp-multisoure-test", {
  
  r.terra3 <- c(r.terra, sqrt(r.terra))
  
  r2 <- ezwarp(r.terra3, r.terra)
  d <- dim(r.terra3)[3]
  
  expect_equal(d, 2)
})



test_that("ezwarp-vect-set", {
  
  r2 <- ezwarp(r.terra, vect.obj200, res=terra::res(r.terra)[1])
  
  expect_lt(sum(r2[], na.rm = TRUE), sum(r.terra[], na.rm = TRUE))
})


test_that("ezwarp-vectfile-set", {
  

  
  r2 <- ezwarp(r.terra, .file, res=terra::res(r.terra)[1])
  
  expect_lt(sum(r2[], na.rm = TRUE), sum(r.terra[], na.rm = TRUE))
})



test_that("ezwarp-list-vapour-mix", {
  
  .l <- list(f, rast.obj200)
  
  r1 <- ezwarp(rast.obj200, .file, res=0.009)
  r2 <- ezwarp(.l, .file, res=0.009)
  
  expect_lt(sum(r1[], na.rm = TRUE), sum(r2[], na.rm = TRUE))
})

test_that("ezwarp-list-files", {
  
  .l <- list(f, ras.file)
  
  r1 <- ezwarp(rast.obj200, .file, res=0.009)
  r2 <- ezwarp(.l, .file, res=0.009, engine="sf")
  
  expect_lt(sum(r1[], na.rm = TRUE), sum(r2[], na.rm = TRUE))
})

test_that("ezwarp-vector-error", {
  
  .l <- c(f, ras.file)
  
  expect_error(ezwarp(.l, .file, res=0.009))
})

test_that("ezwarp-ENGINE-error", {
  
  expect_error(ezwarp(f, .file, res=0.009, engine="loopy"))
})

test_that("ezwarp-OUTCLASS-warn", {
  
  expect_warning(ezwarp(f, .file, res=0.009, out_class = "SLIMJIM"))
})



test_that("ezwarp-band-conflict-error", {
  
  r.terra3 <- c(r.terra, sqrt(r.terra))
  .l <- list(r.terra3, rast.obj200)
  expect_error(ezwarp(.l, r.terra))

})

test_that("ezwarp-band-conflict-pass", {
  r.big <- (r.terra)*1000
  r.terra3 <- c(r.terra, r.big)
  r.terra3b <- c(r.terra, sqrt(r.terra), sqrt(r.terra)*10)
  .l <- list( r.terra3b, r.terra3)
  
  x <- ezwarp(.l, r.terra, bands = 2)
  expect_lt(sum(x[], na.rm=TRUE),sum(r.terra3[], na.rm=TRUE))
  
})



test_that("ezwarp-basic3-vapour", {
  
  r1 <- ezwarp(f, f, cutline=.file,
               crop_to_cutline = TRUE)
  
  r2 <- ezwarp(r.terra, r.stars, cutline=.file,
               crop_to_cutline = TRUE,
               options=c("-csql", 
                         paste0( "SELECT * FROM ", terra::vector_layers(.file), 
                                 " WHERE elevation >= 170")))
  
  expect_lt(sum(r2[], na.rm = TRUE), sum(r1[], na.rm = TRUE))
})


