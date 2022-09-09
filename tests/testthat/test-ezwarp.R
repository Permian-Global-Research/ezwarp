f <- system.file("ex/elev.tif", package="terra") 
r.stars <- stars::read_stars(f)
r.terra <- terra::rast(f)


test_that("ezwarp-basic-vapour", {
  
  r1a <- ezwarp(f,f, res=1e-4)
  
  expect_gt(sum(r1a[], na.rm=TRUE), 0)
})

test_that("ezwarp-basic-sf", {
  
  r1b <- ezwarp(f,f, res=1e-4, engine = 'sf')
  
  expect_gt(sum(r1b[], na.rm=TRUE), 0)
})

test_that("ezwarp-basic2-vapour", {
  
  r.terra2 <- r.terra
  r.terra2[r.terra2>300] <- NA
  
  vect.obj <- terra::as.polygons(r.terra2) 
  
  r2 <- ezwarp(r.stars, r.terra, cutline=vect.obj)
  
  expect_lt(sum(r2[], na.rm = TRUE), sum(r.terra[], na.rm = TRUE))
})

test_that("ezwarp-basic2-sf", {
  
  r.terra2 <- r.terra
  r.terra2[r.terra2>300] <- NA
  
  vect.obj <- terra::as.polygons(r.terra2) 
  
  r2 <- ezwarp(r.stars, r.terra, cutline=vect.obj, engine = 'sf')
  
  expect_lt(sum(r2[], na.rm = TRUE), sum(r.terra[], na.rm = TRUE))
})

test_that("ezwarp-basic3-vapour", {
  
  r.terra2 <- r.terra
  r.terra2[r.terra2>200] <- NA
  
  vect.obj <- terra::as.polygons(r.terra2) 
  
  r2 <- ezwarp(r.terra, r.stars, cutline=sf::st_as_sf(vect.obj))
  
  expect_lt(sum(r2[], na.rm = TRUE), sum(r.terra[], na.rm = TRUE))
})

test_that("ezwarp-basic3-sf", {
  
  r.terra2 <- r.terra
  r.terra2[r.terra2>200] <- NA
  
  vect.obj <- terra::as.polygons(r.terra2) 
  
  r2 <- ezwarp(r.terra, r.stars, cutline=sf::st_as_sf(vect.obj), engine='sf')
  
  expect_lt(sum(r2[], na.rm = TRUE), sum(r.terra[], na.rm = TRUE))
})


test_that("ezwarp-multisoure-test", {
  
  r.terra3 <- c(r.terra, sqrt(r.terra))
  
  r2 <- ezwarp(r.terra3, r.terra)
  d <- dim(r.terra3)[3]
  
  expect_equal(d, 2)
})
