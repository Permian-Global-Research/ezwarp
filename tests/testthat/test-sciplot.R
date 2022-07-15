f <- system.file("ex/elev.tif", package="terra") 
r.stars <- stars::read_stars(f)
r.terra <- terra::rast(f)


test_that("sciplot-test", {
  scip <- sciplot_pals()
  expect_type(scip, "character")
})

test_that("sciplot-stars-basic", {

  expect_silent(sciplot(r.stars))
})

test_that("sciplot-stars-centre", {
  r.stars[[1]] <- scale(r.stars[[1]], center=TRUE, scale=FALSE)  
  
  expect_silent(sciplot(r.stars, centre = TRUE, pal="bukavu"))
})

test_that("sciplot-terra-basic", {

  
  expect_silent(sciplot(r.terra))
})

test_that("sciplot-terra-centre", {
  r.terra[]<- scale(r.terra[], center=TRUE, scale=FALSE)  
  
  expect_silent(sciplot(r.terra, centre = TRUE, pal="bukavu"))
})



