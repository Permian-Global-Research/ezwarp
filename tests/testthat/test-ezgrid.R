test_that("ezgrid-4326-pass", {
  
  expect_visible(ezgrid(extent=c(-180, 180, -90, 90), 
                        dimension = c(180, 90),
                        projection = 'EPSG:4326'))
})

test_that("ezgrid-4326-err-ext", {
  
  expect_error(ezgrid(extent=c(180, -180, 90, -90), 
                      dimension = c(180, 90),
                      projection = 'EPSG:4326'))
})

test_that("ezgrid-laea-basic", {
  
  expect_visible(ezgrid(extent=c(-1,1,-1,1)*1.4e7,
                      dimension=c(100, 100),
                      projection='+proj=laea'))
})

test_that("ezgrid-4326-err-proj", {
  
  expect_error(ezgrid(extent=c(-180, 180, -90, 90), 
                      dimension = c(180, 90),
                      projection = 4326))
})

test_that("ezgrid-4326-err-ext", {
  
  expect_error(ezgrid(extent=c(-180, 180, -90, 90), 
                      dimension = c(180, 90, 0),
                      projection = 'EPSG:4326'))
})