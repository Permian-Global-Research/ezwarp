
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ezwarp

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of ezwarp is to …

make warping easy - more to come.

``` r
library(ezwarp)
library(terra)
template <- ezgrid(c(-180, 180, -90, 90),
                   dimension = c(720, 360),
                   projection='EPSG:4326')

src <- "/vsicurl/https://public.services.aad.gov.au/datasets/science/GEBCO_2021_GEOTIFF/GEBCO_2021.tif"

world.el.terra <- ezwarp(x=src, y=template)
world.el.stars <- ezwarp(x=src, y=template, out_class = 'stars')
sciplot(world.el.terra, pal='bukavu', centre=TRUE)
sciplot(world.el.stars, pal='oleron', n =256, centre=TRUE)
```

<img src="man/figures/README-simple-world, figures-side-1.png" width="50%" /><img src="man/figures/README-simple-world, figures-side-2.png" width="50%" />

cutline…

``` r
wrld <-"/vsizip//vsicurl/https://github.com/wmgeolab/geoBoundaries/raw/main/releaseData/CGAZ/geoBoundariesCGAZ_ADM0.zip"

world.el.mask <- ezwarp(x=src, y=src, res=0.5, cutline = wrld)

sciplot(world.el.mask, pal='lapaz', range=c(0, max(world.el.mask[], na.rm=TRUE)))
```

<img src="man/figures/README-mask-world-1.png" width="100%" />

cutline with sql query

``` r
library(sf)

esri_sat <- "<GDAL_WMS><Service name=\"TMS\"><ServerUrl>http://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/${z}/${y}/${x}</ServerUrl></Service><DataWindow><UpperLeftX>-20037508.34</UpperLeftX><UpperLeftY>20037508.34</UpperLeftY><LowerRightX>20037508.34</LowerRightX><LowerRightY>-20037508.34</LowerRightY><TileLevel>17</TileLevel><TileCountX>1</TileCountX><TileCountY>1</TileCountY><YOrigin>top</YOrigin></DataWindow><Projection>EPSG:900913</Projection><BlockSizeX>256</BlockSizeX><BlockSizeY>256</BlockSizeY><BandsCount>3</BandsCount><MaxConnections>10</MaxConnections><Cache /></GDAL_WMS>"

f <- system.file("gpkg", "nc.gpkg", package = "sf")

nc.mask <- ezwarp(x=esri_sat, y=esri_sat, res=100, cutline = f,
                   crop_to_cutline = TRUE, nodata = -99)

stokes.mask <- ezwarp(x=esri_sat, y=esri_sat, res=25, cutline = f,
                   options=c("-csql", "SELECT * FROM 'nc.gpkg' WHERE NAME = 'Stokes'"),
                   crop_to_cutline = TRUE, nodata = -99)

f_sf <- read_sf(f) %>% 
  st_transform(vapour::vapour_raster_info(esri_sat)$projection)

terra::plotRGB(nc.mask)
plot(st_geometry(f_sf), add=TRUE, border='grey90')
terra::plotRGB(stokes.mask)
plot(st_geometry(f_sf), add=TRUE, border='grey10')
```

<img src="man/figures/README-mask-che, figures-side-1.png" width="50%" /><img src="man/figures/README-mask-che, figures-side-2.png" width="50%" />

This example is just to illustrate that ezwarp can handle differing
inputs as sources - url/filepath/SpatRaster/stars and can use all of
these along with sf/sfc/SpatVector as a template too.

``` r
nc_sub1 <- f_sf[1:10,]
nc_sub2 <- vect(f_sf[90:100,])

nc_sub1.terra <- ezwarp(x=src, y=nc_sub1, res=100, cutline= nc_sub1,
                        crop_to_cutline = TRUE) 

nc_sub2.stars <- ezwarp(x=src, y=nc_sub2, res=200, cutline= nc_sub2,
                        crop_to_cutline = TRUE, out_class = 'stars')

multi.ras <- ezwarp(x=list(nc_sub1.terra, nc_sub2.stars), 
                    y=f_sf, res=200, out_class = 'stars')


sciplot(nc_sub1.terra, pal = 'bamako')
plot(st_geometry(f_sf), add=TRUE, border='grey30')

sciplot(nc_sub2.stars, pal = 'vanimo', reset=FALSE)
plot(st_geometry(f_sf), add=TRUE, border='grey30', reset=TRUE)

sciplot(multi.ras, pal = 'romaO', reset=FALSE)
plot(st_geometry(f_sf), add=TRUE, border='grey30', reset=TRUE)
```

<img src="man/figures/README-mixed-source, figures-side-1.png" width="33%" /><img src="man/figures/README-mixed-source, figures-side-2.png" width="33%" /><img src="man/figures/README-mixed-source, figures-side-3.png" width="33%" />
