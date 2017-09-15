
<!-- README.md is generated from README.Rmd. Please edit that file -->
discrete
========

The goal of discrete is to ...

Installation
------------

You can install discrete from github with:

``` r
# install.packages("devtools")
devtools::install_github("hypertidy/discrete")
```

required components
===================

TILING
------

-\[ \] line is a fall back, might have internal tiling -\[ \] or impose an arbitrary one -\[ \] needs index-alignment, tiling done in index space, converted to sf

POLYGONS
--------

-\[x\] non-raster conversion to sf from extent/dims -\[ \]

EXTENT / BBOX
-------------

-\[x\] a "discrete" class which is xmin, ymin, xmax, ymax, nx, ny -\[x\] conversion of GDAL 6-figure transform to "discrete" -\[ \] tile index conversion to cell edge, so c(offsetX, offsetY, windowX, windowY) &lt;-&gt; c(xmin, ymin, xmax, ymax) -\[ \] spatial extent &lt;-&gt; index extent, based on "discrete"

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
#f <- tail(raadtools::sstfiles()$fullname, 1)
#f <- "/rdsi/PUBLIC/raad/data/eclipse.ncdc.noaa.gov/pub/OI-daily-v2/NetCDF/2017/AVHRR/avhrr-only-v2.20170910_preliminary.nc"
f <- 'NETCDF:"/rdsi/PUBLIC/raad/data/eclipse.ncdc.noaa.gov/pub/OI-daily-v2/NetCDF/2017/AVHRR/avhrr-only-v2.20170910_preliminary.nc":sst'
#f <-  "/rdsi/PUBLIC/raad/data/www.ngdc.noaa.gov/mgg/global/relief/ETOPO2/ETOPO2v2-2006/ETOPO2v2c/netCDF/ETOPO2v2c_f4.nc"

## let's assume we did this without raster
# library(raster)
# r <- raster(f)
# projection(r) <- "+init=epsg:4326"
# tiles <- spex::polygonize(raster(raster::extent(r), crs = projection(r),
#                                  nrow = nrow(r) / 240, ncol = ncol(r)/240))
ri <- raster_info(f)
tiles <- tile_gon(transform6(ri$geotransform, ri$dimXY/240))


tiles$raster_data <- populate_raster(nrow(tiles))
library(sf)
library(spex)
extent <- function(x, ...) UseMethod("extent")
extent.sfc_POLYGON <- function(x, ...) raster::extent(attr(x, "bbox")[c("xmin", "xmax", "ymin", "ymax")])

itile <- 1
library(sf)
gs <- gdal_source(r, st_geometry(tiles[itile, ]))
library(vapour)
tiles$raster_data <- populate_raster(nrow(tiles), a$tile, raster_io(f, c(gs, c(diff(gs[c(1, 3)])+1, diff(gs[c(2, 3)])+1))))
tiles$tile <- seq_len(nrow(tiles))
par(mfrow = c(2, 1))
plot(crop(r, tiles[itile, ]), asp = "")
plot(tiles$raster_data[[itile]]$data, pch = ".")


library(mapedit)
a <- selectFeatures(tiles)
gs <- gdal_source(r, st_geometry(a))
tiles$raster_data <- populate_raster(nrow(tiles), a$tile, raster_io(f, c(gs, c(diff(gs[c(1, 3)])+1, diff(gs[c(2, 3)])+1))))
par(mfrow = c(2, 1))
plot(crop(r, tiles[itile, ]), asp = "")
plot(tiles$raster_data[[a$tile]]$data, pch = ".")
```
