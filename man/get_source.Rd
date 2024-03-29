% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-get-source.R
\name{get_source}
\alias{get_source}
\alias{get_source.SpatRaster}
\alias{get_source.stars}
\alias{get_source.stars_proxy}
\alias{get_source.character}
\alias{get_source.sf}
\alias{get_source.sfc}
\alias{get_source.SpatVector}
\title{Get the source of a spatial object}
\usage{
get_source(x, force = FALSE)

\method{get_source}{SpatRaster}(x, force = FALSE)

\method{get_source}{stars}(x, force = FALSE)

\method{get_source}{stars_proxy}(x, force = FALSE)

\method{get_source}{character}(x, force = FALSE)

\method{get_source}{sf}(x, force = FALSE)

\method{get_source}{sfc}(x, force = FALSE)

\method{get_source}{SpatVector}(x, force = FALSE)
}
\arguments{
\item{x}{a raster source - either character i.e a local source or remote.
If remote, src must be prefixed with relevant gdal module. e.g. "/vsicurl/"}

\item{force}{Logical to determine if the SpatRaster or stars_proxy object should
be re-saved even if on disk sources exist.}
}
\value{
character source.
}
\description{
A class agnostic function to return or create the source of a spatial object.
}
\details{
If a SpatRaster is provided the source of the raster is used.
}
\examples{
f <- system.file("ex/elev.tif", package="terra") 
get_source(f)
get_source(terra::rast(f), force=TRUE)
f2 <- system.file("ex/lux.shp", package="terra") 
get_source(f2)

}
\seealso{
Other spatial helpers (class agnostic): 
\code{\link{get_dim}()},
\code{\link{get_ext}()},
\code{\link{get_proj}()}
}
\concept{spatial helpers (class agnostic)}
