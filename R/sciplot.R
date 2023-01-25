#' sciplot: plot methods using colours from `{scico}`
#' 
#' plot a 'SpatRaster' or 'stars' object using `{scico}` palettes. View palette
#' options with `sciplot_pals()`
#' 
#'
#' @param x object to plot, either a SpatRaster or stars object
#' @param pal Default is 'acton'. Character describing colour palette to use. choose from `sciplot_pals()`
#' @param n Numeric of length 1. The number of breaks/colours to include in the sclae
#' @param direction Default is 1. Direction of colour palette to reverse use -1.
#' @param centre Logical. If TRUE, the palette is centred around zero. good for bivariate/split palettes.
#' @param n_quantile Numeric. if not NULL sets the number of quantile breaks to use for sciplot.SpatRaster. See details.
#' @param ... additional args passed to plot.
#' @family plot_helpers
#' @details 
#' n_quantile sets quantile breaks for the colour palette. When using breaks, 
#' terra::plot does not support continuous legend as is the default in stars.
#' 
#' @examples 
#' 
#' src <- "/vsicurl/https://public.services.aad.gov.au/datasets/science/GEBCO_2021_GEOTIFF/GEBCO_2021.tif"
#' template <- ezgrid(c(-180, 180, -90, 90), c(720, 360), 'EPSG:4326')
#' world.el.terra <- ezwarp(x=src, y=template)
#' sciplot(world.el.terra, pal='oleron', centre=TRUE)
#' 
#' f <- system.file("ex/elev.tif", package="terra") 
#' r.stars <- stars::read_stars(f)
#' sciplot(r.stars)
#'
#' @export
sciplot <- function(x, pal="acton", n, direction, centre, ...) {
  UseMethod("sciplot")
}

#' @rdname sciplot
#' 
#' @export
sciplot.SpatRaster <- function(x, pal="acton", n=256, direction=1, centre=FALSE,
                               n_quantile=NULL, ...){
  brks <- NULL
  if (!is.null(n_quantile)){
    brks <- stats::quantile(x[], probs= seq(0,1, length.out=n_quantile), na.rm =TRUE)
  }
  
  pal <- scico::scico(n, palette = pal, direction=direction)
  
  if (isTRUE(centre)){
    max_absolute_value<-max(abs(terra::minmax(x)))
    
    col_to_include <- include_cols(x, max_absolute_value, n)
    
    terra::plot(x, col=pal[col_to_include], ...)
  } else {
    terra::plot(x, breaks=brks, col=pal, ...)
  }
}

#' @rdname sciplot
#' 
#' @export
sciplot.stars <- function(x, pal="acton", n = 11, direction=1, 
                          centre=FALSE, ...){
  
  pal <- scico::scico(n, palette = pal, direction=direction)
  
  if (isTRUE(centre)){
    max_absolute_value <- max(abs(c(min(x[[1]], na.rm =TRUE), 
                                  max(x[[1]], na.rm =TRUE))))
    
    col_to_include <- include_cols(x, max_absolute_value, n)
    
    plot(x, nbreaks=length(col_to_include)+1, 
         col=pal[col_to_include], breaks="equal", ...)
  } else {
    plot(x, nbreaks=n+1, col=pal, ...)
  }
}

#' view {scico} colour palette options
#' 
#' @rdname sciplot
#' @family plot_helpers
#' @return character vector of scico palette names. see `scico::scico_palette_names()`
#' @export
#' 
#' @details 
#' sciplot_pals plots the various sciplot palettes and returns their names as a
#' character vector. Wrapper for `scico::scico_palette_show()`  and `scico::scico_palette_names()`. 
#'
#' @examples
#' sciplot_pals()
sciplot_pals <- function(){
  scico::scico_palette_show()
  scico::scico_palette_names()
}

# for getting the right colours for centering at zero.
include_cols <- function(x, mav, n){
  color_sequence<-seq(-mav,mav,length.out=n+1)
  
  suppressWarnings({
    if (inherits(x, "SpatRaster")){
      n_in_class <-  hist(x[], breaks=color_sequence, plot=FALSE)$counts>0
    } else {
      n_in_class <-  hist(x, breaks=color_sequence, plot=FALSE)$counts>0
    }
  })
  
  min(which(n_in_class==T)):max(which(n_in_class==T))
}

