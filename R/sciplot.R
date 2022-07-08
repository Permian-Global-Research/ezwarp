#' plot using {scico} palettes
#'
#' plot using {scico} palettes
#' @title sciplot: generic plot but using palettes from {scico}
#' @param x ...
#' @export
sciplot <- function(x, .pal="acton", .n=256, direction=1, centre=FALSE, ...) {
  UseMethod("sciplot")
}

#' @rdname sciplot
#' 
#' @export
sciplot.SpatRaster <- function(x, .pal="acton", .n=256, direction=1, centre=FALSE,
                               ...){
  
  .pal <- scico::scico(.n, palette = .pal, direction=direction)
  
  if (isTRUE(centre)){
    max_absolute_value<-max(abs(terra::minmax(x)))
    
    col_to_include <- include_cols(x, max_absolute_value, .n)
    
    terra::plot(x, col=.pal[col_to_include])
  } else {
    terra::plot(x, col=.pal, ...)
  }
}

#' @rdname sciplot
#' 
#' @export
sciplot.stars <- function(x, .pal="acton", .n=256, direction=1, centre=FALSE, ...){
  .pal <- scico::scico(.n, palette = .pal, direction=direction)
  
  if (isTRUE(centre)){
    max_absolute_value<-max(abs(c(min(x$values), max(x$values))))
    
    col_to_include <- include_cols(x, max_absolute_value, .n)
    
    plot(x, col=.pal[col_to_include], breaks="equal", ...)
  } else {
    plot(x, col=.pal, ...)
  }
}

#' view {scico} colour palette options
#'
#' @return
#' @export
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
  n_in_class<-hist(x, breaks=color_sequence, plot=F)$counts>0
  min(which(n_in_class==T)):max(which(n_in_class==T))
}

