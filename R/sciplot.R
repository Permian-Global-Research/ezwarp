#' plot using {scico} palettes
#'
#' plot using {scico} palettes
#' @title sciplot: generic plot but using palettes from {scico}
#' @param x ...
#' @export
sciplot <- function(x, ...) {
  UseMethod("sciplot")
}

#' @rdname sciplot
#' 
#' @export
sciplot.SpatRaster <- function(x, .pal="bilbao", .n=256, direction=1, ...){
  .pal <- scico::scico(.n, palette = .pal, direction=direction)
  terra::plot(x, col=.pal, ...)
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