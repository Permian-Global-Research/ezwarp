#' @title Suppress output
#' @description Suppress output from a function
#' @param x function to suppress output from
#' @return function with suppressed output
#' @details copied from:
#' https://stackoverflow.com/questions/34208564/how-to-hide-or-disable-in-function-printed-message
#' @noRd
.quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}
