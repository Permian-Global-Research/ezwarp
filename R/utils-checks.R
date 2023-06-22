# Package checks ---------------------------------------------------------------
check_terra <- function() {
  if (!requireNamespace("terra", quietly = TRUE)) {
    no_terra_error()
  }
}

check_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    no_sf_error()
  }
}

check_stars <- function() {
  if (!requireNamespace("stars", quietly = TRUE)) {
    no_stars_error()
  }
}

check_engine <- function(.eng) {
  if (.eng[1] == "sf") check_sf()
}

# Warper checks ----------------------------------------------------------------
check_options <- function(o) {
  if ("-crop_to_cutline" %in% o) {
    check_options_error(o)
  }
}

check_vapour_write <- function(vap_result) {
  if (isFALSE(vap_result)) {
    vapour_write_error()
  }
}

check_n_bands <- function(b) {
  if (!length(unique(b)) == 1) {
    band_mismatch_error()
  }
}

# Input checks -----------------------------------------------------------------

#' check input source format/length
#'
#' ensures that a list is provided
#'
#' @param x x argument of ezwarp
#'
#' @return list
#' @noRd
check_in_form <- function(x) {
  if (length(x) > 1) {
    if (class(x) != "list") {
      not_list_error(x)
    }
  } else {
    x <- list(x)
  }
  return(x)
}


#' check input template source format/length
#'
#' ensures template is of length 1
#'
#' @param x y argument of ezwarp
#'
#' @return list
#' @noRd
check_grid_form <- function(x) {
  if (length(list(x)) > 1) {
    template_length_error(x)
  }
  return(x)
}


#' check that res is provided if template is a spatial vector
#'
#' ensures res is provided where spatial vector provided
#'
#' @param x
#'
#' @return list
#' @noRd
check_res_form <- function(x, .res) {
  if (missing(.res) && inherits(x, c("sf", "sfc", "SpatVector"))) {
    no_res_vec_error()
  } else if (missing(.res) && inherits(x, "ezgrid")) {
    if (is.null(x$dimension)) {
      no_res_vec_error()
    }
  }
}


check_logical <- function(x, .arg) {
  if (!is.logical(x)) {
    not_logical_error()
  }
}
