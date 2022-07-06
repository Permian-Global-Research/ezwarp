check_terra <- function(){
  if (!requireNamespace("terra", quietly = TRUE))
    stop(paste0("The {terra} package required to carry out this warp configuration.\n",
          "Please see: https://rspatial.github.io/terra/index.html for instructions")) 
}

check_sf <- function(){
  if (!requireNamespace("sf", quietly = TRUE))
    stop(paste0("The {sf} package required to carry out this warp configuration.\n",
                "Please see: https://r-spatial.github.io/sf/ to for instructions")) 
}

check_stars <- function(){
  if (!requireNamespace("stars", quietly = TRUE))
    stop(paste0("The {stars} package required to carry out this warp configuration.\n",
                "Please see: https://r-spatial.github.io/stars/ to for instructions")) 
}