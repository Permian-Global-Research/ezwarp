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

check_options <- function(o){
  if ("-crop_to_cutline" %in% o){
    stop(paste0("The option -crop_to_cutline is not supported. Instead, use ",
                "the crop_to_cutline argument in ezwarp function."))
  }
}

check_engine <- function(.eng){
  if (.eng[1]=="sf") check_sf()
}