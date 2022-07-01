mpc_endpoint <- function(){
  rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
}

mpc_dem <- function(aoi, src = c("cop-dem-glo-30", "alos-dem")) {
  stac_region <- sf::st_transform(aoi, 4326)  %>% 
    sf::st_bbox()
  
  it_obj <- mpc_endpoint()   %>% 
    rstac::stac_search(collections = src[1],
                       bbox = stac_region) |>
    rstac::get_request()
  
  src_list <- rstac::assets_list(it_obj)$path
  
  src_list[grep(".tif$", src_list)] %>% 
    as.list()
}