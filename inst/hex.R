library(ggplot2)
library(ezwarp)
library(sf)
library(terra)
library(ggpattern)
library(maps)
library(cropcircles)
library(magick)

make_logo <- function(
    .path,
    p1 = "+proj=ob_tran +o_proj=moll +o_lon_p=40 +o_lat_p=35 +lon_0=170",
    p2 = "+proj=tpers +h=6500000 +lat_0=5 +lon_0=112 +azi=23.5",
    text_size = 35,
    bgc = "#373654",
    viridis_pal = "mako",
    scico_pal = NULL,
    direction = 1,
    src = paste0(
        "/vsicurl/https://public.services.aad.gov.au/datasets/",
        "science/GEBCO_2021_GEOTIFF/GEBCO_2021.tif"
    )) {
    tf <- tempfile(fileext = ".png")
    p_logo <- data.frame(text = "ezwarp", .x = 0, .y = 0) |>
        ggplot(aes(".x", ".y", label = text)) +
        geom_text(size = text_size, family = "Racing Sans One") + # Monoton
        theme_void() +
        coord_fixed()
    ggsave(tf, p_logo,
        width = 180 * 3, height = 90 * 3, units = "mm"
    )

    wrld <- rast(extent = c(-180, 180, -90, 90), crs = "EPSG:4326") |>
        project(p1)

    r <- rast(tf)[[4]]
    ext(r) <- ext(wrld)
    crs(r) <- crs(wrld)
    r[r <= 0] <- NA
    r[r > 0] <- 1
    r_p <- project(r, p2)


    rv <- as.polygons(r_p) |>
        st_as_sf()

    st_crs(rv)$proj4string

    bbox <- st_bbox(r_p)

    elev <- ezwarp(src, r_p, res = 1e4)
    elev_na <- elev
    elev_na[elev_na <= 0] <- NA

    elv_df <- as.data.frame(elev_na, xy = TRUE)

    gg1 <- ggplot(rv) +
        geom_sf_pattern(
            pattern = "gradient",
            pattern_fill = "#d3d3d3",
            pattern_fill2 = bgc,
            colour = "white",
            pattern_alpha = 0.8,
            linewidth = 0.2
        ) +
        geom_tile(data = elv_df, aes(x, y, fill = Band1), alpha = 0.3) +
        coord_sf(
            xlim = c(bbox$xmin, bbox$xmax),
            ylim = c(bbox$ymin, bbox$ymax), expand = TRUE
        ) +
        guides(fill = FALSE) +
        theme_void() +
        theme(
            panel.background = element_rect(
                fill = bgc,
                color = bgc
            ),
            plot.background = element_rect(
                fill = bgc,
                color = bgc
            ),
            panel.grid.major = element_line(color = "#e6e6e6", size = 0.2),
            panel.border = element_blank(),
            plot.margin = margin(1, 1, 3, 1, "cm")
        )

    if (!is.null(scico_pal)) {
        gg1 <- gg1 + scico::scale_fill_scico(
            palette = scico_pal,
            trans = scales::yj_trans(0.5),
            direction = direction
        )
    } else {
        gg1 <- gg1 + scale_fill_viridis_c(
            option = viridis_pal,
            trans = scales::yj_trans(0.5),
            direction = direction
        )
    }

    world <- sf::st_as_sf(map("world", plot = FALSE, fill = TRUE)) |>
        st_make_valid() |>
        sf::st_transform(p2)

    gg2 <- gg1 +
        geom_sf(data = world, fill = NA, colour = alpha("white", 0.1))

    tf2 <- tempfile(fileext = ".png")
    ggsave(tf2, gg2)

    img <- image_read(hex_crop(tf2,
        border_size = 8,
        border_colour = "white"
    ))

    perm <- image_scale(
        image_read(paste0(
            "https://avatars.githubusercontent.com/u/",
            "106586419?s=400&u=69fb140fd7d3f204f361e85dba7398ac33d88a03&v=4"
        )),
        "x150"
    )

    comp <- image_composite(img, perm, offset = "+1250-2800")
    image_write(comp, .path)

    return(.path)
}



p <- make_logo(
    .path = "man/figures/ezwarp-hex.png",
    p1 = "+proj=ob_tran +o_proj=moll +o_lon_p=40 +o_lat_p=70 +lon_0=152",
    p2 = "+proj=tpers +h=6500000 +lat_0=5 +lon_0=112 +azi=23.5",
    text_size = 35,
    bgc = "#736180",
    scico_pal = "tokyo"
)

p
# Compose images using one of many operators

hex <- image_read("man/figures/ezwarp-hex.png")
perm <- image_scale(
    image_read("https://avatars.githubusercontent.com/u/106586419?s=400&u=69fb140fd7d3f204f361e85dba7398ac33d88a03&v=4"),
    "x150"
)
# Standard is atop
comp <- image_composite(hex, perm, offset = "+1250-2800")
image_write(comp, "man/figures/ezwarp-hex-perm.png")
