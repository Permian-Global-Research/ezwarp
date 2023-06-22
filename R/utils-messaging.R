# Package messages -------------------------------------------------------------
no_stars_error <- function() {
    cli::cli_abort(
        c(
            paste0(
                "The", cli::style_bold(" stars "),
                "package is required to carry out this warp configuration"
            ),
            "i" = paste0(
                "Please install ",
                cli::style_bold(" stars "),
                "and try again."
            ),
            "i" = "See: https://r-spatial.github.io/stars/ for instructions"
        )
    )
}

no_sf_error <- function() {
    cli::cli_abort(
        c(
            paste0(
                "The", cli::style_bold(" sf "),
                "package is required to carry out this warp configuration"
            ),
            "i" = paste0(
                "Please install ",
                cli::style_bold(" sf "),
                "and try again."
            ),
            "i" = "See: https://r-spatial.github.io/sf/ for instructions"
        )
    )
}

no_terra_error <- function() {
    cli::cli_abort(
        c(
            paste0(
                "The", cli::style_bold(" terra "),
                "package is required to carry out this warp configuration"
            ),
            "i" = paste0(
                "Please install ",
                cli::style_bold(" terra "),
                "and try again."
            ),
            "i" = "See: https://rspatial.github.io/terra/ for instructions"
        )
    )
}

# Warper Messages ---------------------------------------------------------------

check_options_error <- function(opt) {
    if (opt == "-crop_to_cutline") {
        cli::cli_abort(
            c(
                "The gdal option {opt} is not supported.",
                "i" = "Instead, use the 'crop_to_cutline' argument."
            )
        )
    } else {
        cli::cli_abort(
            "The gdal option {opt} is not supported."
        )
    }
}


out_class_warn <- function(out_class) {
    cli::cli_warn(c(
        paste0(
            "The requested {.var out_class} value:",
            cli::style_bold(" {out_class} "),
            "is not supported."
        ),
        "i" = "Returning SpatRaster."
    ))
}

engine_error <- function(engine) {
    cli::cli_abort(
        c("{engine} is not a supported {.var engine}",
            "i" = paste0(
                "choose from", cli::style_bold(" vapour "),
                "or", cli::style_bold(" sf")
            )
        )
    )
}

band_mismatch_error <- function() {
    cli::cli_abort(c("Input raster source band numbers differ for `x`.",
        "x" = "You must provide a value for `bands` in this case."
    ))
}

vapour_write_error <- function() {
    cli::cli_abort(c(
        "The vapour_write_raster_block function failed. Please check the ",
        "i" = "Check the vapour package documentation for more information:",
        paste0(
            "https://hypertidy.github.io/vapour/reference/",
            "vapour_write_raster_block.html"
        )
    ))
}




# Input Messages ---------------------------------------------------------------

not_list_error <- function(x) {
    cli::cli_abort(
        c(
            paste0("When > 1 source is provided for {.var x},
            it must be as a list not {class(x)}."),
            "i" = "For example: list(src1, src2, src3)"
        )
    )
}

template_length_error <- function(y) {
    cli::cli_abort(
        c("Only one spatial source can be provided for {.var y}.",
            "i" = "Currently, {.var y} has length {length(y)}."
        )
    )
}

no_res_vec_error <- function() {
    cli::cli_abort(
        "When `y` is a spatial vector source, `res` must be provided."
    )
}

not_logical_error <- function(x) {
    cli::cli_abort(
        c(
            paste0("The {.var x} argument must be a logical value."),
            "i" = "Currently, {.var x} is {class(x)}."
        )
    )
}

spat_source_error <- function(x) {
    cli::cli_abort(c("Source can't be read by gdal or ogr",
        "i" = "check the following source: {x}"
    ))
}

# ezgrid Messages ---------------------------------------------------------------

missing_param_error <- function(val) {
    cli::cli_abort("Can't return param {val} from ezgrid")
}

invlaid_extent_error <- function() {
    cli::cli_abort(
        "The target grid extent is invalid"
    )
}

invalid_dimension_error <- function() {
    cli::cli_abort(
        "The target grid dimensions are invalid"
    )
}

invalid_projection_error <- function() {
    cli::cli_abort(
        "The target grid projection is invalid"
    )
}
