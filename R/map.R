#' Get Map
#'
#' Retrieve map data to pass to the `maps`
#' argument of the `fig_map()` function.
#'
#' @param iso3c Iso3c code of the country to retrieve, e.g.: `USA`.
#' @param keep Proportion of points to retain, it is highly recommended
#' to reduce the detail of the map or it will take too long to load in the
#' browser. Set to `NULL` to not reduce the amount of details.
#' @param name Name of the database to use.
#' @param region Character vector that names the polygons to draw.
#' @param level Level of the polygons to draw, either the region or
#' subregion.
#'
#' @section Functions:
#'
#' - `get_gadm_data`: Retrieves country-level data from [gadm.org](https://gadm.org/).
#' - `get_map_data`: Uses the `maps::map()` function to retrieve the map data, similar to `ggplot2::map_data`.
#' - `get_world_map`: Retrives a world map (from GeoJSON).
#'
#' @importFrom jsonlite read_json
#' @importFrom tibble tibble
#' @importFrom purrr map_dfr
#' @importFrom utils download.file
#'
#' @name map_data
#' @export
get_gadm_data <- function(iso3c, level = c(0, 1, 2, 3, 4), keep = 0.05) {
  check_package("rmapshaper")

  if (missing(iso3c)) {
    stop("Missing `iso3c`", call. = FALSE)
  }

  # ensure it's all caps
  iso3c <- toupper(iso3c)

  # base GADM URL
  BASE <- "https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_%s_%s_sp.rds"

  #  download to temp
  url <- sprintf(BASE, iso3c, level[1])
  temp <- tempfile(fileext = ".rds")
  on.exit({
    unlink(temp)
  })

  dl <- tryCatch(
    download.file(url, temp, quiet = TRUE),
    error = function(e) e
  )

  if (inherits(dl, "error")) {
    stop("Failed to download data, is the `iso3c` code correct?", call. = FALSE)
  }

  sp <- readRDS(temp)

  if (!is.null(keep)) {
    sp <- suppressWarnings(
      rmapshaper::ms_simplify(sp, keep = keep)
    )
  }

  sp
}

#' @rdname map_data
#' @export
get_map_data <- function(
  region = ".",
  level = c("region", "subregion"),
  name = "world"
) {
  check_package("maps")

  level <- match.arg(level)

  map <- maps::map(
    name,
    region,
    exact = FALSE,
    plot = FALSE,
    fill = TRUE
  )

  df <- tibble(
    longitude = map$x,
    latitude = map$y,
    group = cumsum(is.na(map$x) & is.na(map$y)) + 1,
    order = seq_along(map$x)
  )

  names <- do.call("rbind", lapply(strsplit(map$names, "[:,]"), "[", 1:2))
  df$region <- names[df$group, 1]
  df$subregion <- names[df$group, 2]
  df <- df[stats::complete.cases(df$latitude, df$longitude), ]

  split(df, df[[level]]) %>%
    map_dfr(function(grp, lvl) {
      t <- tibble(
        longitude = list(grp$longitude),
        latitude = list(grp$latitude)
      )

      t[["level"]] <- unique(grp[[lvl]])
      return(t)
    }, lvl = level)
}

#' @rdname map_data
#' @export
get_world_map <- function() {
  check_alter()

  url <- "https://raw.githubusercontent.com/JohnCoene/echarts4r.maps/master/inst/World.json"

  json <- read_json(url)

  map <- map_alter(json)
  map[, 14] <- NULL
  map
}

#' Alter GeoJSON
#'
#' @param geojson Geojson as list.
#'
#' @keywords internal
map_alter <- function(geojson) {
  check_alter()

  dataset <- alter::Alter$new(geojson)$
    source(type = "GeoJSON")$
    transform(
    type = "geo.projection",
    projection = "geoMercator"
  )$
    get("rows")

  if ("properties" %in% names(dataset)) {
    properties <- dataset$properties
    dataset <- cbind.data.frame(dataset, properties)
    dataset$properties <- NULL
  }

  structure(dataset, class = c("geoData", class(dataset)))
}

#' Map
#'
#' Add a map figure.
#'
#' @inheritParams fig_point
#' @param map Name of map to pass to the `region` argument
#' of the [get_map_data()] function, or map object as returned by
#' [get_world_map()], [get_gadm_data()], or [get_map_data()],
#' or a `SpatialPolygonsDataFrame` as
#' returned by [raster::getData()], or a `geo_list` as
#' obtained from `geojsonio::geojson_list()`, or `MULTIPOLYGON`
#' of the class`sf` as obtained from reading shapefiles.
#'
#' @examples
#' g2() %>%
#'   fig_map(stroke = "#fff", fill = "gray") %>%
#'   axis_hide()
#' @export
fig_map <- function(
  g,
  ...,
  inherit_asp = TRUE,
  sync = TRUE,
  map = get_world_map()
) {
  UseMethod("fig_map")
}

#' @export
#' @method fig_map g2r
fig_map.g2r <- function(
  g,
  ...,
  inherit_asp = TRUE,
  sync = TRUE,
  map = get_world_map()
) {
  data <- map_data(map)

  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  asp$x <- "longitude"
  asp$y <- "latitude"

  fig_primitive(
    g,
    ...,
    data = data,
    inherit_asp = inherit_asp,
    sync = sync,
    type = "polygon",
    asp = asp
  )
}

#' @export
map_data <- function(map, ...) UseMethod("map_data")

#' @export
map_data.default <- function(map, ...) {
  map_data()
}

#' @export
#' @method map_data sf
#' @importFrom tibble tibble
map_data.sf <- function(map, ...) {
  check_package("sf")

  coords <- sf::st_coordinates(map) %>%
    as.data.frame()

  if (!"L3" %in% names(coords)) {
    stop("Must be a MULTIPOLYGON object", call. = FALSE)
  }

  #  remove geometry
  map$geometry <- NULL

  coords <- split(coords, coords[["L3"]]) %>%
    map_dfr(function(polygon) {
      tibble(
        longitude = list(polygon[["X"]]),
        latitude = list(polygon[["Y"]])
      )
    })

  # add other column
  cbind.data.frame(coords, map)
}

#' @export
#' @method map_data data.frame
map_data.data.frame <- function(map, ...) {
  return(map)
}

#' @export
#' @method map_data character
map_data.character <- function(map, ...) {
  get_map_data(map)
}

#' @export
#' @method map_data SpatialPolygonsDataFrame
map_data.SpatialPolygonsDataFrame <- function(map, ...) {
  check_package("geojsonio")
  map <- geojsonio::geojson_list(map)
  map_data(map)
}

#' @export
#' @method map_data geo_list
map_data.geo_list <- function(map, ...) {
  class(map) <- "list"
  map_data(map)
}

#' @export
#' @method map_data list
map_data.list <- function(map, ...) {
  map_alter(map)
}
