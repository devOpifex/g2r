#' Get Map
#' 
#' Retrieve map data from [echarts4r.maps](https://github.com/JohnCoene/echarts4r.maps).
#' 
#' @param name The name of the map.
#' 
#' @importFrom jsonlite read_json
#' @export 
get_map_data <- function(name = "World"){

  check_alter()
  
  BASE <- "https://raw.githubusercontent.com/JohnCoene/echarts4r.maps/master/inst/%s.json"

  url <- sprintf(BASE, name)
  json <- read_json(url)

  map_alter(json)
}

map_alter <- function(geojson){
  dataset <- alter::Alter$new(geojson)$
    source(type = "GeoJSON")$
    get("rows")

  structure(dataset, class = c("geoData", class(dataset)))
}

#' Map
#' 
#' Add a map figure.
#' 
#' @inheritParams fig_point
#' @param map Name of map, or map object as returned by 
#' [get_map_data()], or a `SpatialPolygonsDataFrame` as
#' as returned by [raster::getData], or a `geo_list` as
#' obtained from `geojsonio::geojson_list()`.
#' 
#' @export 
fig_map <- function(
  g, 
  ...,
  map = "World"
){
  UseMethod("fig_map")
}

#' @export 
#' @method fig_map g2r
fig_map.g2r <- function(
  g,
  ...,
  map = "World",
  keep = 0.05
) {

  data <- map_data(map, keep = 0.05)

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
  map_data("World")
}

#' @export 
#' @method map_data character
map_data.character <- function(map, ...) {
  get_map_data(map)
}

#' @export 
#' @method map_data SpatialPolygonsDataFrame
map_data.SpatialPolygonsDataFrame <- function(map, ..., keep = 0.05) {
  check_package("rmapshaper")
  rmapshaper::ms_simplify(map, keep = keep)
}

#' @export 
#' @method map_data geo_list
map_data.geo_list <- function(map, ...) {
  map <- geojsonio::geojson_list(map)
  class(map) <- "list"
  map_data(map)
}

#' @export 
#' @method map_data list
map_data.list <- function(map, ...) {
  map_alter(map)
}
