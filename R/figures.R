#' Point
#' 
#' Add a point figure.
#' 
#' @param g An object of class `g2r` as returned by [g2()].
#' @param ... Options to pass to the figure, including [asp()],
#' and [adjust()].
#' @param sync Whether to sync the axis data (align).
#' @param data A dataset (`data.frame` or `tibble`) to use to 
#' draw the figure.
#' @param inherit_asp Whether to inherit the aspects paseed to
#' [g2()] initialisation function.
#' 
#' @examples 
#' g2(cars) %>% 
#'  fig_point(asp(speed, dist))
#' 
#' g2(mtcars, asp(mpg, disp, size = qsec)) %>% 
#'  fig_point(asp(color = "red", shape = "square"))
#' 
#' @export 
fig_point <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_point")
}

#' @method fig_point g2r
#' @export 
fig_point.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "point"
  )
}

#' Line
#' 
#' Add a line figure.
#' 
#' @inheritParams fig_point
#' 
#' @examples 
#' g2(CO2, asp(conc, uptake, color = Plant)) %>% 
#'  fig_line()
#' 
#' @export 
fig_line <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_line")
}

#' @method fig_line g2r
#' @export 
fig_line.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "line"
  )
}

#' Area
#' 
#' Add an area figure.
#' 
#' @inheritParams fig_point
#' 
#' @examples 
#' g2(Orange, asp(age, circumference, color = Tree)) %>% 
#'  fig_area(adjust("stack"))
#' 
#' @export 
fig_area <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_area")
}

#' @method fig_area g2r
#' @export
fig_area.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "area"
  )
}

#' Interval
#' 
#' Add an interval figure.
#' 
#' @inheritParams fig_point
#' 
#' @examples
#' g2(sleep, asp(ID, extra, color = group)) %>% 
#'  fig_interval()
#' 
#' df <- data.frame(
#'  cat = letters[1:5],
#'  value = c(0.15, .3, .65, .75, .9)
#' )
#' 
#' g2(df, asp(cat, value, color = cat, shape = "funnel")) %>% 
#'  fig_interval(adjust("symmetric")) %>% 
#'  coord_type("rect") %>% 
#'  coord_transpose() %>% 
#'  coord_scale(-1, 1) %>% 
#'  axis_hide()
#' 
#' @name figures
#' 
#' @export 
fig_interval <- function(
  g, 
  ...,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_interval")
}

#' @method fig_interval g2r
#' @export 
fig_interval.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "interval"
  )
}

#' Polygon
#' 
#' Add a polygon figure.
#' 
#' @inheritParams fig_point
#' 
#' @export 
fig_polygon <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_polygon")
}

#' @method fig_polygon g2r
#' @export 
fig_polygon.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "polygon"
  )
}

#' Edge
#' 
#' Add an edge figure.
#' 
#' @inheritParams fig_point
#' 
#' @export 
fig_edge <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_edge")
}

#' @method fig_edge g2r
#' @export 
fig_edge.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "edge"
  )
}

#' Path
#' 
#' Add a path figure.
#' 
#' @inheritParams fig_point
#' 
#' @export 
fig_path <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_path")
}

#' @method fig_path g2r
#' @export 
fig_path.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "path"
  )
}

#' Path
#' 
#' Add a path figure.
#' 
#' @inheritParams fig_point
#' 
#' @keywords internal
fig_primitive <- function(
  g, 
  ..., 
  data = NULL, 
  inherit_asp = TRUE,
  sync = TRUE,
  type = c(
    "point", 
    "interval", 
    "line", 
    "schema", 
    "edge", 
    "area", 
    "polygon"
  )
){

  type <- match.arg(type)

  # adjust
  adjust <- get_adjust(...)

  # tooltip
  tooltip <- get_tooltip(...)

  # chart aspects
  asp <- get_asp(...)
  asp <- combine_asp(g$x$main_asp, asp, inherit_asp = inherit_asp)

  position <- select_asp_labels(asp, "x", "y")
  size <- select_asp_labels(asp, "size")
  color <- select_asp_labels(asp, "color")
  shape <- select_asp_labels(asp, "shape")
  label <- select_asp_labels(asp, "label")

  # store columns
  cols <- c(position, color, shape, size)
  g$x$cols <- append(g$x$cols, cols)

  # scales
  g <- sync(g, position, sync)
  g <- gauges_types(g, cols, data)
  
  opts <- list(
    type = type,
    position = position,
    color = color,
    data = select_columns(data, cols),
    shape = shape,
    size = size,
    adjust = adjust,
    tooltip = tooltip,
    label = label
  ) %>% 
    drop_nulls()

  g$x$views <- append(g$x$views, list(opts))

  g
}