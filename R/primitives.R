#' Point
#' 
#' Add a point figure.
#' 
#' @param g An object of class `g2r` or `g2Proxy` as returned by [g2()] or 
#' [g2_proxy()].
#' @param ... Options to pass to the figure, including [asp()],
#' and [adjust()], [active()], [selected()], and [config()], 
#' other key value pairs of are passed to 
#' [style](https://g2.antv.vision/en/docs/api/general/style).
#' @param sync Whether to sync the axis data (align) with that
#' used in other figures, set to `FALSE` to not sync or set to
#' a character string to use as name of sync group.
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

  if(is.null(data) && !is.null(g$x$graph))
    data <- g$x$data$nodes
  
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "point"
  )
}

#' @method fig_point g2Proxy
#' @export 
fig_point.g2Proxy <- function(
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

#' @method fig_line g2Proxy
#' @export 
fig_line.g2Proxy <- function(
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

#' @method fig_area g2Proxy
#' @export
fig_area.g2Proxy <- function(
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

#' @method fig_interval g2Proxy
#' @export 
fig_interval.g2Proxy <- function(
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

#' @method fig_polygon g2Proxy
#' @export 
fig_polygon.g2Proxy <- function(
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

  if(is.null(data) && !is.null(g$x$graph))
    data <- g$x$data$edges

  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "edge"
  )
}

#' @method fig_edge g2Proxy
#' @export 
fig_edge.g2Proxy <- function(
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

#' Schema
#' 
#' Add a schema figure.
#' 
#' @inheritParams fig_point
#' 
#' @export 
fig_schema <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_schema")
}

#' @method fig_schema g2r
#' @export 
fig_schema.g2r <- function(
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
    type = "schema"
  )
}

#' @method fig_schema g2Proxy
#' @export 
fig_schema.g2Proxy <- function(
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
    type = "schema"
  )
}

#' Path
#' 
#' Add a path figure.
#' 
#' @inheritParams fig_point
#'
#' @examples 
#' df <- data.frame(
#'  x = runif(100),
#'  y = runif(100) 
#' )
#' 
#' g2(df, asp(x, y)) %>% 
#'  fig_path()
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

#' @method fig_path g2Proxy
#' @export 
fig_path.g2Proxy <- function(
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

#' Heatmap
#' 
#' Add a path figure.
#' 
#' @inheritParams fig_point
#' 
#' @export 
fig_heatmap <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_heatmap")
}

#' @method fig_heatmap g2r
#' @export 
fig_heatmap.g2r <- function(
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
    type = "heatmap"
  )
}

#' @method fig_heatmap g2Proxy
#' @export 
fig_heatmap.g2Proxy <- function(
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
    type = "heatmap"
  )
}

#' Primitive
#' 
#' Add a path figure.
#' 
#' @inheritParams fig_point
#' @param asp Aspects to override those passed to `...`.
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
    "polygon",
    "heatmap",
    "path"
  ),
  asp = NULL
){
  type <- match.arg(type)

  # adjust
  adjust <- get_adjust(...)

  # animation
  animation <- get_animation(...)

  # state
  states <- get_state(...)

  # config
  conf <- get_config(...)

  # chart aspects
  if(is.null(asp))
    asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)

  position <- select_asp_labels(asp, "x", "y")
  size <- select_asp_labels(asp, "size")
  color <- select_asp_labels(asp, "color")
  shape <- select_asp_labels(asp, "shape")
  label <- select_asp_labels(asp, "label")
  interaction <- select_asp_labels(asp, "interplay")
  tooltip <- select_asp_labels(asp, "tooltip")
  style_asp <- select_asp_labels(asp, "style")

  style <- rm_asp(...)

  if(length(style_asp) && length(style)){
    style_asp <- collapse_asp(style_asp)
    style <- list(style_asp, style)
  } else if (length(style_asp) && !length(style)){
    style <- style_asp
  }
  
  # store columns
  cols <- c(position, color, shape, size, tooltip, label)
  g$x$cols <- append(g$x$cols, cols)

  # scales
  g <- sync(g, position[1], sync, if_true = "mainGroupX")
  g <- sync(g, position[2], sync, if_true = "mainGroupY")
  g <- gauges_types(g, cols, data)

  data <- select_columns(data, cols)

  # don't reorder those figures
  # either breaks it or there is no point
  if(!type %in% NO_REORDER_TYPES)
    data <- order_data(data, position)
  
  opts <- list(
    type = type,
    conf = conf,
    data = data,
    position = collapse_asp(position),
    color = collapse_asp(color),
    shape = collapse_asp(shape),
    size = collapse_asp(size),
    tooltip = collapse_asp(tooltip),
    label = collapse_asp(label),
    interaction = collapse_asp(interaction),
    adjust = adjust,
    style = style,
    animation = animation,
    states = states
  ) %>% 
    drop_nulls()

  g$x$views <- append(g$x$views, list(opts))

  g
}
