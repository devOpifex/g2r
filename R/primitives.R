#' Point
#' 
#' Add a point figure.
#' 
#' @param g An object of class `g2r` or `g2Proxy` as returned by [g2()] or 
#' [g2_proxy()].
#' @param ... Options to pass to the figure, including [asp()],
#' and [adjust()], [active()], and [selected()].
#' @param sync Whether to sync the axis data (align) with that
#' used in other figures, set to `FALSE` to not sync or set to
#' a character string to use as name of sync group.
#' @param data A dataset (`data.frame` or `tibble`) to use to 
#' draw the figure.
#' @param inherit_asp Whether to inherit the aspects paseed to
#' [g2()] initialisation function.
#' @param style A list of options to pass to
#' [style](https://g2.antv.vision/en/docs/api/general/style) that 
#' customise the figure.
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
  inherit_asp = TRUE,
  style = NULL
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
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "point",
    style = style
  )
}

#' @method fig_point g2Proxy
#' @export 
fig_point.g2Proxy <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "point",
    style = style
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
  inherit_asp = TRUE,
  style = NULL
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
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "line",
    style = style
  )
}

#' @method fig_line g2Proxy
#' @export 
fig_line.g2Proxy <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "line",
    style = style
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
  inherit_asp = TRUE,
  style = NULL
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
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "area",
    style = style
  )
}

#' @method fig_area g2Proxy
#' @export
fig_area.g2Proxy <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "area",
    style = style
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
  inherit_asp = TRUE,
  style = NULL
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
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "interval",
    style = style
  )
}

#' @method fig_interval g2Proxy
#' @export 
fig_interval.g2Proxy <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "interval",
    style = style
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
  inherit_asp = TRUE,
  style = NULL
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
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "polygon",
    style = style
  )
}

#' @method fig_polygon g2Proxy
#' @export 
fig_polygon.g2Proxy <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "polygon",
    style = style
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
  inherit_asp = TRUE,
  style = NULL
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
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "edge",
    style = style
  )
}

#' @method fig_edge g2Proxy
#' @export 
fig_edge.g2Proxy <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "edge",
    style = style
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
  inherit_asp = TRUE,
  style = NULL
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
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "schema",
    style = style
  )
}

#' @method fig_schema g2Proxy
#' @export 
fig_schema.g2Proxy <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "schema",
    style = style
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
  inherit_asp = TRUE,
  style = NULL
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
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "path",
    style = style
  )
}

#' @method fig_path g2Proxy
#' @export 
fig_path.g2Proxy <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "path",
    style = style
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
  inherit_asp = TRUE,
  style = NULL
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
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "heatmap",
    style = style
  )
}

#' @method fig_heatmap g2Proxy
#' @export 
fig_heatmap.g2Proxy <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){
  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "heatmap",
    style = style
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
  style = NULL,
  asp = NULL
){
  UseMethod("fig_primitive")
}

#' @method fig_primitive g2r
#' @export
fig_primitive.g2r <- function(
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
  style = NULL,
  asp = NULL
){

  fig_primitive_common(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = type,
    style = style,
    asp = asp
  )

}

#' @method fig_primitive g2Proxy
#' @export
fig_primitive.g2Proxy <- function(
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
  style = NULL,
  asp = NULL
){

  g <- fig_primitive_common(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = type,
    style = style,
    asp = asp
  )

  g$x$views[[1]]$data <- proxy_data(g$x$views[[1]]$data)

  g$session$sendCustomMessage("figure", g$x)

  g$x$views <- NULL

  g
}

fig_primitive_common <- function(
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
  style = NULL,
  asp = NULL
){

  type <- match.arg(type)

  data <- get_data(g, data)

  # adjust
  adjust <- get_adjust(...)

  # animation
  animation <- get_animation(...)

  # state
  states <- get_state(...)

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

  # overwrite style
  if(length(style_asp) > 0 && !is.null(style))
    warning(
      "Cannot use `style` aspect and `style` argument together, ",
      "using the former", call. = FALSE
    )

  if(is.null(style))
    style <- collapse_asp(style_asp)
  
  # store columns
  cols <- c(position, color, shape, size, tooltip, label)
  g$x$cols <- append(g$x$cols, cols)

  # scales
  g <- sync(g, position[1], sync, if_true = "mainGroupX")
  g <- sync(g, position[2], sync, if_true = "mainGroupY")
  g <- gauges_types(g, cols, data)
  
  opts <- list(
    type = type,
    data = select_columns(data, cols),
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
