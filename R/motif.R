#' Motif
#' 
#' Set the motif of the chart, defaults to `light`.
#' 
#' @inheritParams fig_point
#' @param ... Key value pair defining style, or
#' [element()].
#' @param renderer Renderer to use, defaults to `canvas`.
#' @param padding An integer, or a vector of length 4.
#' @param visible Whether the chart is visible.
#' @param brandColor Main default color.
#' @param backgroundColor Plot background color.
#' @param path Path to JSON file.
#' @param motif List defnining the theme, similar to JSON.
#' 
#' @details The function [motif_from_json()] can be used to
#' define the theme from a JSON file of theme, to see the 
#' default theme file: 
#' `system.file("theme.json", package = "g2r")`.
#' 
#' @section Functions:
#' 
#' - `motif`: Defines the motif of a visualisation.
#' - `motif_from_json`: Defines the motif from a JSON
#' file of theme, see the theme file. 
#' `system.file("theme.json", package = "g2r")`
#' - `motif_from_list`: Defines the motif from a `list`,
#' derived from the JSON file.
#' - `motif_as_list`: Returns a motif as a `list` to use
#' with [motif_from_list()].
#' 
#' @examples
#' g2(iris, asp(Sepal.Width, Sepal.Length)) %>% 
#'  fig_point(
#'    asp(color = Species, shape = "circle")
#'  ) %>% 
#'  motif(
#'    brandColor = "orange",
#'    backgroundColor = "black",
#'    elementPoint(
#'      shape = "circle",
#'      stroke = "white",
#'      fillOpacity = .7
#'    )
#'  )
#' 
#' @name motif
#' @export 
motif <- function(
  g, 
  ..., 
  brandColor = NULL,
  backgroundColor = "transparent",
  renderer = c("canvas", "svg"),
  padding = "auto",
  visible = TRUE
){
  UseMethod("motif")
}

#' @method motif g2r
#' @export 
motif.g2r <- function(
  g, 
  ...,
  brandColor = NULL,
  backgroundColor = "transparent",
  renderer = c("canvas", "svg"),
  padding = "auto",
  visible = TRUE
){
  renderer <- match.arg(renderer)

  misc <- rm_elements(...)
  geoms <- get_elements(...)

  # theme options
  theme <- list(
    styleSheet = drop_nulls(
      list(
        brandColor = brandColor,
        backgroundColor = backgroundColor
      )
    )
  ) %>% 
    append(misc)

  theme <- make_geoms(theme, geoms)

  g$x$motif <- theme

  # chart options
  g$x$chartOpts$renderer <- renderer
  g$x$chartOpts$padding <- padding
  g$x$chartOpts$visible <- visible
  g$x$chartOpts$autoFit <- TRUE
  
  g
}

#' @rdname motif
#' @export
motif_from_json <- function(g, path){
  UseMethod("motif_from_json")
}

#' @export 
#' @method motif_from_json g2r
motif_from_json.g2r <- function(g, path){
  if(missing(path))
    stop("Missing `path`", call. = FALSE)
  
  lst <- jsonlite::read_json(path)
  motif_from_list(g, lst)
}

#' @rdname motif
#' @export
motif_from_list <- function(g, motif){
  UseMethod("motif_from_json")
}

#' @export 
#' @method motif_from_list g2r
motif_from_list.g2r <- function(g, motif){
  g$x$motif <- motif
  g
}

#' @rdname motif
#' @export
motif_as_list <- function(
  ...,
  brandColor = NULL,
  backgroundColor = "transparent"
){
  misc <- rm_elements(...)
  geoms <- get_elements(...)

  # theme options
  theme <- list(
    styleSheet = drop_nulls(
      list(
        brandColor = brandColor,
        backgroundColor = backgroundColor
      )
    )
  ) %>% 
    append(misc)

  make_geoms(theme, geoms)
}

#' Element
#' 
#' Function to use in [motif()] and style specific elements.
#' 
#' @param ... Key value pairs to pass to `style`.
#' @param figure Figure to modify.
#' @param shape Shape to modify, if `NULL` selects
#' a common default based on the figure, 
#' e.g.: `hollow-circle` for the `point` shape.
#' @param state State of the shape to modify.
#' 
#' @section Functions:
#' 
#' [element()] will work for any figure, but other
#' functions may be more convienient to use.
#' 
#' - `element`: Customise any element.
#' - `elementPoint`: Customise point.
#' - `elementLine`: Customise line.
#' - `elementArea`: Customise area.
#' - `elementEdge`: Customise edge.
#' - `elementInterval`: Customise interval.
#' - `elementPolygon`: Customise polygon.
#' - `elementSchema`: Customise schema.
#' 
#' @examples
#' g2(iris, asp(Sepal.Width, Sepal.Length)) %>% 
#'  fig_point(
#'    asp(color = Species, shape = "circle")
#'  ) %>% 
#'  motif(
#'    brandColor = "orange",
#'    backgroundColor = "black",
#'    elementPoint(
#'      shape = "circle",
#'      stroke = "white",
#'      fillOpacity = .7
#'    )
#'  )
#' 
#' @name elements
#' @export 
element <- function(
  ...,
  shape = NULL,
  figure = c(
    "point",
    "area",
    "edge",
    "line",
    "interval",
    "polygon",
    "schema"
  ),
  state = c(
    "default",
    "active",
    "inactive",
    "selected"
  )
) {

  state <- match.arg(state)
  figure <- match.arg(figure)

  if(is.null(shape))
    shape <- default_shape(figure)

  el <- list(
    figure = figure,
    shape = shape,
    state = state,
    opts = list(
      ...
    )
  )
  
  structure(el, class = c("element", class(el)))
}

#' @rdname elements
#' @export 
elementPoint <- function(
  ..., 
  shape = c(
    "hollow-circle",
    "cross",
    "hypen",
    "line",
    "plus",
    "tick",
    "circle",
    "square",
    "bowtie",
    "diamond",
    "hexagon",
    "triangle",
    "triangle-down",
    "hollow-square",
    "hollow-bowtie",
    "hollow-triangle-down"
  ), 
  state = c(
    "default",
    "active",
    "inactive",
    "selected"
  )
){
  shape <- match.arg(shape)
  element(..., state = state, shape = shape, figure = "point")
}

#' @rdname elements
#' @export 
elementLine <- function(
  ..., 
  shape = c(
    "line",
    "dot",
    "dash",
    "smooth",
    "hv",
    "vh",
    "hvh",
    "vhv"
  ), 
  state = c(
    "default",
    "active",
    "inactive",
    "selected"
  )
){
  shape <- match.arg(shape)
  element(..., state = state, shape = shape, figure = "line")
}

#' @rdname elements
#' @export 
elementArea <- function(
  ..., 
  shape = c(
    "area",
    "smooth",
    "line",
    "smooth-line"
  ), 
  state = c(
    "default",
    "active",
    "inactive",
    "selected"
  )
){
  shape <- match.arg(shape)
  element(..., state = state, shape = shape, figure = "area")
}

#' @rdname elements
#' @export 
elementEdge <- function(
  ..., 
  shape = c(
    "line",
    "vhv",
    "smooth",
    "arc"
  ), 
  state = c(
    "default",
    "active",
    "inactive",
    "selected"
  )
){
  shape <- match.arg(shape)
  element(..., state = state, shape = shape, figure = "edge")
}

#' @rdname elements
#' @export 
elementInterval <- function(
  ..., 
  shape = c(
    "rect",
    "hollow-rect",
    "line",
    "tick",
    "funnel",
    "pyramid"
  ), 
  state = c(
    "default",
    "active",
    "inactive",
    "selected"
  )
){
  shape <- match.arg(shape)
  element(..., state = state, shape = shape, figure = "interval")
}

#' @rdname elements
#' @export 
elementPolygon <- function(
  ..., 
  state = c(
    "default",
    "active",
    "inactive",
    "selected"
  )
){
  element(..., state = state, shape = "polygon", figure = "polygon")
}

#' @rdname elements
#' @export 
elementSchema <- function(
  ..., 
  shape = c(
    "box",
    "candle"
  ),
  state = c(
    "default",
    "active",
    "inactive",
    "selected"
  )
){
  shape <- match.arg(shape)
  element(..., state = state, shape = shape, figure = "schema")
}

#' Make geoms
#' 
#' Make geometries structure for theme.
#' 
#' @param theme Base theme object.
#' @param geoms List of geoms as returned by [get_elements()].
#' 
#' @keywords internal
make_geoms <- function(theme, geoms){
  if(!length(geoms))
    return(theme)

  for(i in 1:length(geoms)){
    geom <- geoms[[i]]
    theme$geometries[[geom$figure]][[geom$shape]][[geom$state]]$style <- geom$opts 
  }
  
  theme
}

#' @export
print.element <- function(x, ...){
  cat(
    "Element changing:\n",
    "Figure:", x$figure, "\n",
    "Shape:", x$shape, "\n",
    "State:", x$state
  )
}

#' Default Shape
#' 
#' Selects a default shape given a figure.
#' 
#' @param figure Figure.
#' 
#' @keywords internal
default_shape <- function(figure){
  switch(
    figure,
    point = "hollow-circle",
    area = "area",
    edge = "line",
    line = "line",
    interval = "rect",
    polygon = "polygon",
    schema = "box"
  )
}

#' Element Check
#' 
#' Check whether the object is an [element()].
#' 
#' @param obj An object to check.
#' 
#' @return If [element()] returns `TRUE`,
#' otherwise returns `FALSE`. 
#' 
#' @keywords internal
is_element <- function(obj){
  if(inherits(obj, "element"))
    return(TRUE)
  FALSE
}

#' Get or Remove Elements
#' 
#' Get or remove elements from three dot constructs.
#' 
#' @param ... Passed from [motif()].
#' 
#' @keywords internal
get_elements <- function(...){
  list(...) %>% 
    keep(is_element)
}

#' @keywords internal
rm_elements <- function(...){
  list(...) %>% 
    discard(is_element)
}

#' Digits
#' 
#' Maximum number of digits to show on charts.
#' 
#' @param n Maximum number of digits.
#' 
#' @export
global_digits <- function(n = 16L){
  options(G2_DIGITS = n)
}

#' @keywords internal
get_global_digits <- function(){
  getOption("G2_DIGTIS", 4)
}

# default chart options
# required for htmlwidgets
DEFAULT_CHART_OPTS <- list(
  padding = "auto",
  autoFit = TRUE
)

#' Get chart options
#' 
#' We don't currently provide a way to set the theme
#' globally but it'll be useful then.
#' 
#' @keywords internal
get_global_chart_opts <- function(){
  getOption("G2_CHART_OPTS", DEFAULT_CHART_OPTS)
}

#' @keywords internal
get_global_theme <- function(){
  getOption("G2_THEME", NULL)
}
