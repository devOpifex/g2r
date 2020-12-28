#' Text
#' 
#' Add a point figure.
#' 
#' @param g An object of class `g2r` as returned by [g2()].
#' @param ... Options to pass to the informational annotation.
#' @param style A list of options defning the style.
#' @param data A dataset to use with [asp()].
#' 
#' @examples 
#' df <- head(cars, 5)
#' 
#' g2(cars, asp(speed, dist)) %>%
#'  fig_point() %>% 
#'  info_text(
#'    position = c(20, 35), content = "Look here!"
#'  ) %>% 
#'  info_text(
#'    asp(speed, dist),
#'    content = "Using aspects",
#'    data = df
#'  )
#' 
#' @seealso [Official annotation documentation](https://antv-g2.gitee.io/en/docs/api/general/annotation)
#' for defails pon what to pass to `...`.
#' 
#' @name info
#' @export 
info_text <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  UseMethod("info_text")
}

#' @method info_text g2r
#' @export 
info_text.g2r <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "text",
    style = style,
    data = data
  )
}

#' @rdname info
#' @export 
info_image <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  UseMethod("info_image")
}

#' @method info_image g2r
#' @export 
info_image.g2r <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "image",
    style = style,
    data = data
  )
}

#' @rdname info
#' @export 
info_arc <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  UseMethod("info_arc")
}

#' @method info_arc g2r
#' @export 
info_arc.g2r <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "arc",
    style = style,
    data = data
  )
}

#' @rdname info
#' @export 
info_line <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  UseMethod("info_line")
}

#' @method info_line g2r
#' @export 
info_line.g2r <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "line",
    style = style,
    data = data
  )
}

#' @rdname info
#' @export 
info_region <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  UseMethod("info_region")
}

#' @method info_region g2r
#' @export 
info_region.g2r <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "region",
    style = style,
    data = data
  )
}

#' @rdname info
#' @export 
info_region_filter <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  UseMethod("info_region_filter")
}

#' @method info_region_filter g2r
#' @export 
info_region_filter.g2r <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "regionFilter",
    style = style,
    data = data
  )
}

#' @rdname info
#' @export 
info_marker <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  UseMethod("info_marker")
}

#' @method info_marker g2r
#' @export 
info_marker.g2r <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "dataMarker",
    style = style,
    data = data
  )
}

#' @rdname info
#' @export 
info_data_region <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  UseMethod("info_data_region")
}

#' @method info_data_region g2r
#' @export 
info_data_region.g2r <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "dataRegion",
    style = style,
    data = data
  )
}

#' @rdname info
#' @export 
info_shape <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  UseMethod("info_shape")
}

#' @method info_shape g2r
#' @export 
info_shape.g2r <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "shape",
    style = style,
    data = data
  )
}

#' @rdname info
#' @export 
info_html <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  UseMethod("info_html")
}

#' @method info_html g2r
#' @export 
info_html.g2r <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "html",
    style = style,
    data = data
  )
}

#' Info
#' 
#' Primitive to create informational annotations.
#' 
#' @inheritParams fig_point
#' @param type Type of annotation.
#' @param style Style list to pass to the annotation.
#' @param data Optional data.frame to use with the [asp()].
#' 
#' @keywords internal
info_primitive <- function(
  g, 
  ..., 
  type = c(
    "text",
    "image",
    "arc",
    "line",
    "region",
    "regionFilter",
    "dataMarker",
    "dataRegion",
    "shape",
    "html"
  ),
  style = NULL,
  data = NULL
){

  type <- match.arg(type)

  data <- get_data(g, data)

  # options without aspects
  opts <- rm_asp(...)

  # info aspects
  asp <- get_asp(...)

  # process aspects
  opts_asp <- info_aspects_data(asp, data = data)

  info <- list(
    style = style
  ) %>% 
    append(opts) %>% 
    drop_nulls()

  if(!is.null(opts_asp)){
    info <- lapply(opts_asp, function(opt, info){
      append(info, opt)
    }, info = info)
  } else {
    info <- list(info)
  }

  g$x$annotations <- append(g$x$annotations, info)

  g
}
