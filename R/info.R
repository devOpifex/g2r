#' Text
#' 
#' Add a point figure.
#' 
#' @param g An object of class `g2r` as returned by [g2()].
#' @param ... Options to pass to the informational annotation.
#' @param style A list of options defning the style.
#' @param data A dataset to use with [asp()].
#' 
#' @details `info_vline`, and `info_hline` use the `x`, and `y`
#' [asp()] for placement.
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
#' g2(cars, asp(speed, dist)) %>%
#'  fig_point() %>% 
#'  info_vline(asp(x = 20)) %>% 
#'  info_hline(asp(y = 20))
#' 
#' @seealso [Official annotation documentation](https://antv-g2.gitee.io/en/docs/api/general/annotation)
#' for defails pon what to pass to `...`, and [asp()].
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
info_vline <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  UseMethod("info_vline")
}

#' @method info_vline g2r
#' @export 
info_vline.g2r <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  
  asp <- get_asp(...)

  if(length(asp)){
    asp$xend <- "min"
    asp$y <- rlang::quo_name(asp$x)
    asp$yend <- "max"
  }

  info_primitive(
    g, 
    ..., 
    type = "line",
    style = style,
    data = data,
    asp = asp
  )
}

#' @rdname info
#' @export 
info_hline <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  UseMethod("info_hline")
}

#' @method info_hline g2r
#' @export 
info_hline.g2r <- function(
  g, 
  ..., 
  style = NULL,
  data = NULL
){
  
  asp <- get_asp(...)

  if(length(asp)){
    y <- asp$y
    asp$x <- "min"
    asp$xend <- y
    asp$y <- "max"
    asp$yend <- rlang::quo_name(y)
  }

  info_primitive(
    g, 
    ..., 
    type = "line",
    style = style,
    data = data,
    asp = asp
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
#' @param asp Aspects, if `NULL` then the function retrieves them
#' from the `...`.
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
  data = NULL,
  asp = NULL
){

  type <- match.arg(type)

  # options without aspects
  opts <- rm_asp(...)

  # info aspects
  if(is.null(asp))
    asp <- get_asp(...)

  # process aspects
  opts_asp <- info_aspects_data(asp, data = data)

  # common options
  info_opts <- list(
    style = style
  ) %>% 
    append(opts) %>% 
    drop_nulls()

  # structure with type
  info <- list(type = type, opts = info_opts)

  if(!is.null(opts_asp)){
    info <- lapply(opts_asp, function(opt, info){
      info$opts <- append(info$opts, opt)
      return(info)
    }, info = info)
  } else {
    info <- list(info)
  }

  g$x$annotations <- append(g$x$annotations, info)

  g
}
