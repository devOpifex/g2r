#' Text
#' 
#' Add a point figure.
#' 
#' @param g An object of class `g2r` as returned by [g2()].
#' @param ... Options to pass to the informational annotation.
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
  data = NULL
){
  UseMethod("info_text")
}

#' @method info_text g2r
#' @export 
info_text.g2r <- function(
  g, 
  ..., 
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "text",
    data = data
  )
}

#' @rdname info
#' @export 
info_image <- function(
  g, 
  ..., 
  data = NULL
){
  UseMethod("info_image")
}

#' @method info_image g2r
#' @export 
info_image.g2r <- function(
  g, 
  ..., 
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "image",
    data = data
  )
}

#' @rdname info
#' @export 
info_arc <- function(
  g, 
  ..., 
  data = NULL
){
  UseMethod("info_arc")
}

#' @method info_arc g2r
#' @export 
info_arc.g2r <- function(
  g, 
  ..., 
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "arc",
    data = data
  )
}

#' @rdname info
#' @export 
info_line <- function(
  g, 
  ..., 
  data = NULL
){
  UseMethod("info_line")
}

#' @method info_line g2r
#' @export 
info_line.g2r <- function(
  g, 
  ..., 
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "line",
    data = data
  )
}

#' @rdname info
#' @export 
info_vline <- function(
  g, 
  ..., 
  data = NULL
){
  UseMethod("info_vline")
}

#' @method info_vline g2r
#' @export 
info_vline.g2r <- function(
  g, 
  ..., 
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
    data = data,
    asp = asp
  )
}

#' @rdname info
#' @export 
info_hline <- function(
  g, 
  ..., 
  data = NULL
){
  UseMethod("info_hline")
}

#' @method info_hline g2r
#' @export 
info_hline.g2r <- function(
  g, 
  ..., 
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
    data = data,
    asp = asp
  )
}

#' @rdname info
#' @export 
info_region <- function(
  g, 
  ..., 
  data = NULL
){
  UseMethod("info_region")
}

#' @method info_region g2r
#' @export 
info_region.g2r <- function(
  g, 
  ..., 
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "region",
    data = data
  )
}

#' @rdname info
#' @export 
info_region_filter <- function(
  g, 
  ..., 
  data = NULL
){
  UseMethod("info_region_filter")
}

#' @method info_region_filter g2r
#' @export 
info_region_filter.g2r <- function(
  g, 
  ..., 
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "regionFilter",
    data = data
  )
}

#' @rdname info
#' @export 
info_marker <- function(
  g, 
  ..., 
  data = NULL
){
  UseMethod("info_marker")
}

#' @method info_marker g2r
#' @export 
info_marker.g2r <- function(
  g, 
  ..., 
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "dataMarker",
    data = data
  )
}

#' @rdname info
#' @export 
info_data_region <- function(
  g, 
  ..., 
  data = NULL
){
  UseMethod("info_data_region")
}

#' @method info_data_region g2r
#' @export 
info_data_region.g2r <- function(
  g, 
  ..., 
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "dataRegion",
    data = data
  )
}

#' @rdname info
#' @export 
info_shape <- function(
  g, 
  ..., 
  data = NULL
){
  UseMethod("info_shape")
}

#' @method info_shape g2r
#' @export 
info_shape.g2r <- function(
  g, 
  ..., 
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "shape",
    data = data
  )
}

#' @rdname info
#' @export 
info_html <- function(
  g, 
  ..., 
  data = NULL
){
  UseMethod("info_html")
}

#' @method info_html g2r
#' @export 
info_html.g2r <- function(
  g, 
  ..., 
  data = NULL
){
  info_primitive(
    g, 
    ..., 
    type = "html",
    data = data
  )
}

#' Info
#' 
#' Primitive to create informational annotations.
#' 
#' @inheritParams fig_point
#' @param type Type of annotation.
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
  data = NULL,
  asp = NULL
){

  type <- match.arg(type)

  # info aspects
  if(is.null(asp))
    asp <- get_asp(...)

  # process aspects
  opts_asp <- info_aspects_data(asp, data = data)

  # common options
  info_opts <- list(
    style = rm_asp(...)
  ) %>% 
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
