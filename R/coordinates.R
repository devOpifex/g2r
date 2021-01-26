#' Coordinates
#'
#' Configure chart coordinates axis.
#'
#' @inheritParams fig_point
#' @param type Type of coordinate axis.
#' @param angle Angle of axis rotation.
#' @param x,y Scale of axis along `x` and `y` axis.
#' @param axis Axis to reflect (reverse).
#' @param ... Any other options.
#'
#' @section Functions:
#'
#' - `coord_type`: Type of coordinates to use where `rect`
#'  corresponds to cartesian.
#' - `coord_rotate`: Rotate the coordinates by a certain angle.
#' - `coord_scale`: Rescale the coordinates.
#' - `coord_reflect`: Mirror the axis along the x, y, or xy (both)
#'  axes.
#' - `coord_transpose`: x, y axes displacement.
#'
#' @examples
#' g2(cars, asp(speed, dist, color = dist)) %>%
#'   fig_point() %>%
#'   coord_type("helix")
#' @name coord
#' @export
coord_type <- function(g, type = c("rect", "polar", "theta", "helix"), ...) {
  UseMethod("coord_type")
}

#' @method coord_type g2r
#' @export
coord_type.g2r <- function(g, type = c("rect", "polar", "theta", "helix"), ...) {
  g$x$coord <- list(
    type = match.arg(type),
    opts = list(...)
  )
  return(g)
}

#' @rdname coord
#' @export
coord_rotate <- function(g, angle = 90) UseMethod("coord_rotate")

#' @method coord_rotate g2r
#' @export
coord_rotate.g2r <- function(g, angle = 90) {
  if (!length(g$x$coord)) {
    g <- coord_type(g)
  }
  g$x$coordRotate <- angle
  return(g)
}

#' @rdname coord
#' @export
coord_scale <- function(g, x, y) UseMethod("coord_scale")

#' @method coord_scale g2r
#' @export
coord_scale.g2r <- function(g, x, y) {
  if (missing(x) || missing(y)) {
    stop("missing `x` or `y`", call. = FALSE)
  }

  if (!length(g$x$coord)) {
    g <- coord_type(g)
  }
  g$x$coordScale <- list(x, y)
  return(g)
}

#' @rdname coord
#' @export
coord_reflect <- function(g, axis = "xy") UseMethod("coord_reflect")

#' @method coord_reflect g2r
#' @export
coord_reflect.g2r <- function(g, axis = "xy") {
  if (!length(g$x$coord)) {
    g <- coord_type(g)
  }
  g$x$coordReflect <- axis
  return(g)
}

#' @rdname coord
#' @export
coord_transpose <- function(g) UseMethod("coord_transpose")

#' @method coord_transpose g2r
#' @export
coord_transpose.g2r <- function(g) {
  if (!length(g$x$coord)) {
    g <- coord_type(g)
  }
  g$x$coordTranspose <- TRUE
  return(g)
}
