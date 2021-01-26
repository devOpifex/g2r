#' Aliases
#'
#' Aliases an aspect, this changes the name of
#' the aspects when displayed in axis titles,
#' tooltips, labels, and other places.
#'
#' @inheritParams fig_point
#' @param asp Bare name of aspect to alias.
#' @param alias A string defining the alias.
#'
#' @examples
#' # see tooltip
#' g2(cars, asp(speed, dist)) %>%
#'   fig_point() %>%
#'   aka(dist, "SO FAR")
#' @inheritParams fig_point
#'
#' @export
aka <- function(g, asp, alias) UseMethod("aka")

#' @export
#' @method aka g2r
aka.g2r <- function(g, asp, alias) {
  asp <- rlang::as_label(rlang::enquo(asp))

  g$x$scale[[asp]][["alias"]] <- alias

  g
}
