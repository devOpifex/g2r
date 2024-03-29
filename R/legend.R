#' Legend
#'
#' Customise the legend.
#'
#' @inheritParams fig_point
#' @param ... Options to pass to the legend, pass `FALSE`
#' to hide the axis. Visit the
#' [official documentation](https://g2.antv.vision/en/docs/api/general/legend/)
#' for the full list of options.
#' @param asps Aspect (column names) to change the legend.
#'
#' @section Functions:
#'
#' - `legend_color`: Customise the x axis.
#' - `legend_size`: Customise the y axis.
#' - `legend_asps`: Customise the axis by aspects (column names).
#'
#' @examples
#' g <- g2(mtcars, asp(mpg, qsec, color = gear)) %>%
#'   fig_point()
#'
#' g %>% legend_color(position = "top")
#' g %>% legend_color(FALSE)
#' @name axis
#' @export
legend_color <- function(g, ...) UseMethod("legend_color")

#' @method legend_color g2r
#' @export
legend_color.g2r <- function(g, ...) {
  cols <- get_aspect_names(g, "color")
  legend_asps(g, cols, ...)
}

#' @rdname axis
#' @export
legend_size <- function(g, ...) UseMethod("legend_size")

#' @method legend_size g2r
#' @export
legend_size.g2r <- function(g, ...) {
  cols <- get_aspect_names(g, "size")
  legend_asps(g, cols, ...)
}

#' @rdname axis
#' @export
legend_asps <- function(g, asps, ...) UseMethod("legend_asps")

#' @method legend_asps g2r
#' @export
legend_asps.g2r <- function(g, asps, ...) {
  aspect_action(g, asps, ..., action = "legend")
}
