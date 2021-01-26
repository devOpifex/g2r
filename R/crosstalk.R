#' Crosstalk Customisation
#'
#' Customise the crosstalk selection handle.
#'
#' @inheritParams fig_point
#' @param attribute Attribute to customise, e.g.: `stroke`, `fill`, `strokeOpacity`, etc.
#' @param on,off Value to set the `attribute` to if the selected is on or off.
#'
#' @export
crosstalk_select <- function(g, attribute, on, off = NULL) UseMethod("crosstalk_select")

#' @method crosstalk_select g2r
#' @export
crosstalk_select.g2r <- function(g, attribute, on, off = NULL) {
  if (missing(attribute)) {
    stop("Missing `attribute` argument", call. = FALSE)
  }

  if (missing(on)) {
    stop("Missing `on` argument", call. = FALSE)
  }

  if (is.null(on)) {
    on <- "none"
  }
  if (is.null(off)) {
    off <- "none"
  }

  opts <- list(
    attribute = attribute,
    on = on,
    off = off
  )

  g$x$crosstalk_select <- append(g$x$crosstalk_select, list(opts))

  g
}
