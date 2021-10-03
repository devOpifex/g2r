#' Configure Figure
#'
#' Configure a figure.
#'
#' @param id Id of figure.
#' @param visible Whether the figure is visible.
#' @param region Region that the figure should occupy
#' on the canvas.
#' @param padding Padding around the figure.
#' @param theme Theme of the figure.
#' @param ... Any other options from the
#' [official documentation](https://g2.antv.vision/en/docs/api/general/chart/).
#'
#' @examples
#' mtcars %>%
#'   g2(asp(qsec)) %>%
#'   fig_point(
#'     asp(y = mpg),
#'     config(
#'       region = list(
#'         start = list(x = 0, y = 0),
#'         end = list(x = 0.5, y = 1)
#'       )
#'     )
#'   ) %>%
#'   fig_point(
#'     asp(y = wt),
#'     config(
#'       region = list(
#'         start = list(x = 0.5, y = 0),
#'         end = list(x = 1, y = 1)
#'       )
#'     )
#'   )
#' @export
config <- function(
  id = NULL,
  visible = NULL,
  region = NULL,
  padding = NULL,
  theme = NULL,
  ...
) {
  conf <- list(
    id = id,
    visible = visible,
    region = region,
    padding = padding,
    theme = theme,
    ...
  ) %>%
    drop_nulls()

  structure(conf, class = c("config", class(conf)))
}

#' Config Check
#'
#' Checks whether the object is of class `config`,
#' as returned by [config()].
#'
#' @param x Object to check.
#'
#' @examples
#' \dontrun{
#' is_config(1)
#' is_config(conf("hello"))
#' }
#'
#' @return A boolean.
#'
#' @keywords internal
is_config <- function(x) {
  if (inherits(x, "config")) {
    return(TRUE)
  }
  FALSE
}

#' Get Config
#'
#' Get config from three dot construct
#'
#' @param ... Three dot passed from parent from which
#' to _retrieve_ config.
#'
#' @examples
#' \dontrun{
#' foo <- function(...) {
#'   get_conf(...)
#' }
#'
#' foo(conf("hello"))
#' }
#'
#' @importFrom purrr keep flatten
#'
#' @keywords internal
get_config <- function(...) {
  list(...) %>%
    keep(is_config) %>%
    flatten()
}

#' @export
print.config <- function(x, ...) {
  cat("Config\n")
}
