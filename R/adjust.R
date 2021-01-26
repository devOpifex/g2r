#' Adjust
#'
#' Adjust a figure.
#'
#' @param type A vector of types of adjustement to apply
#' to the figure, see the "types" section below for valid values.
#' @param margin Margin, between `0` and `1`.
#' @param dodge_by Bare column name to use as group for dodge.
#'
#' @section Types:
#' Valid values for the \code{type} argument.
#'
#' - `stack`
#' - `dodge`
#' - `jitter`
#' - `symmetric`
#'
#' @examples
#' df <- data.frame(
#'   x = c(letters, letters),
#'   y = runif(52),
#'   grp = rep(c("A", "Z"), each = 2)
#' )
#'
#' g2(df, asp(x, y, color = grp)) %>%
#'   fig_interval(adjust("stack"))
#' @importFrom rlang enquo quo_is_null quo_text
#'
#' @export
adjust <- function(type, margin = NULL, dodge_by = NULL) {
  if (missing(type)) {
    stop("missing type")
  }

  validity <- sum(type %in% c("stack", "dodge", "jitter", "symmetric"))

  if (!identical(validity, length(type))) {
    stop("invalid type specified")
  }

  dodge_enquo <- enquo(dodge_by)

  options <- list(type = type)
  if (!is.null(margin)) {
    options$marginRatio <- margin
  }
  if (!quo_is_null(dodge_enquo)) {
    options$dodgeBy <- quo_text(dodge_enquo)
  }

  structure(options, class = c("adjust", class(options)))
}

#' Adjust Check
#'
#' Checks whether the object is of class `adjust`,
#' as returned by [adjust()].
#'
#' @param x Object to check.
#'
#' @examples
#' \dontrun{
#' is_adjust(1)
#' is_adjust(adj("stack"))
#' }
#'
#' @return A boolean.
#'
#' @keywords internal
is_adjust <- function(x) {
  if (inherits(x, "adjust")) {
    return(TRUE)
  }
  FALSE
}

#' Get Adjust
#'
#' Get any [adjust()] from the three dot construct.
#'
#' @param ... Any argument.
#'
#' @examples
#' \dontrun{
#' foo <- function(...) {
#'   get_adjust(...)
#' }
#'
#' foo(1)
#' foo(adjust("stack"))
#' }
#'
#' @importFrom purrr keep
#'
#' @keywords internal
get_adjust <- function(...) {
  list(...) %>%
    keep(is_adjust) %>%
    unname()
}

#' @export
print.adjust <- function(x, ...) {
  cat("Adjust: ")
  cat(paste(x$type, collapse = "|"), "\n")
}
