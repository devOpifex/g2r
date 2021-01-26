#' Remove NULLs
#'
#' @param x A `list`.
#'
#' @examples
#' \dontrun{
#' drop_nulls(list(NULL, 1))
#' }
#'
#' @importFrom purrr keep
#' @keywords internal
drop_nulls <- function(x) {
  keep(x, function(x) {
    length(x) > 0
  })
}

#' Select Columns
#'
#' Select specific columns from a data.frame, if
#' they exist.
#'
#' @param data A data.frame.
#' @param cols Column names to select.
#'
#' @examples
#' \dontrun{
#' select_columns(cars, c("notExist", "speed"))
#' }
#'
#' @keywords internal
select_columns <- function(data = NULL, cols) {
  if (is.null(data)) {
    return()
  }

  data <- to_g2r(data)

  if (!inherits(data, "data.frame")) {
    return(data)
  }

  data[, names(data) %in% cols]
}

#' Pass an action bound to an aspect
#'
#' @section Uses:
#'
#' - `legend_asps`
#' - `axis_asps`
#'
#' @keywords internal
aspect_action <- function(g, asps, ..., action) {
  if (missing(asps)) {
    stop("Missing `asps`", call. = FALSE)
  }

  if (missing(action)) {
    stop("Missing `action`", call. = FALSE)
  }

  asps <- unique(asps)

  item <- lapply(asps, function(c, opts) {

    # can be chart.action(false)
    if (is.logical(opts[[1]])) {
      opts <- opts[[1]]
    }

    list(
      column = c,
      opts = opts
    )
  }, opts = list(...))

  g$x[[action]] <- append(g$x[[action]], item)

  g
}

#' Select Aspects from Data
#'
#' Select aspects from the data for info.
#'
#' @param asp Aspects as returned by [asp()].
#' @param data Data.frame.
#'
#' @importFrom purrr keep discard
#' @importFrom rlang as_label is_quosure
#'
#' @keywords internal
info_aspects_data <- function(asp, data = NULL) {
  if (!length(asp)) {
    return()
  }

  data <- as_tib(data)

  # only keep columns, remove constants
  asp_keep <- keep(asp, is_quosure)

  # columns as labels
  asp_keep_label <- sapply(asp_keep, as_label)

  if (inherits(data, "data.frame") && length(asp_keep_label)) {
    data <- data[, asp_keep_label]
    names(data) <- names(asp_keep)
  }

  # get constants, remove columns
  add_data <- data.frame()
  asp <- discard(asp, is_quosure)
  if (length(asp)) {
    add_data <- sapply(asp, asp_as_string)
    names(add_data) <- names(asp)
    add_data <- as.data.frame(t(add_data))
  }

  if (!is.null(data) && nrow(add_data)) {
    data <- cbind.data.frame(data, add_data)
  } else if (is.null(data) && nrow(add_data)) {
    data <- add_data
  }

  rehsape_data(data)
}


#' Reshape Data
#'
#' Reshape data to what g2.js expects.
#'
#' @param data A data.frame.
#'
#' @importFrom purrr pmap map
#'
#' @keywords internal
rehsape_data <- function(data) {
  data %>%
    pmap(list) %>%
    map(function(x) {
      pos <- x[names(x) %in% c("x", "y")]
      if (length(pos) == 2) x$position <- list(pos[["x"]], pos[["y"]])

      if (length(pos) == 2) x$start <- list(pos[["x"]], pos[["y"]])

      nd <- x[names(x) %in% c("xend", "yend")]
      if (length(nd) == 2) x$end <- list(nd[["xend"]], nd[["yend"]])

      x$x <- NULL
      x$y <- NULL
      x$xend <- NULL
      x$yend <- NULL

      return(x)
    })
}

# Figure types that SHOULD NOT be reordered
NO_REORDER_TYPES <- c("path", "polygon", "edge", "heatmap")

#' Order Data
#'
#' Fortunately or unfortunately G2.js does not reorder
#' the data and uses it as-is.
#'
#' This function reorders along the `x` axis to make sure
#' most plots look correct, see the `NO_REORDER_TYPES` for
#' figures types that are excluded from reordering.
#'
#' @param data Data to reorder.
#' @param x Column on x axis.
#' @param color Color variable name.
#'
#' @keywords internal
order_data <- function(
  data = NULL,
  x = NULL,
  color = NULL
) {
  if (is.null(data)) {
    return()
  }

  if (!inherits(data, "data.frame")) {
    return(data)
  }

  if (!length(x) && !length(color)) {
    return(data)
  }

  # only reorder these
  valid_types <- c("numeric", "factor", "POSIXt", "Date")

  if (length(x) && !length(color)) {
    if (inherits(data[[x]], valid_types)) {
      return(data[order(data[[x]]), ])
    } else {
      return(data)
    }
  } else if (!length(x) && length(color)) {
    return(data[order(data[[color]]), ])
  } else if (length(x) && length(color)) {
    if (inherits(data[[x]], valid_types)) {
      return(data[order(data[[color]], data[[x]]), ])
    } else {
      return(data[order(data[[color]]), ])
    }
  }
}

#' Get Data
#'
#' @inheritParams fig_point
#' @param data A data.frame or `NULL`.
#'
#' @keywords internal
get_data <- function(g, data = NULL) {
  if (!is.null(data)) {
    return(to_g2r(data))
  }

  g$x$data
}

#' As Tibble
#'
#' Returns a tibble or `NULL`.
#'
#' @param data A data.frame, tibble or `NULL`.
#'
#' @keywords internal
as_tib <- function(data = NULL) {
  if (is.null(data)) {
    return()
  }

  if (inherits(data, "list")) {
    return(data)
  }

  if (!inherits(data, "matrix")) {
    data <- as.data.frame(data)
  }

  if (!inherits(data, "data.frame")) {
    return(data)
  }

  for (col in 1:ncol(data)) {
    if (is.factor(data[[col]])) {
      data[[col]] <- as.character(data[[col]])
    }
  }

  tibble::as_tibble(data)
}

#' Checks if Package is Installed
#'
#' Checks if a package is installed, stops if not.
#'
#' @param pkg Package to check.
#'
#' @noRd
#' @keywords internal
check_alter <- function() {
  has_it <- base::requireNamespace("alter", quietly = TRUE)

  msg <- "This function requires the {alter} package installed.\n remotes::install_github(\"devOpifex/alter\")"
  if (!has_it) {
    stop(msg, call. = FALSE)
  }
}

#' @noRd
#' @keywords internal
check_package <- function(pkg) {
  has_it <- base::requireNamespace(pkg, quietly = TRUE)

  msg <- sprintf(
    "This function requires {%s}: `install.packages('%s')`.\n",
    pkg,
    pkg
  )
  if (!has_it) {
    stop(msg, call. = FALSE)
  }
}

#' Create widget
#'
#' Create widget.
#'
#' @param x List of options.
#' @param width,height Dimensions of chart.
#' @param elementId Id of element to create.
#' @param dependencies Dependencies (for crosstalk).
#'
#' @importFrom crosstalk crosstalkLibs
#'
#' @keywords internal
as_widget <- function(
  x,
  width = NULL,
  height = NULL,
  elementId = NULL,
  digits = NULL
) {

  # if NULL get global
  # allows overriding for individual charts
  if (is.null(digits)) {
    digits <- get_global_digits()
  }

  x$actions <- list()

  attr(x, "TOJSON_ARGS") <- list(
    dataframe = "rows",
    digits = digits
  )

  createWidget(
    name = "g2r",
    x,
    width = width,
    height = height,
    package = "g2r",
    elementId = elementId,
    preRenderHook = pre_render,
    dependencies = crosstalkLibs(),
    sizingPolicy = sizingPolicy(
      defaultWidth = "100%",
      browser.fill = TRUE,
      padding = 5,
      knitr.defaultWidth = "100%"
    )
  )
}
