#' Plot Action
#'
#' Include dynamic elements in Rmarkdown.
#'
#' @param plot_id Id of chart to interact with.
#' @param btn_id Id of the [input_button()] that triggers the action.
#' @param ... Aspects, see [asp()].
#' @param data Data.frame containing data to plot.
#' @param reorder Whether to internally reorder the data, namely
#' the `x` and `color`. The `x` axis must be reordered in a descending
#' order for most data type since G2.js plots data as-is. Moreover,
#' `color` order of all data.frames passed either to this function or
#' subsequent `fig_*` layers must be identical or the colors will
#' might match the legends on the plot. However, one may sometimes
#' not want the data to be reordered.
#'
#' @export
g2_action <- function(
  plot_id,
  btn_id,
  ...,
  data = NULL,
  reorder = TRUE
) {
  if (missing(plot_id)) {
    stop("Missing `plot_id`", call. = FALSE)
  }

  if (missing(btn_id)) {
    stop("Missing `btn_id`", call. = FALSE)
  }

  action <- list(
    x = list(
      reorder = reorder,
      is_action = TRUE,
      id = plot_id,
      # id of chart
      input_id = btn_id,
      # id of input
      data = as_tib(data),
      # dataset
      main_asp = get_asp(...),
      # main aspects
      views = list(),
      # views | figures
      scale = list(),
      # chart.scale
      cols = c() # keep track of columns for filter
    )
  )

  structure(action, class = c("g2Action", "g2Proxy", class(action)))
}

#' @export
print.g2Action <- function(x, ...) {
  cat("Action for chart: ", x$id, "\n")
}


#' Toggle Visibility
#'
#' Toggle the visibily of a chart.
#'
#' @inheritParams fig_point
#' @param btn Id of the [input_button] that toggles
#' the visibility.
#'
#' @examples
#' # works in Rmarkdown
#' input_button("toggle", "Show/hide chart")
#'
#' g2(mtcars, asp(qsec, mpg)) %>%
#'   fig_point() %>%
#'   action_toggle_visibility("toggle")
#' @export
action_toggle_visibility <- function(g, btn) UseMethod("action_toggle_visibility")

#' @export
#' @method action_toggle_visibility g2r
action_toggle_visibility.g2r <- function(g, btn) {
  if (missing(btn)) {
    stop("Missing `btn`", call. = FALSE)
  }

  opts <- list(id = btn, name = "toggle-visibility")
  g$x$actions <- append(g$x$actions, list(opts))
  g
}

#' Select Data
#'
#' Select a dataset with an [input_select()].
#'
#' @inheritParams fig_point
#' @param input Id of the [input_select()] used to
#' choose the dataset.
#' @param datasets A key value pair `list` where the `key`
#' is the name of the dataset as listed in the `choices` of
#' the [input_select()].
#'
#' @examples
#' # works in Rmarkdown
#' input_select(
#'   "selector",
#'   "Select a dataset",
#'   c("Cars", "More Cars")
#' )
#'
#' cars1 <- cars
#' cars2 <- cars + c(1, -4)
#'
#' g2(cars, asp(dist, speed)) %>%
#'   fig_point() %>%
#'   action_select_data(
#'     "selector",
#'     datasets = list(
#'       "Cars" = cars1,
#'       "More Cars" = cars2
#'     )
#'   )
#' @export
action_select_data <- function(g, input, datasets) UseMethod("action_select_data")

#' @export
#' @method action_select_data g2r
action_select_data.g2r <- function(g, input, datasets) {
  if (missing(input)) {
    stop("Missing `input`", call. = FALSE)
  }

  if (missing(datasets)) {
    stop("Missing `datasets`", call. = FALSE)
  }

  opts <- list(
    id = input,
    name = "select-data",
    datasets = datasets
  )
  g$x$actions <- append(g$x$actions, list(opts))
  g
}

#' Filter Data
#'
#' Filter data.
#'
#' @inheritParams fig_point
#' @param input The `id` of the input that triggers the filter,
#' either [input_select()] or [input_slider()].
#' @param asp Aspect (column) to filter.
#' @param operator Operator of the filter, this is combined with
#' the value from the `input` and the `asp` to form a filter
#' statement with the following template; `asp operator inputValue`.
#' For instance, a filter on input `id = "theFilter"` on the column
#' `speed` (of the `cars` dataset) with the operator `>` (greater than)
#' will create the following filter statement: `speed > inputValue`
#'
#' @examples
#' # works in Rmarkdown
#' input_slider(
#'   "yFilter",
#'   "Filter Y >",
#'   value = 50,
#'   min = 40,
#'   max = 70,
#'   step = 5
#' )
#'
#' \dontrun{
#' g2(cars, asp(speed, dist)) %>%
#'   fig_point() %>%
#'   fig_smooth() %>%
#'   action_filter_data(
#'     "yFilter",
#'     dist,
#'     operator = ">"
#'   )
#' }
#' @importFrom rlang as_label enquo
#'
#' @export
action_filter_data <- function(
  g,
  input,
  asp,
  operator = ">"
) {
  UseMethod("action_filter_data")
}

#' @export
#' @method action_filter_data g2r
action_filter_data.g2r <- function(
  g,
  input,
  asp,
  operator = ">"
) {
  asp <- as_label(enquo(asp))

  if (!is.null(g$x$data)) {
    type <- typeof(g$x$data[[asp]])
  } else {
    type <- map(g$x$views, function(v, aspect) {
      typeof(v$data[[aspect]])
    }, aspect = asp) %>%
      unlist()
  }

  opts <- list(
    id = input,
    field = asp,
    name = "filter-data",
    op = operator,
    type = type[1]
  )

  g$x$cols <- c(g$x$cols, asp)
  g$x$actions <- append(g$x$actions, list(opts))

  g
}
