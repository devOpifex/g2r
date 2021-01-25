#' Initialise
#'
#' Initialise a chart.
#' 
#' @param data A data.frame or tibble containing data to chart,
#' an object of class `igraph`, an object of class `ts`, 
#' or as `crosstalk::sharedDataset`.
#' @param ... Aspects of the chart, see [asp()].
#' @param width,height Dimensions of the chart, accepts 
#' any valid CSS unit e.g.: `100%`, numerics are treated
#' as pixels, e.g.: `400` = `400px`.
#' @param elementId Valid CSS id attribute.
#' @param digits Maximum number of digits after the comma to show
#' on the chart.
#'
#' @examples 
#' g2(cars) %>% 
#'  fig_point(asp(speed, dist))
#' 
#' @import htmlwidgets
#'
#' @export
g2 <- function(
  data = NULL, 
  ..., 
  width = NULL, 
  height = NULL, 
  elementId = NULL,
  digits = NULL
) {
  UseMethod("g2")
}

#' @export
g2.default <- function(
  data = NULL, 
  ..., 
  width = NULL, 
  height = NULL, 
  elementId = NULL,
  digits = NULL
) {

  asp <- get_asp(...)

  if(is_asp(data)){
    asp <- data
    data <- NULL
  }

  x = list(
    data = to_g2r(data), # dataset
    main_asp = asp, # main aspects
    views = list(), # views | figures
    scale = list(), # chart.scale
    cols = c() # keep track of columns for filter
  )

  as_widget(x, width, height, elementId, digits)
}

#' @export
#' @method g2 data.frame
g2.data.frame <- function(
  data = NULL, 
  ..., 
  width = NULL, 
  height = NULL, 
  elementId = NULL,
  digits = NULL
) {

  asp <- get_asp(...)

  x = list(
    data = to_g2r(data), # dataset
    main_asp = asp, # main aspects
    views = list(), # views | figures
    scale = list(), # chart.scale
    cols = c() # keep track of columns for filter
  )

  as_widget(x, width, height, elementId, digits)
}

#' @export
#' @method g2 igraph
g2.igraph <- function(
  data = NULL, 
  ..., 
  width = NULL, 
  height = NULL, 
  elementId = NULL,
  digits = NULL
) {

  asp <- get_asp(...)

  x = list(
    graph = data,
    data = to_g2r(data), # dataset
    main_asp = asp, # main aspects
    views = list(), # views | figures
    scale = list(), # chart.scale
    cols = c() # keep track of columns for filter
  )

  as_widget(x, width, height, elementId, digits)
}

#' @export
#' @method g2 SharedData
g2.SharedData <- function(
  data = NULL, 
  ..., 
  width = NULL, 
  height = NULL, 
  elementId = NULL,
  digits = NULL
) {

  asp <- get_asp(...)

  key_col <- "CROSSTALK_KEYS"

  dataset <- data$origData()
  dataset[[key_col]] <- data$key()

  x = list(
    crosstalk_group = data$groupName(),
    data = dataset, # dataset
    main_asp = asp, # main aspects
    views = list(), # views | figures
    scale = list(), # chart.scale
    cols = c(key_col) # keep track of columns for filter
  )

  as_widget(x, width, height, elementId, digits)
}

#' Shiny Bindings
#'
#' Output and render functions for using g2r within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a g2r
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name g2r-shiny
#'
#' @export
g2Output <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'g2r', width, height, package = 'g2r')
}

#' @rdname g2r-shiny
#' @export
renderG2 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, g2Output, env, quoted = TRUE)
}
