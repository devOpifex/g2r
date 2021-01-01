#' Initialise
#'
#' Initialise a chart.
#' 
#' @param data A data.frame or tibble containing data to chart.
#' @param ... Aspects of the chart, see [asp()].
#' @param width,height Dimensions of the chart, accepts 
#' any valid CSS unit e.g.: `100%`, numerics are treated
#' as pixels, e.g.: `400` = `400px`.
#' @param elementId Valid CSS id attribute.
#'
#' @examples 
#' g2(cars) %>% 
#'  fig_point(asp(speed, dist))
#' 
#' @import htmlwidgets
#'
#' @export
g2 <- function(data = NULL, ..., width = NULL, height = NULL, elementId = NULL) {

  asp <- get_asp(...)

  if(is_asp(data)){
    asp <- data
    data <- NULL
  }

  x = list(
    chartOpts = list(
      autoFit = TRUE,
      theme = "light",
      padding = "auto"
    ),
    data = as_tib(data), # dataset
    main_asp = asp, # main aspects
    views = list(), # views | figures
    scale = list(), # chart.scale
    cols = c() # keep track of columns for filter
  )

  attr(x, "TOJSON_ARGS") <- list(dataframe = "rows")

  # create widget
  createWidget(
    name = 'g2r',
    x,
    width = width,
    height = height,
    package = 'g2r',
    elementId = elementId,
    preRenderHook = renderG2,
    sizingPolicy = sizingPolicy(
      defaultWidth = "100%",
      browser.fill = TRUE,
      padding = 5,
      knitr.defaultWidth = "100%"
    ) 
  )
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
g2rOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'g2r', width, height, package = 'g2r')
}

#' @rdname g2r-shiny
#' @export
renderG2r <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, g2rOutput, env, quoted = TRUE)
}
