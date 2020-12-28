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

  x = list(
    chartOpts = list(
      autoFit = TRUE,
      theme = "light"
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
      browser.fill = TRUE
    ) 
  )
}

#' Shiny bindings for g2r
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

#' Shiny Proxy
#' 
#' Proxy to dynamically interact with the chart in shiny.
#' 
#' @param id Id of chart to interact with.
#' @param ... Aspects, see [asp()].
#' @param data Data.frame containing data to plot.
#' @param session A valid shiny session.
#' 
#' @export
g2_proxy <- function(
  id, 
  ..., 
  data = NULL, 
  session = shiny::getDefaultReactiveDomain()
){

  if(missing(id))
    stop("Missing `id`", call. = FALSE)

  proxy <- list(
    id = id, 
    session = session, 
    data = data,
    main_asp = get_asp(...)
  )
  
  structure(proxy, class = c("g2Proxy", class(proxy)))
}

#' @export 
print.g2Proxy <- function(x, ...){
  cat("Proxy for chart `", x$id, "`\n")
}
