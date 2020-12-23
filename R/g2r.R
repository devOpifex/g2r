#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
g2 <- function(data = NULL, ..., width = NULL, height = NULL, elementId = NULL) {

  asp <- get_asp(...)

  x = list(
    data = data, # dataset
    main_asp = asp, # main aspects
    views = list() # views // figures
  )

  attr(x, "TOJSON_ARGS") <- list(dataframe = "rows")

  # create widget
  htmlwidgets::createWidget(
    name = 'g2r',
    x,
    width = width,
    height = height,
    package = 'g2r',
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
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
