#' Remove a figure
#' 
#' Remove a figure from the plot.
#' 
#' @inheritParams fig_point
#' @param index Index of figure to remove. Either
#' the numeric index of figure (layer number in order 
#' it was added to the visualisation), or the `id` of 
#' the figure as set by [config()] (see examples).
#' 
#' @examples 
#' g <- g2(mtcars, asp(qsec, wt)) %>% 
#'  fig_point(config(id = "myPoints")) %>% 
#'  fig_point(asp(y = drat)) 
#' 
#' # all figures
#' g
#' 
#' # remove figure
#' remove_figure(g, "myPoints")
#' 
#' library(shiny)
#' 
#' df <- data.frame(
#'  x = 1:100,
#'  y = runif(100),
#'  z = runif(100)
#' )
#' 
#' ui <- fluidPage(
#'  g2Output("plot"),
#'  actionButton("rm", "Randomly remove a figure")
#' )
#' 
#' server <- function(input, output){
#' 
#'  output$plot <- renderG2({
#'    g2(df, asp(x, y)) %>% 
#'      fig_point() %>% 
#'      fig_line(asp(y = z))
#'  })
#' 
#'  observeEvent(input$rm, {
#'    g2_proxy("plot") %>% 
#'      remove_figure(sample(1:2, 1))
#'  })
#' 
#' }
#' 
#' if(interactive())
#'  shinyApp(ui, server)
#' 
#' @export 
remove_figure <- function(g, index) UseMethod("remove_figure")

#' @method remove_figure g2r
#' @export
remove_figure.g2r <- function(g, index){
  if(missing(index))
    stop("Missing `index`", call. = FALSE)

  if(is.numeric(index))
    g$x$views[[index]] <- NULL
  
  if(is.character(index)) {
    pos <- map(g$x$views, function(x, id){
      !isTRUE(x$conf$id == id)
    }, id = index) %>% 
      unlist()

    g$x$views <- g$x$views[pos]
  }

  g
}

#' @method remove_figure g2Proxy
#' @export
remove_figure.g2Proxy <- function(g, index){
  if(missing(index))
    stop("Missing `index`", call. = FALSE)

  # R index to JavaScript
  if(is.numeric(index))
    index <- index - 1

  g$session$sendCustomMessage(
    "remove-figure", 
    list(
      id = g$x$id, 
      index = index
    )
  )

  invisible(g)
}