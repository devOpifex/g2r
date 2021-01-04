#' Remove a figure
#' 
#' Remove a figure from the plot.
#' 
#' @inheritParams fig_point
#' @param index Index of figure to remove.
#' 
#' @examples 
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

  g$x$views[[index]] <- NULL
  g
}

#' @method remove_figure g2Proxy
#' @export
remove_figure.g2Proxy <- function(g, index){
  if(missing(index))
    stop("Missing `index`", call. = FALSE)

  # R index to JavaScript
  index <- index - 1

  g$session$sendCustomMessage(
    "remove_figure", 
    list(
      id = g$x$id, 
      index = index
    )
  )

  invisible(g)
}