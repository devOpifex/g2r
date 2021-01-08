#' Shiny Proxy
#' 
#' Proxy to dynamically interact with the chart in shiny.
#' 
#' @param id Id of chart to interact with.
#' @param ... Aspects, see [asp()].
#' @param data Data.frame containing data to plot.
#' @param session A valid shiny session.
#' 
#' @examples 
#' library(shiny)
#' 
#' dataset <- data.frame(x = 1:100, y = runif(100, 1, 100))
#' 
#' ui <- fluidPage(
#'  g2Output("plot"),
#'  actionButton("add", "Add figure")
#' )
#' 
#' server <- function(input, output, session){
#' 
#'  output$plot <- renderG2({
#'    g2(dataset, asp(x, y)) %>% 
#'      fig_point()
#'  })
#' 
#'  observeEvent(input$add, {
#'    df <- data.frame(x = 1:100, y = runif(100, 1, 100))
#'    g2_proxy("plot", data = df) %>% 
#'      fig_point(asp(x, y)) %>% 
#'      render()
#'    })
#' 
#' }
#' 
#' if(interactive())
#'  shinyApp(ui, server)
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
    session = session, 
    x = list(
      id = id, # id of chart
      data = as_tib(data), # dataset
      main_asp = get_asp(...), # main aspects
      views = list(), # views | figures
      scale = list(), # chart.scale
      cols = c() # keep track of columns for filter
    )
  )
  
  structure(proxy, class = c("g2Proxy", class(proxy)))
}

#' @export 
print.g2Proxy <- function(x, ...){
  cat("Proxy for chart `", x$id, "`\n")
}

#' Render
#' 
#' Render proxy calls.
#' 
#' @param g An object of class `g2Proxy` as returned 
#' by [g2_proxy()].
#' @param update Whether to trigger the update process.
#' 
#' @export 
render <- function(g, update = TRUE) UseMethod("render")

#' @method render g2Proxy
#' @export 
render.g2Proxy <- function(g, update = TRUE){
  g$x$update <- update

  g$x$data <- select_columns(g$x$data, g$x$cols)
  g$x$data <- serialise_df(g$x$data)

  for(i in 1:length(g$x$views)){
    g$x$views[[i]]$data <- select_columns(g$x$views[[i]]$data, g$x$cols)
    g$x$views[[i]]$data <- serialise_df(g$x$views[[i]]$data)
  }

  g$session$sendCustomMessage("render", g$x)
  invisible(g)
}

#' @importFrom jsonlite toJSON
#' @method render g2Action
#' @export 
render.g2Action <- function(g, update = TRUE){
  check_package("htmltools")

  g$x$data <- select_columns(g$x$data, g$x$cols)
  g$x$data <- serialise_df(g$x$data)

  for(i in 1:length(g$x$views)){
    g$x$views[[i]]$data <- select_columns(g$x$views[[i]]$data, g$x$cols)
    g$x$views[[i]]$data <- serialise_df(g$x$views[[i]]$data)
  }

  htmltools::tags$script(
    `data-for` = g$x$id,
    class = "g2-actions",
    type = "application/json",
    toJSON(
      g$x, 
      data.frame = "rows",
      null = "null",
      auto_unbox = TRUE
    )
  )
}

#' Serialise
#' 
#' Serialise the data from a data frame to a rowwise
#' list. This is because proxies do not come with a
#' serialiser that can be adapted.
#' 
#' @param data A dataframe or `NULL`.
#' 
#' @importFrom purrr pmap
#' @keywords internal
serialise_df <- function(data = NULL){
  if(is.null(data))
    return()

  if(!inherits(data, "data.frame"))
    return(data)

  pmap(data, list)
}
