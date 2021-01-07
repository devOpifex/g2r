#' Events
#' 
#' Capture events in shiny.
#' 
#' @inheritParams fig_point
#' @param event Name of event to trigger the callback.
#' @param callback A callback function to run when the `event`
#' is fired, if `NULL` then a default one is created, see
#' details.
#' @param when When the event should be triggered.
#' 
#' @details The `callback` function should accept a single 
#' argument; the event data. If no callback function is passed
#' (`NULL`) then one is generated. The generated callback function
#' sets a shiny input `<chartId>_<eventName>` with the event data.
#' 
#' @examples 
#' g2(iris, asp(Sepal.Width, Sepal.Length)) %>%
#'  fig_point() %>% 
#'  capture_event("point:click")
#' 
#' @export
capture_event <- function(
  g, 
  event, 
  callback = NULL,
  when = c("on", "once", "off")
){
  UseMethod("capture_event")
}

#' @method capture_event g2r
#' @export
capture_event.g2r <- function(
  g, 
  event, 
  callback = NULL,
  when = c("on", "once", "off")
){
  if(missing(event))
    stop("Missing `event`", call. = FALSE)

  if(!is.null(callback) && !inherits(callback, "JS_EVAL"))
    stop("`callback` must be either `NULL` or a JavaScript function", call. = FALSE)

  opts <- list(
    when = match.arg(when),
    event = event,
    callback = callback
  )

  g$x$events <- append(g$x$events, list(opts))
  g
}
