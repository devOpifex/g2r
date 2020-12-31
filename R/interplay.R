#' Interplay
#' 
#' Configure global interplay (interactions) for the chart.
#' See [gauge_interplay()] to customise figure-level interplay.
#' 
#' @inheritParams fig_point
#' @param ... String(s) defining interactions.
#' @param name Name of interaction to register
#' 
#' @examples 
#' # global interaction on chart
#' df <- data.frame(
#'  x = letters,
#'  y = runif(26)
#' )
#' 
#' g2(df, asp(x, y)) %>%
#'  fig_interval(
#'    selected(fill = "orange")
#'  ) %>% 
#'  conf_interplay("element", "selected")
#' 
#' # brush
#' g2(cars, asp(speed, dist)) %>% 
#'  fig_point(asp(interplay = "brush"))
#' 
#' # register
#' df <- data.frame(
#'  x = c(letters, letters),
#'  y = runif(52),
#'  grp = c(rep("a", 26), rep("b", 26))
#' )
#' 
#' g2(df, asp(x, y, color = grp), elementId = "x") %>% 
#'  fig_interval(
#'    asp(interplay = "element-highlight-by-color"),
#'    adjust("dodge")
#'  ) %>%  
#'  register_interplay(
#'    "element-highlight-by-color",
#'      start = list(
#'        list(
#'          trigger = 'element:mouseenter', 
#'          action = 'element-highlight-by-color:highlight'
#'        )
#'      ),
#'      end = list(
#'        list(
#'          trigger = 'element:mouseleave', 
#'          action = 'element-highlight-by-color:reset'
#'        )
#'      )
#'    )
#' 
#' @name interplay
#' @export 
interplay <- function(g, ...) UseMethod("interplay")

#' @method interplay g2r
#' @export 
interplay.g2r <- function(g, ...){
  action <- paste0(c(...), collapse = "-")
  g$x$interactions <- append(g$x$interactions, list(action))
  g
}

#' @name interplay
#' @export 
remove_interplay <- function(g, ...) UseMethod("remove_interplay")

#' @method remove_interplay g2r
#' @export 
remove_interplay.g2r <- function(g, ...){
  action <- paste0(c(...), collapse = "-")
  g$x$rmInteractions <- append(g$x$rmInteractions, list(action))
  g
}

#' @name interplay
#' @export 
register_interplay <- function(g, name, ...) UseMethod("register_interplay")

#' @method register_interplay g2r
#' @export 
register_interplay.g2r <- function(g, name, ...){
  if(missing(name))
    stop("Missing `name`", call. = FALSE)

  opts <- list(
    name = name,
    opts = list(...)
  )
  g$x$registerInteractions <- append(
    g$x$registerInteractions, 
    list(opts)
  )
  g
}
