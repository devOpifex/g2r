#' Planes
#' 
#' Split the chart into planes according to variables.
#' 
#' @inheritParams fig_point
#' @param asp Aspects that define split.
#' @param ... Any other option.
#' @param type Type of planes to use.
#' 
#' @examples 
#' g2(iris, asp(Sepal.Length, Sepal.Width, color = Species)) %>% 
#'  fig_point() %>% 
#'  planes("Species", type = "tree")
#' 
#' @importFrom rlang enquos as_label
#' @importFrom purrr map
#' 
#' @export 
planes <- function(
  g,
  asp,
  ...,
  type = c(
    "rect",
    "list",
    "matrix",
    "circle",
    "tree"
  )
){
  UseMethod("planes")
}

#' @method planes g2r
#' @export 
planes.g2r <- function(
  g,
  asp,
  ...,
  type = c(
    "rect",
    "list",
    "matrix",
    "circle",
    "tree"
  )
){
  if(missing(asp))
    stop("Missing `asp`", call. = FALSE)
  
  if(is.null(g$x$data))
    stop("Planes requires data to be passed to `g2`", call. = FALSE)

  type <- match.arg(type)

  g$x$facet <- list(
    type = type, 
    opts = list(
      fields = as.list(asp),
      ...
    )
  )
  g
}