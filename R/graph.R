#' igraph to List
#' 
#' Returns a list of nodes and edges.
#' 
#' @param data An igraph object.
#' 
#' @return `list` of length 2.
#' 
#' @keywords internal
igraph_to_list <- function(data){

  # nodes and edges as data frame
  edges <- igraph::as_data_frame(data, what = "edges")
  nodes <- igraph::as_data_frame(data, what = "vertices")

  # rename for alter transform default
  names(edges)[1:2] <- c("source", "target")

  if(ncol(nodes) == 0)
    nodes <- data.frame(id = as.vector(igraph::V(data)))

  names(nodes)[1] <- "id"

  list(nodes = nodes, edges = edges)
}

#' Layout with igraph
#' 
#' Layout the graph using an igraph layout function.
#' This function only works with the graph was initialised
#' with an object of class `igraph`.
#' 
#' @inheritParams fig_point
#' @param ... Any options to pass to the `method` function.
#' @param method An igraph layout function to compute the
#' nodes and edges (source and target) position on the canvas.
#' 
#' @details The function runs the `method` to obtain the `x`
#' and `y` coordinates. These are added to the `nodes` 
#' data.frame (extracted from initial graph) and to the edges
#' data.frame, as `x` and `y` _nested_ columns, e.g.: 
#' `c(source_x, target_x)`. These `x` and `y` coordinates 
#' can be used in [asp()] (see example).
#' 
#' @examples 
#' ig <- igraph::make_ring(100)
#' 
#' # use x and y for positioning
#' g2(ig, asp(x, y)) %>%
#'  layout_igraph() %>% 
#'  fig_edge() %>% 
#'  fig_point(asp(shape = "circle")) %>% 
#'  axis_hide()
#' 
#' @importFrom purrr map
#' 
#' @export 
layout_igraph <- function(
  g, 
  ..., 
  method = igraph::layout_nicely
){
  check_package("igraph")
  UseMethod("layout_igraph")
}

#' @method layout_igraph g2r
#' @export 
layout_igraph.g2r <- function(
  g, 
  ..., 
  method = igraph::layout_nicely
) {

  if(is.null(g$x$graph))
    stop("Missing graph", call. = FALSE)

  # compute layout
  l <- g$x$graph %>% 
    method(...) %>% 
    as.data.frame()
  
  names(l) <- c("x", "y")

  # remove x and y then add 
  g$x$data$nodes[["x"]] <- g$x$data$nodes[["y"]] <- NULL
  g$x$data$nodes <- cbind.data.frame(g$x$data$nodes, l)

  # add source target
  tmp <- g$x$data$nodes[, c("id", "x", "y")]

  names(tmp) <- c("source", "source_x", "source_y")
  g$x$data$edges <- merge(
    x = g$x$data$edges, 
    y = tmp, 
    by = "source", 
    all.x = TRUE
  )

  names(tmp) <- c("target", "target_x", "target_y")
  g$x$data$edges <- merge(
    x = g$x$data$edges, 
    y = tmp, 
    by = "target", 
    all.x = TRUE
  )

  g$x$data$edges <- pmap(g$x$data$edges, list) %>% 
    map(function(edge){
      edge$x <- c(edge$source_x, edge$target_x)
      edge$y <- c(edge$source_y, edge$target_y)

      edge$source_x <- edge$target_x <- NULL
      edge$source_y <- edge$target_y <- NULL

      return(edge)
    })

  g
}

#' Layout Arc
#' 
#' Layout as arc using the alter package.
#' 
#' @inheritParams fig_point
#' @param thickness Node height, between `0` and `1`.
#' @param marginRatio Space ratio, between `0` and `1`.
#' 
#' @examples 
#' ig <- igraph::erdos.renyi.game(100, 1/100)
#' 
#' g2(ig, asp(x, y)) %>% 
#'  layout_arc() %>% 
#'  fig_edge(asp(color = source, shape = "arc"), opacity = .3) %>% 
#'  fig_point(asp(color = id, shape = "circle", size = value)) %>% 
#'  coord_type("polar") %>% 
#'  coord_reflect("y") %>% 
#'  axis_hide()
#' 
#' g2(ig, asp(x, y)) %>% 
#'  layout_arc() %>% 
#'  fig_edge(asp(color = source, shape = "arc"), opacity = .3) %>% 
#'  fig_point(asp(color = id, shape = "circle", size = value))
#' 
#' @export 
layout_arc <- function(
  g,
  thickness = .05,
  marginRatio = .1
){
  check_alter()
  UseMethod("layout_arc")
}

#' @method layout_arc g2r
#' @export 
layout_arc.g2r <- function(
  g,
  thickness = .05,
  marginRatio = .1
) {

  alt <- alter::Alter$new(g$x$data)$
    source(
      type = "graph"
    )$
    transform(
      type = "diagram.arc",
      y = 0,
      thickness = thickness,
      marginRatio = marginRatio
    )

  g$x$data$edges <- alt$get("edges", clean = FALSE)
  g$x$data$nodes <- alt$get("nodes")

  g
}

# #' @export 
# layout_sankey <- function(
#   g,
#   nodeAlign = c(
#     "sankeyJustify",
#     "sankeyLeft",
#     "sankeyRight",
#     "sankeyCenter"
#   ),
#   nodeWidth = .2,
#   nodePadding = .02
# ){
#   check_alter()
#   UseMethod("layout_sankey")
# }

# #' @method layout_sankey g2r
# #' @export 
# layout_sankey.g2r <- function(
#   g,
#   nodeAlign = c(
#     "sankeyJustify",
#     "sankeyLeft",
#     "sankeyRight",
#     "sankeyCenter"
#   ),
#   nodeWidth = .2,
#   nodePadding = .02
# ){

#   node_align <- match.arg(nodeAlign)

#   alt <- alter::Alter$new(g$x$data)$
#     source(
#       type = "graph"
#     )$
#     transform(
#       type = "diagram.sankey",
#       nodeAlign = nodeAlign,
#       nodeWidth = nodeWidth,
#       nodePadding = nodePadding
#     )

#   g$x$data$edges <- alt$get("edges", clean = FALSE)
#   g$x$data$nodes <- alt$get("nodes")

#   g
# }