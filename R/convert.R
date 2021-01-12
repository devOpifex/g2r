#' Convert to Tibble
#' 
#' Converts objects to `data.frame`.
#' 
#' @param data An object to convert.
#' 
#' @export
to_tib <- function(data = NULL) UseMethod("to_tib")

#' @export
to_tib.default <- function(data = NULL){
  as_tib(data)
}

#' @export
#' @importFrom zoo as.Date
#' @importFrom stats time
#' @method to_tib ts
to_tib.ts <- function(data = NULL) {
  tibble(
    x = time(data) %>% as.Date(),
    y = as.vector(data)
  )
}

#' @export
#' @method to_tib data.frame
to_tib.data.frame <- function(data = NULL) {
  as_tib(data)
}

#' @export
#' @method to_tib igraph
to_tib.igraph <- function(data = NULL){
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

#' @export
#' @method to_tib forecast
to_tib.forecast <- function(data = NULL) {
  x <- to_tib(data$x)
  mean <- to_tib(data$mean)
  lower <- to_tib(data$lower)
  upper <- to_tib(data$upper)
  
  names(mean) <- c("x", "mean")
  names(lower) <- c("x", "lower")
  names(upper) <- c("x", "upper")

  base <- merge(x, mean, by = "x", all = TRUE)
  bands <- merge(lower, upper, by = "x", all = TRUE)
  all <- merge(base, bands, by = "x", all = TRUE)
  
}
