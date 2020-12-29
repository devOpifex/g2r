#' State
#' 
#' Customise the styles of figures given states 
#' (active or selected).
#' 
#' @param ... Key value pair passed to styles.
#' 
#' @examples 
#' g2(iris, asp(Sepal.Width, Sepal.Length)) %>%
#'  fig_point(
#'    selected(fill = "red")
#'  ) %>% 
#'  conf_interplay("element", "selected")
#' 
#' @name state
#' @export 
active <- function(...){
  state <- list(
    active = list(
      style = list(...)
    )
  )

  structure(state, class = c("state", class(state)))
}

#' @rdname state
#' @export 
selected <- function(...){
  state <- list(
    selected = list(
      style = list(...)
    )
  )

  structure(state, class = c("state", class(state)))
}

#' @export 
print.state <- function(x, ...){
  cat("State:", names(x), "\n")
}

#' State Check
#' 
#' Checks whether the object is of class `state`,
#' as returned by [selected()] and [active()].
#' 
#' @param x Object to check.
#' 
#' @examples 
#' \dontrun{
#' is_state(1)
#' is_state(selected(fill = "red"))
#' }
#' 
#' @return A boolean.
#' 
#' @keywords internal
is_state <- function(x){
  if(inherits(x, "state"))
    return(TRUE)
  FALSE
}

#' Get States
#' 
#' Get states from three dot construct
#' 
#' @param ... Three dot passed from parent from which 
#' to _retrieve_ states.
#' 
#' @examples 
#' \dontrun{
#' foo <- function(...){
#'  get_state(...)
#' }
#' 
#' foo(active(stroke = "red"))
#' }
#' 
#' @importFrom purrr keep flatten
#' 
#' @keywords internal
get_state <- function(...){
  list(...) %>% 
    keep(is_state) %>% 
    flatten()
}