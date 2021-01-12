#' Convert to Tibble
#' 
#' Converts objects to `data.frame`.
#' 
#' @param data An object to convert.
#' 
#' @export
to_tib <- function(data) UseMethod("to_tib")

#' @export
to_tib.default <- function(data){
  return()
}

#' @export
#' @importFrom zoo as.Date
#' @importFrom stats time
#' @method to_tib ts
to_tib.ts <- function(data) {
  tibble(
    x = time(data) %>% as.Date(),
    y = as.vector(data)
  )
}
