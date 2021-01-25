#' Aspects
#' 
#' Define aspects of a visualisation.
#' 
#' @param x,y Defines values to map to cartesian coordinates.
#' @param ... Any other key value pair of aspect.
#' 
#' @section Figure aspects:
#' 
#' - `x`, `y`
#' - `ymin`, `ymax`
#' - `size`
#' - `color`
#' - `shape`
#' - `label`
#' - `tooltip`
#' - `style`
#' - `interaction`
#' - `color`
#' 
#' @section Info aspects:
#' 
#' - `x`, `y`
#' - `start`, `end`
#' - `content`
#' - `top`
#' 
#' @importFrom rlang enquos is_quosure is_symbolic quo_is_symbolic quo_get_expr new_quosure quo_label
#' @importFrom purrr keep
#' 
#' @export
asp <- function(x, y, ...) {
  exprs <- enquos(x = x, y = y, ..., .ignore_empty = "all")
  asp <- new_asp(exprs, env = parent.frame())
  .construct_aspects(asp)
}

# construct aspect for re-use
.construct_aspects <- function(asp){
  structure(asp, class = c("aspects", class(asp)))
}

# Wrap symbolic objects in quosures but pull out constants out of
# quosures for backward-compatibility
new_aspect <- function(x, env = globalenv()) {
  if (is_quosure(x)) {
    if (!quo_is_symbolic(x)) {
      x <- quo_get_expr(x)
    }
    return(x)
  }

  if (is_symbolic(x)) {
    x <- new_quosure(x, env = env)
    return(x)
  }

  x
}

# create a new aspect
new_asp <- function(x, env = globalenv()) {
  stopifnot(is.list(x))
  x <- lapply(x, new_aspect, env = env)
  structure(x, class = c("unevaluated", class(x)))
}

#' @export
print.unevaluated <- function(x, ...) {
  cat("Aspects: \n")

  if (length(x) == 0) {
    cat("<empty>\n")
  } else {
    values <- vapply(x, quo_label, character(1))
    bullets <- paste0("* ", format(paste0("`", names(x), "`")), " -> ", values, "\n")

    cat(bullets, sep = "")
  }

  invisible(x)
}

#' @export
"[.unevaluated" <- function(x, i, ...) {
  new_asp(NextMethod())
}

# If necessary coerce replacements to quosures for compatibility
#' @export
"[[<-.unevaluated" <- function(x, i, value) {
  new_asp(NextMethod())
}
#' @export
"$<-.unevaluated" <- function(x, i, value) {
  # Can't use NextMethod() because of a bug in R 3.1
  x <- unclass(x)
  x[[i]] <- value
  new_asp(x)
}
#' @export
"[<-.unevaluated" <- function(x, i, value) {
  new_asp(NextMethod())
}

#' Aspect Check
#' 
#' Checks whether the object is of class `aspects`,
#' as returned by [asp()].
#' 
#' @param x Object to check.
#' 
#' @examples 
#' \dontrun{
#' is_asp(1)
#' is_asp(asp(dist))
#' }
#' 
#' @return A boolean.
#' 
#' @keywords internal
is_asp <- function(x){
  if(inherits(x, "aspects"))
    return(TRUE)
  FALSE
}

#' Get Aspects
#' 
#' Get aspects from three dot construct
#' 
#' @param ... Three dot passed from parent from which to _retrieve_ aspects.
#' 
#' @examples 
#' \dontrun{
#' foo <- function(...){
#'  get_asp(...)
#' }
#' 
#' foo(asp(speed))
#' }
#' 
#' @importFrom purrr keep flatten
#' 
#' @keywords internal
get_asp <- function(...){
  list(...) %>% 
    keep(is_asp) %>% 
    flatten()
}

#' Discard Aspects
#' 
#' Given a arguments, remove [asp()], [state], and [Animation]. 
#' Used to remove aspects and only keep options that need
#' to make it to the serie.
#' 
#' @param ... Three dot passed from parent from which to _remove_ aspects.
#' 
#' @examples 
#' \dontrun{
#' foo <- function(...){
#'  rm_asp(...)
#' }
#' 
#' foo(asp(speed), x = 1)
#' }
#' 
#' @importFrom purrr discard
#' 
#' @keywords internal
rm_asp <- function(...){
  list(...) %>% 
    discard(is_asp) %>% 
    discard(is_state) %>%
    discard(is_config) %>% 
    discard(is_adjust) %>%  
    discard(is_animation) 
}

#' Select Specific Aspects
#' 
#' Select specific aspects from a list of aspects
#' 
#' @param asp Aspects as returned by [asp()].
#' @param ... Names of aspects to select.
#' 
#' @examples 
#' \dontrun{
#' a <- asp(x = speed)
#' 
#' select_asp(a, "x")
#' }
#' 
#' @importFrom rlang as_label is_symbolic
#' 
#' @keywords internal
select_asp <- function(asp, ...){
  # indices
  indices <- names(asp) %in% c(...)  

  # found
  asp[indices]
}

#' @keywords internal
select_asp_labels <- function(asp, ...){
  a <- asp %>% 
    select_asp(...) %>% 
    sapply(asp_as_string)

  # order x and y
  if(length(a))
    a <- a[order(names(a))] 
  
  unname(a)
}

#' Aspect as String
#' 
#' @param a Aspect.
#' 
#' @return Aspect as string.
#' 
#' @importFrom rlang is_symbolic is_double is_integer
#' 
#' @keywords internal
asp_as_string <- function(a){
  if(is_symbolic(a))
    return(as_label(a))

  if(is_double(a))
    return(a)

  if(is_integer(a))
    return(a)

  return(a)
}

#' Has Aspects
#' 
#' Check whether the list contains aspects. This is
#' because functions to select or retrieve aspects
#' return lists of length 0 when none are found.
#' 
#' @examples 
#' \dontrun{
#' has_asp(asp())
#' has_asp(asp(dist))
#' }
#' 
#' @keywords internal
has_asp <- function(asp){
  length(asp) > 0
}

#' Combine Aspects
#' 
#' Combine aspects, generally used to combine parents with serie-level
#' aspects.
#' 
#' @param main_asp Main aspects as returned by [asp()].
#' @param asp Aspects passed to the serie, as returned by [asp()].
#' @param inherit_asp Whether the main aspects should be inherited/combined
#' 
#' @examples 
#' \dontrun{
#' combine_asp(
#'  asp(dist),
#'  asp(y = speed)
#' )
#' }
#' 
#' @keywords internal
combine_asp <- function(main_asp, asp, inherit_asp = TRUE){
  if(!has_asp(main_asp) && !has_asp(asp))
    return(list())

  if(!has_asp(asp))
    return(main_asp)

  if(inherit_asp){
    for(i in 1:length(asp)){
      c <- names(asp)[[i]]
      
      if(!c %in% c("tooltip", "label"))
        main_asp[[c]] <- asp[[i]]
      else 
        main_asp <- append(main_asp, asp[i])
    }
  } else {
    main_asp <- asp
  }

  main_asp
}

#' Get column names
#' 
#' @importFrom purrr map
#' 
#' @keywords internal
get_aspect_names <- function(g, aspect, index = NULL){
  g$x$views %>% 
    map(aspect) %>% 
    map(function(x, i){

      if(is.null(x))
        return()
      
      # split the collapsed aspects x*y
      spl <- strsplit(x[[1]], split = "\\*")[[1]]

      if(!is.null(i))
        spl <- spl[i]

      return(spl)
    }, i = index) %>% 
    unlist()
}

#' Collapse Aspects
#' 
#' Collapse aspects on a single line. Only `position`
#' works with a vector/array (`["x", "y"]`), hence enforcement
#' of `x*y` everywhere.
#' 
#' @param asp Aspects, a character vector to collapse
#' 
#' @examples
#' \dontrun{
#' collapse_asp(c("x", "y"))
#' } 
#' 
#' @keywords internal
collapse_asp <- function(asp){
  if(length(asp) == 0)
    return()

  if(length(asp) == 1)
    return(asp)
  
  paste0(asp, collapse = "*")
}

#' Combined Aspects
#' 
#' Retrieve the combined main and figure-level aspects.
#' 
#' @inheritParams fig_point
#' @param ... Three dots from parents (containing aspects).
#' @param inherits_asp Whether the main aspects are inherited.
#' 
#' @keywords internal
get_combined_asp <- function(g, ..., inherit_asp = FALSE){
  asp <- get_asp(...)
  combine_asp(g$x$main_asp, asp, inherit_asp = inherit_asp)
}