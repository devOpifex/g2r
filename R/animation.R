#' Animations
#'
#' Helper function to build animations.
#'
#' @examples
#' # create animation
#' anim <- Animation$
#'   new()$
#'   appear(
#'   duration = 2000,
#'   delay = 500
#' )
#'
#' iris %>%
#'   g2(asp(Sepal.Length, Sepal.Width, color = Species)) %>%
#'   fig_point(anim)
#' @name animation
#' @export
Animation <- R6::R6Class(
  "Animation",
  public = list(
    #' @details Animation to use on enter
    #' @param animation Name of animation; `clipIn`, `zoomIn`,
    #' `pathIn`, `scaleInY`, `scaleInX`, `fanIn`, or `fadeIn`.
    #' @param easing Name of easing function.
    #' @param delay,duration Delay and duration in milliseconds.
    enter = function(animation = NULL, easing = NULL, delay = NULL, duration = NULL) {
      private$.enter <- private$build_list(animation, easing, delay, duration)
      invisible(self)
    },
    #' @details Animation to use on leave
    #' @param animation Name of animation; `lineWidthOut`,
    #' `zoomOut`, `pathOut`, or `fadeOut`.
    #' @param easing Name of easing function.
    #' @param delay,duration Delay and duration in milliseconds.
    leave = function(animation = NULL, easing = NULL, delay = NULL, duration = NULL) {
      private$.leave <- private$build_list(animation, easing, delay, duration)
      invisible(self)
    },
    #' @details Animation to use on appear
    #' @param animation Name of animation; `clipIn`,
    #' `zoomIn`, `pathIn`, `scaleInY`, `scaleInX`, `fanIn`,
    #' or `fadeIn`.
    #' @param easing Name of easing function.
    #' @param delay,duration Delay and duration in milliseconds.
    appear = function(animation = NULL, easing = NULL, delay = NULL, duration = NULL) {
      private$.appear <- private$build_list(animation, easing, delay, duration)
      invisible(self)
    },
    #' @details Animation to use on appear
    #' @param animation Name of animation; `fadeIn`, or `fanIn`.
    #' @param easing Name of easing function.
    #' @param delay,duration Delay and duration in milliseconds.
    update = function(animation = NULL, easing = NULL, delay = NULL, duration = NULL) {
      private$.update <- private$build_list(animation, easing, delay, duration)
      invisible(self)
    },
    #' @details Print
    print = function() {
      print("A g2r figure animation")
      invisible(self)
    },
    #' @details Retrieve the animation list
    retrieve = function() {
      list(
        enter = private$.enter,
        leave = private$.leave,
        appear = private$.appear,
        update = private$.update
      ) %>%
        drop_nulls()
    }
  ),
  private = list(
    .enter = NULL,
    .leave = NULL,
    .appear = NULL,
    .update = NULL,
    build_list = function(animation = NULL, easing = NULL, delay = NULL, duration = NULL) {
      anim <- list()

      if (!is.null(animation)) {
        anim$animation <- animation
      }

      if (!is.null(easing)) {
        anim$easing <- easing
      }

      if (!is.null(delay)) {
        anim$delay <- delay
      }

      if (!is.null(duration)) {
        anim$duration <- duration
      }

      return(anim)
    }
  )
)

#' New Animation
#'
#' Convenience function to create a new animation,
#' equivalent to `Animation$new()`.
#'
#' @seealso [Animation]
#'
#' @export
new_animation <- function() {
  Animation$new()
}

# is animation (to keep)
is_animation <- function(x) {
  if (inherits(x, "Animation")) {
    return(TRUE)
  }
  return(FALSE)
}

# retrieve animation
get_animation <- function(...) {
  anim <- list(...) %>%
    keep(is_animation)

  if (length(anim)) {
    anim[[1]]$retrieve()
  } else {
    NULL
  }
}
