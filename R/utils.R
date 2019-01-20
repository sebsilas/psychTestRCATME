is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

tagify <- function(x) {
  stopifnot(is.character(x) || is(x, "shiny.tag"))
  if (is.character(x)) {
    stopifnot(is.scalar(x))
    shiny::p(x)
  } else x
}

is.null.or <- function(x, f) {
  is.null(x) || f(x)
}

#' Assert that a global variable is NULL
#'
#' This function checks whether a specified global variable
#' in a psychTestR state object is \code{NULL}.
#' If so, it throws an error.
#' @param key
#' (Character scalar)
#' Identifies the global variable to be checked.
#'
#' @param state
#' A psychTestR state object.
#'
#' @export
assert_global_is_null <- function(key, state) {
  stopifnot(is.scalar.character(key), is(state, "state"))
  if (is.null(psychTestR::get_global(key, state))) TRUE else {
    stop("global variable <", key, "> in <state> was not NULL")
  }
}

is.integerlike <- function(x) {
  all(round(x) == x)
}

is.scalar.integerlike <- function(x) {
  is.scalar(x) && is.integerlike(x)
}
