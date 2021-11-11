#' @title Debug selected functions
#'
#' @description
#' Applies debugonce to specified functions.
#'
#' @param fn_name character vector. Names of functions.
#' @param debug_live \code{logical} or \code{character}.
#' \itemize{
#'   \item{If \code{TRUE}, then all functions with name in \code{fn_name} in environment
#'   \code{env} are debugged once.
#'   If \code{FALSE, then none are}}
#'   \item{If \code{character} and contains \code{'all'}, then all functions with name in \code{fn_name} are debugged once.
#'   If \code{character} but not including \code{'all'}, then all functions that have names for which the beginning matches
#'   at least partially any element in \code{debug_live} are debugged once.}
#' }
#'
#' @param env environment. Environment in which functions are found.
.add_debug <- function(fn_name = NULL, debug_live, env) {
  purrr::walk(fn_name, function(x) .add_debug_ind(fn_name = x, debug_live = debug_live, env = env))
  invisible(TRUE)
}

#' @title Debug a function once if run if selected
#'
#' @description Applies \code{debugonce} to a single function.
#'
#' @inheritParams .add_debug
#' @param fn_name character. Name of function.
#'
#' @return \code{invisible(TRUE)}.
.add_debug_ind <- function(fn_name, debug_live, env) {
  if (!is.character(debug_live[1]) && !is.logical(debug_live[1])) stop("debug_live must be a character or logical vector.")
  fn <- env[[fn_name]]
  # do nothing if debug_live is FALSE
  if (is.logical(debug_live)) {
    if (!debug_live) {
      return(invisible(TRUE))
    }
  }
  # debug function if debug_live is TRUE or first element is 'all'
  if (debug_live == TRUE || "all" %in% debug_live) {
    debugonce(fn)
    assign(x = fn_name, value = fn, envir = env)
    return(invisible(TRUE))
  }

  # debug function if an element of debug_live matches the first characters of fn_name
  if (is.character(debug_live)) {
    match <- purrr::map_lgl(debug_live, function(elem) {
      max_len <- min(stringr::str_length(elem), stringr::str_length(fn_name))
      fn_name <- stringr::str_sub(fn_name, end = max_len)
      elem <- stringr::str_sub(elem, end = max_len)
      elem == fn_name
    }) %>%
      any()
    if (match) {
      debugonce(fn)
      assign(x = fn_name, value = fn, envir = env)
    }
  }

  # output
  invisible(TRUE)
}
