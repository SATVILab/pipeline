#' @title Set up project directory
#'
#' @description
#' Get the project directory, either as supplied (if \code{dir_proj} in \code{run} is
#' a \code{character}) or as created by the \code{dir_proj} parameter if its argument is a \code{function}
#' taking its arguments from the dots of \code{run}.
#'
#' This directory is always created, and emptied if instructed.
#'
#' @inheritParams run
#' @param p_dots named list. Named list returned by \code{rlang::list2} applied to
#' the ellipses of \code{run}.
#'
#' @return A \code{character}, that is the absolute path of the project directory. Side effects
#' are that this project directory is created if it doesn't exist and emptied if \code{proj_dir_empty}
#' is \code{TRUE}.
.setup_proj_dir <- function(dir_proj,
                            dir_proj_empty,
                            p_dots) {
  # check that dir_proj is a character or a function.
  if (missing(dir_proj) || !("character" %in% class(dir_proj) || base::is.function(dir_proj))) {
    stop("dir_proj should be a function or a character.")
  }

  # create dir_proj if need be
  if (base::is.function(dir_proj)) {
    if (missing(p_dots)) stop("p_dots must be supplied when dir_proj is a function.")
    if (is.null(formals(dir_proj)) || length(formals(dir_proj)) != 1) stop("dir_proj must accept one argument if it's a function.")
    dir_proj <- dir_proj(p_dots)
    if (!is.character(dir_proj)) stop("output of dir_proj function must be a character")
  }

  # ensure that path is normalised
  dir_proj <- suppressWarnings(suppressMessages(normalizePath(dir_proj)))

  # empty dir_proj if it exists and dir_proj_empty is TRUE
  if (dir.exists(dir_proj) && dir_proj_empty) unlink(dir_proj, recursive = TRUE)

  # creat dir_proj if it doesn't exist
  if (!dir.exists(dir_proj)) dir.create(dir_proj, recursive = TRUE)

  # return path as character
  dir_proj
}
