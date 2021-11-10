#' @title Create a project directory
#'
#' @param dir_base character.
#' Path to "base" directory, to which
#' additional sub-directories are appended.
#' Default is \code{here::here()}.
#' @param iter tibble of one row or list.
#' Each element in iter is used to create
#' a sub-directory, appended in order from left to
#' right (i.e. the left-most column of iter creates
#' the first appended sub-directory).
#' See \code{?pipeline:::.create_text_ref} for details.
#' @param max_len numeric.
#' Maximum length of the directory.
#' If \code{NULL}, then is is set to 256
#' if the operating system is Windows and
#' 1e3 otherwise.
#' Default is \code{NULL}.
#'
#' @return Invisible returns the project directory.
#'
#' @examples
#' .create_dir_proj(
#'   dir_base = tempdir(),
#'   iter = tibble::tibble(
#'     param1 = c("a"),
#'     param2 = list(list(fn1 = function(x) x)),
#'     param3 = c("a" = "a")
#'   )
#' )
#'
#' iter <- structure(
#'   list(
#'     var_exp = "Progressor",
#'     var_exp_spline = list(
#'       `tc~soma~C9` = list(fn = "ns", params = list(df = 2))
#'     ),
#'     var_int = list(
#'       c("Progressor", "C9")
#'     ),
#'     wins = "wins_n",
#'     var_conf = list(
#'       "none"
#'     ),
#'     ttb_max = NA_real_,
#'     var_dep = "risk6"
#'   ),
#'   row.names = c(
#'     NA,
#'     -1L
#'   ),
#'   class = c("tbl_df", "tbl", "data.frame")
#' )
#' .create_dir_proj(dir_base = tempdir(), iter = iter)
.create_dir_proj <- function(dir_base = here::here(),
                             iter = NULL,
                             max_len = NULL) {
  max_len <- ifelse(
    grepl("windows", tolower(sessionInfo()$running)),
    256,
    1e3
  )
  dir_proj <- file.path(dir_base)
  for (x in iter) {
    dir_proj <- file.path(
      dir_proj,
      .create_text_ref(x, max_len = max_len)
    )
  }
  dir_proj <- fs::path_norm(dir_proj)
  if (stringr::str_length(dir_proj) > max_len) {
    stop(paste0(
      dir_proj,
      " has length ",
      stringr::str_length(dir_proj)
    ))
  }
  if (!dir.exists(dir_proj)) {
    dir.create(
      dir_proj,
      recursive = TRUE
    )
  }
  invisible(dir_proj)
}

#' @title Create directory addition
#'
#' @description Create addition to add to directory.
#' The basic behaviour is that either an object's
#' elements are named (e.g. \code{c("a" = 1)} instead of
#' the unnamed \code{c(1)}),
#' in which case the name(s) creates the sub-directory,
#' or else the object is not named (e.g \code{c(1)}) above)
#' in which case the

#' @param elem some R object.
#' If the object is named (i.e. names(elem) is not \code{NULL}),
#' then the name is the addition
#' (multiple names are concatenated to a length-one
#' character vector).
#' If the object is not named, then the value
#' itself is coerced to a character vector of length 1.
#' If this does not work, an error is returned.
#'
#' @examples
#' .create_text_ref(c("a" = "b"))
#' .create_text_ref(c("b"))
#' .create_text_ref(list("a" = "b"))
#' .create_text_ref(list("b"))
#' .create_text_ref(list("a" = function(x) x))
#' # will create an error if a function is passed not
#' # in a named list
#' try(.create_text_ref(list(function(x) x)))
#' # it will automatically "lift up" elements
#' # that are lower down:r
#' .create_text_ref(list(list("a")))
#' .create_text_ref(list(list("a", "b")))
#' .create_text_ref(list(list("a", "b", function(x) x)))
.create_text_ref <- function(elem,
                             max_len = 40,
                             collapse = ""
) {
  if (is.null(names(elem))) {
    need_name <- purrr::map_lgl(elem, function(x) {
      typeof(x) %in% c(
        "closure", "expression",
        "language", "environment"
      )
    }) %>%
      any()
    if (need_name) {
      stop("If no name is supplied, then no element within the object can be a closure, expression, language or environment object.") # nolint
    }
  }

  nm_base <- switch(as.character(is.null(names(elem))),
    "TRUE" = try(purrr::map_chr(elem, function(x) {
      if (is.null(x)) {
        return("NULL")
      }
      paste0(x, collapse = "", sep = "")
    })),
    "FALSE" = names(elem)
  )
  out <- try(paste0(nm_base, collapse = "", sep = ""))

  if (class(out)[1] == "try-error") {
    stop(paste0("Could not coerce the following into a character of length 1: ", nm_base)) # nolint
  }
  if (length(out) != 1) {
    stop(paste0("Could not coerce the following into a character of length 1: ", nm_base)) # nolint
  }
  if (!is.character(out)) {
    stop(paste0("Could not coerce the following into a character of length 1: ", nm_base)) # nolint
  }
  if (stringr::str_length(out) > max_len) {
    stop(paste0("Use a name for directory entry as ", out, " is longer than ", max_len, " characters")) # nolint
  }
  if (grepl("\\\\|/", out)) {
    stop(paste0("Cannot use a directory divider, i.e. \\\\ or /, to name directory: ", out)) # nolint
  }
  if (grepl("\\n|\\r|\\t|\\e|\\f", out)) {
    stop(paste0("Cannot use a back slash to name directories: ", out)) # nolint
  }
  out
}
