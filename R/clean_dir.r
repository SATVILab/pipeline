#' @title Clean up a directory
#'
#' @description Remove all unspecified files, or files
#' not matching a particular character expression, from
#' a directory.
#'
#' @param clean_dir character.
#' Directory to clean.
#' @param keep_fn character vector.
#' Paths (preferrably absolute) to files
#' that should not be deleted.
#' @param keep_dir_sub character vector.
#' Paths to directories in which files
#' (and sub-directories) should not
#' be deleted.
#' @param keep_base_name character.
#' File names that are not to be deleted,
#' (even if not specified within fn or
#' part of a keep sub-directory).
#' Matched on the basis of regular expression.
#' @param keep_base_name_strict
#' logical.
#' If \code{TRUE}, then files with matching base names
#' will be keep only if they belong to a super-directory
#' of a keep directory.
#' This allows keeping files outside the keep sub-directories
#' that "describe" the sub-directory.
#' @return If successful, \code{invisible(TRUE)}.
clean_dir <- function(clean_dir,
                      keep_fn = NULL,
                      keep_dir_sub = NULL,
                      keep_base_name = NULL,
                      keep_base_name_strict = FALSE) {

  if (missing(clean_dir)) stop("clean_dir must be provided")
  if (is.null(clean_dir)) stop("clean_dir must not be NULL")
  if (!dir.exists(clean_dir)) stop("clean_dir does not exist")

  if (!is.null(keep_fn)) {
    if (any(!is.character(keep_fn))) {
      stop("At least one keep_fn element is not a character")
    }
    if (any(!fs::is_file(keep_fn))) {
      stop("At least one element of keep_fn is not a file")
    }
  }
  if (!is.null(keep_dir_sub)) {
    if (any(!is.character(keep_dir_sub))) {
      stop("At least one keep_dir_sub element is not a character")
    }
    if (any(!fs::is_dir(keep_dir_sub))) {
      stop("At least one element of keep_dir_sub is not a directory")
    }
  }

  if (!is.null(keep_base_name)) {
    if (!is.character(keep_base_name)) {
      stop("keep_base_name must be a character if not NULL")
    }
  }

  if (!is.logical(keep_base_name_strict)) {
    stop("keep_base_name_strict must be a logical")
  }

  if (length(keep_base_name_strict) > 1) {
    stop("keep_base_name_strict must have length 1")
  }

  clean_dir <- fs::path_norm(clean_dir)
  remove_fn <- list.files(
    clean_dir,
    full.names = TRUE,
    recursive = TRUE
  )
  remove_fn <- fs::path_norm(remove_fn)
  keep_fn <- fs::path_norm(keep_fn)

  if (!is.null(keep_dir_sub)) {
    keep_dir_sub <- setdiff(keep_dir_sub, "")
    if (length(keep_dir_sub) == 0) keep_dir_sub <- NULL
    if (!is.null(keep_dir_sub)) {
      keep_dir_sub <- fs::path_abs(keep_dir_sub)
      keep_fn <- c(
        keep_fn,
        list.files(keep_dir_sub, full.names = TRUE, recursive = TRUE)
      )
    }
  }

  if (!is.null(keep_base_name)) {
    if (!keep_base_name_strict) {
      remove_fn_n_ind <- vapply(
        basename(remove_fn),
        function(x) {
          any(
            vapply(keep_base_name, function(bn) {
              grepl(bn, x)
            }, logical(1))
          )
        },
        logical(1)
      )
      keep_fn <- c(keep_fn, remove_fn[remove_fn_n_ind])
    } else if (!is.null(keep_dir_sub) || !is.null(keep_fn)) {
      
      keep_dir <- NULL
      if (!is.null(keep_dir_sub)) {
        keep_dir <- c(keep_dir, .get_dir_parent(
          fs::path_rel(path = keep_dir_sub, start = clean_dir)
        ))
      }
      if (!is.null(keep_fn)) {
        keep_dir <- c(keep_dir, .get_dir_parent(
          fs::path_rel(path = keep_fn, start = clean_dir)
        ))
        keep_dir <- unique(keep_dir)
      }

      keep_dir <- setdiff(keep_dir, ".")
      keep_dir <- file.path(clean_dir, keep_dir)
      keep_dir <- c(keep_dir, clean_dir)

      keep_fn_add <- purrr::map(keep_dir, function(dir_sub) {
        fn_vec <- list.files(dir_sub, recursive = FALSE, full.names = TRUE)
        if (length(fn_vec) == 0) {
          return(NULL)
        }
        remove_fn_n_ind <- vapply(
          basename(fn_vec),
          function(bn) {
            any(
              vapply(keep_base_name, function(bn_exc) {
                grepl(bn_exc, bn)
              }, logical(1))
            )
          },
          logical(1)
        )
        fn_vec[remove_fn_n_ind]
      }) %>%
        purrr::compact() %>%
        unlist()
      keep_fn <- c(keep_fn, keep_fn_add)
    }
  }

  remove_fn <- fs::path_norm(remove_fn)
  keep_fn <- fs::path_norm(keep_fn)

  remove_fn <- setdiff(remove_fn, keep_fn)
  file.remove(remove_fn)

  dir_clean_sub_left <- list.dirs(clean_dir)
  dir_clean_sub_left <- fs::path_norm(dir_clean_sub_left)
  dir_clean_sub_left <- setdiff(dir_clean_sub_left, clean_dir)
  dir_clean_sub_left <- unique(dir_clean_sub_left)
  if (length(dir_clean_sub_left) == 0) {
    return(invisible(TRUE))
  }
  dir_clean_sub_left_comp <- fs::path_rel(
    path = dir_clean_sub_left,
    start = clean_dir
  )

  keep_fn_comp <- fs::path_rel(
    path = dirname(keep_fn),
    start = clean_dir
  )

  if (!is.null(keep_dir_sub)) {
    keep_dir_sub_comp <- fs::path_rel(
      path = keep_dir_sub,
      start = clean_dir
    )
    keep_comp <- c(keep_fn_comp, keep_dir_sub_comp)
    keep_comp <- unique(keep_comp)
  } else {
    keep_comp <- keep_fn_comp
  }

  if (FALSE) {
    keep_comp_parent <- purrr::map(keep_comp, function(x) {
            out <- dirname(x)
            x <- dirname(x)
            while (!grepl("^\\.$", dirname(x))) {
              out <- c(out, dirname(x))
              x <- dirname(x)
            }
            # add in "base" directory here,
            # as not added above.
            # add here rather than just at the end,
            # when it might be slightly faster,
            # for consistency's sake (so everything is added
            # in this "loop").
            out <- c(out, ".")
            out
          }) %>%
          unlist() %>%
          unique()
  }

  keep_comp_parent <- .get_dir_parent(path = keep_comp)
  keep_comp <- c(keep_comp, keep_comp_parent)
  keep_comp <- setdiff(keep_comp, ".")

  dir_clean_sub_to_remove_ind <- !dir_clean_sub_left_comp %in% keep_comp
  dir_clean_sub_to_remove <- dir_clean_sub_left[dir_clean_sub_to_remove_ind]

  unlink(dir_clean_sub_to_remove, recursive = TRUE)

  invisible(TRUE)
}

.get_dir_parent <- function(path) {

  if (any(fs::is_absolute_path(path))) {
    stop("dir must be a relative path")
  }

  purrr::map(path, function(x) {
        out <- dirname(x)
        x <- dirname(x)
        while (!grepl("^\\.$", dirname(x))) {
          out <- c(out, dirname(x))
          x <- dirname(x)
        }
        # add in "base" directory here,
        # as not added above.
        # add here rather than just at the end,
        # when it might be slightly faster,
        # for consistency's sake (so everything is added
        # in this "loop").
        out <- c(out, ".")
        out
      }) %>%
        unlist() %>%
        unique()
}
