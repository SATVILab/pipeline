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
#' @param var_to_encode
#' "~all~", "" or a character vector.
#' Specifies which columns in \code{iter}
#' should encode the levels within them by, e.g. "v1", "v2", "v3", ...,
#' rather than using the actual values, to
#' create the sub-directories.
#' If \code{""}, then no variables are matched.
#' If \code{"~all~"}, then all variables are encoded.
#' #' If some character vector, then only those columns in
#' \code{iter} with name matching an elemented in
#' \code{var_to_encode} are matched.
#' Default is \code{""}.
#' @param var_to_fn
#' A named list, with each element a function
#' The first argument to the function is the value
#' and the second the column name. For example,
#' if \code{iter <- data.frame(x = 1)}, then \code{1}
#' is passed to the first argument and \code{"x"}
#' to the second.
#' The output of this function should be a character vector
#' of length 1.
#' Columns in \code{var_to_fn} matching a name in
#' \code{var_to_fn} use the corresponding
#' function in \code{var_to_fn} to determine
#' the sub-directory name.
#' Default is \code{list()}.
#' @param debug logical.
#' If \code{TRUE}, then debug statements are printed
#' at appropriate places to assess the function.
#' Default is \code{FALSE}.
#' @return Returns the project directory as a character vector.
#'
#' @details
#' Issues:
#' - What happens if there is parallel computing and you're encoding
#' variables?
#'   - In that case, possibly best to create a map initially using
#' single-threading (i.e. not running the pipeline) and then run the pipeline.
#'   - Another option (if you are allowed long enough directories) would be
#'   to create a unique label initially, and then replace it once the entire run
#'   is complete.
#'     - This would involve quite a lot of copying, though.
#'     - Perhaps results could be saved to a temporary directory first,
#'       and then changed out.
#'   - Unless your data processing is really heavy (and why should it be?),
#'   that seems reasonable to me.
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
                             max_len = NULL,
                             var_to_exc = "",
                             var_to_encode = "",
                             var_to_fn = list(),
                             debug = FALSE,
                             save_cn_and_encoding_in_dir = TRUE,
                             delete_pre_existing = FALSE) {
  max_len <- ifelse(
    is.null(max_len) && grepl("windows", tolower(sessionInfo()$running)),
    256,
    1e3
  )
  dir_proj <- file.path(dir_base)
  path_long_to_short_list_var <- file.path(
    dir_base, "long_to_short_list_var.rds"
  )
  cn_exc_vec <- which(colnames(iter) %in% var_to_exc)
  if (length(cn_exc_vec) > 0) {
    iter <- iter[, -cn_exc_vec]
  }
  if (!dir.exists(dir_base)) dir.create(dir_base, recursive = TRUE)
  if (!identical(stringr::str_trim(var_to_encode), "")) {
    if (identical(stringr::str_trim(var_to_encode), "~all~")) {
      var_to_encode <- colnames(iter)
    }

    if (file.exists(path_long_to_short_list_var)) {
      long_to_short_list_var <- readRDS(path_long_to_short_list_var)

      # remove variables not slated for encoding
      # from long_to_short_list_var so that
      # we can keep logn_to_short_list_var
      # as also a definitive list of variables to encode
      var_to_encode_missing <- setdiff(
        names(long_to_short_list_var),
        var_to_encode
      )
      if (length(var_to_encode_missing) > 0) {
        var_to_encode_missing_ind <- which(
          names(long_to_short_list_var) %in%
            var_to_encode_missing
        )
        long_to_short_list_var <- long_to_short_list_var[
          -var_to_encode_missing_ind
        ]
      }
    } else {
      long_to_short_list_var <- list()
    }

    var_to_encode_add <- setdiff(
      var_to_encode,
      c(
        names(long_to_short_list_var),
        names(var_to_fn)
      )
    )

    if (length(var_to_encode_add) > 0) {
      long_to_short_list_var_add <- purrr::map(
        var_to_encode_add,
        function(x) {
          character(0L)
        }
      ) %>%
        setNames(var_to_encode_add)
      long_to_short_list_var <- long_to_short_list_var %>%
        append(long_to_short_list_var_add)
    }
  } else {
    long_to_short_list_var <- list()
  }

  for (i in seq_along(iter)) {
    nm <- names(iter)[[i]]
    ind_fn <- which(vapply(
      names(var_to_fn), function(x) identical(x, nm), logical(1)
    ))
    val <- iter[[i]]
    if (debug) print("val")
    if (debug) print(val)
    if (length(ind_fn) > 1) {
      stop(paste0("more than one list name matches ", nm))
    }
    ind_encoding <- which(vapply(
      names(long_to_short_list_var), function(x) identical(x, nm), logical(1)
    ))
    if (debug) print("i")
    if (debug) print(i)
    if (debug) print("nm")
    if (debug) print(nm)
    if (debug) print("ind_encoding")
    if (debug) print(ind_encoding)
    if (length(ind_encoding) > 1) {
      stop(paste0("more than one list name matches ", nm))
    }
    if (length(ind_fn) == 1) {
      dir_sub <- var_to_fn[[ind_fn]](val, nm)
    } else {
      dir_sub_orig <- .create_text_ref(
        elem = val,
        max_len = Inf
      )

      if (length(ind_encoding) == 1) {
        if (debug) print("ind_encoding activated")
        long_to_short_var <- long_to_short_list_var[[ind_encoding]]
        if (debug) print("long_to_short_var")
        if (debug) print(long_to_short_var)
        if (dir_sub_orig %in% names(long_to_short_var)) {
          if (debug) print("dir_sub_orig is in names(long_to_short_var)")
          if (debug) print("dir_sub_orig")
          if (debug) print(dir_sub_orig)
          dir_sub <- long_to_short_var[[dir_sub_orig]]
          if (debug) print("dir_sub")
          if (debug) print(dir_sub)
        } else {
          if (debug) print("dir_sub_orig NOT in names(long_to_short_var)")
          if (debug) print("dir_sub_orig")
          long_vec <- names(long_to_short_var)
          if (debug) print("long_vec")
          if (debug) print(long_vec)
          if (length(long_vec) == 0) {
            if (debug) print("long_vec empty")
            long_to_short_var <- setNames("v1", dir_sub_orig)
            if (debug) print("long_to_short_var created")
            if (debug) print(long_to_short_var)

            dir_sub <- "v1"
            if (debug) print("dir_sub")
            if (debug) print(dir_sub)
          } else {
            if (debug) print("long_vec has length >= 1")
            short_vec <- setNames(long_to_short_var, NULL)
            if (debug) print("short_vec")
            if (debug) print(short_vec)
            last_name <- short_vec[length(short_vec)]
            if (debug) print("last_name")
            if (debug) print(last_name)
            last_val <- stringr::str_split(
              last_name,
              pattern = "^v"
            )[[1]][2] %>%
              as.numeric()
            if (debug) print("last_val")
            if (debug) print(last_val)

            if (debug) print("long_to_short_var")
            if (debug) print(long_to_short_var)
            dir_sub <- paste0("v", last_val + 1)
            if (debug) print("dir_sub")
            if (debug) print(dir_sub)
            long_to_short_var <- c(
              long_to_short_var,
              setNames(paste0("v", last_val + 1), dir_sub_orig)
            )
          }
          long_to_short_list_var[[ind_encoding]] <- long_to_short_var
          if (debug) print("long_to_short_list_var")
          if (debug) print(long_to_short_list_var)
          saveRDS(
            object = long_to_short_list_var,
            file = path_long_to_short_list_var
          )
        }
      } else {
        dir_sub <- dir_sub_orig
      }
    }

    fn_vec <- list.files(dir_proj, full.names = TRUE)
    fn_vec_to_remove <- fn_vec[
      !grepl("^enc~.*\\.txt$", basename(fn_vec))
    ]
    if (length(fn_vec_to_remove) >= 1) {
      fn_vec_to_remove <- fn_vec_to_remove[fs::is_file(fn_vec_to_remove)]
    }
    if (length(fn_vec_to_remove) >= 1) {
      file.remove(fn_vec_to_remove)
    }
    if (save_cn_and_encoding_in_dir) {
      fn_out <- file.path(dir_proj, paste0("enc~", nm, ".txt"))
      out_tbl <- switch(as.character(nm %in% names(long_to_short_list_var)),
        "TRUE" = data.frame(
          orig = names(long_to_short_var),
          dir_sub = long_to_short_var
        ),
        "FALSE" = ""
      )
      write.table(
        x = out_tbl,
        file = fn_out,
        append = FALSE
      )
      fn_vec <- list.files(dir_proj, full.names = TRUE)
      fn_vec_to_remove <- setdiff(fn_vec, fn_out)
      if (length(fn_vec_to_remove) >= 1) {
        fn_vec_to_remove <- fn_vec_to_remove[fs::is_file(fn_vec_to_remove)]
      }
      if (length(fn_vec_to_remove) >= 1) {
        file.remove(fn_vec_to_remove)
      }
    }
    dir_proj <- file.path(
      dir_proj,
      dir_sub
    )
    if (!dir.exists(dir_proj)) dir.create(dir_proj, recursive = TRUE)
  }

  saveRDS(
    object = long_to_short_list_var,
    file = path_long_to_short_list_var
  )

  dir_proj <- fs::path_norm(dir_proj)
  if (stringr::str_length(dir_proj) > max_len) {
    stop(paste0(
      dir_proj,
      " has length ",
      stringr::str_length(dir_proj)
    ))
  }
  if (dir.exists(dir_proj) && delete_pre_existing) {
    unlink(dir_proj, recursive = TRUE)
  }
  if (!dir.exists(dir_proj)) {
    dir.create(
      dir_proj,
      recursive = TRUE
    )
  }
  dir_proj
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
                             collapse = "") {
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
  if (grepl("\\\\n|\\\\r|\\\\t|\\\\e|\\\\f", out)) {
    warning(paste0("removed special characters from dir_sub: ", out))
    out <- gsub("\\\\n|\\\\r|\\\\t|\\\\e|\\\\f", "", out)
  }
  if (grepl("\\\\|/", out)) {
    warning(paste0("removed directory-creating characters from dir_sub: ", out))
    out <- gsub("\\\\", "", out)
    out <- gsub("/", "", out)
  }
  out
}
