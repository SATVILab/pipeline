#' @title Collate model output file objects and model parameters
#'
#' @param dir_proj character.
#' Path to directory where individual
#' model results are kept.
#' @param dir_sub character vector.
#' Names of directories inside \code{dir_proj}
#' whose contents are to be included as
#' relative paths (relative to \code{dir_proj})
#' inside a column \code{"fn"}.
#' If \code{NULL}, then all directories
#' inside \code{dir_proj} are included.
#' Default is \code{NULL}.
#' @param iter dataframe with one row (or,
#' alternatively, a list that is coerced to
#' a dataframe (tibble) with one row).
#' Specifies arguments to specific
#' model run.
#'
#' @details
#' Assumes that it is being run inside the R project/package
#' that created the model.
#'
#' @return A tibble with one row, containing the following columns:
#' path_proj (path to project, relative to package directory,
#'            i.e. excluding the package name),
#' fn (a column where each element is a list containing all the relative paths
#'     to files in specified sub-directories),
#' and all the columns from \code{iter}, specifying the model setup.
.get_out_tbl <- function(dir_proj, dir_sub = NULL, iter = NULL) {
  out_tbl <- tibble::tibble(
    path_proj = stringr::str_remove(
      dir_proj, paste0(here::here(), "/")
    )
  )

  if (is.null(dir_sub)) {
    fn_vec <- list.files(dir_proj, recursive = TRUE, full.names = FALSE)
  } else {
    fn_vec <- purrr::map(dir_sub, function(x) {
      file.path(
        x,
        list.files(file.path(dir_proj, x), recursive = TRUE, full.names = FALSE)
      )
    }) %>%
      unlist()
  }
  out_tbl <- out_tbl %>%
    dplyr::mutate(fn = list(fn_vec))

  if (!is.null(iter)) {
    out_tbl <- switch(class(iter)[1],
      "list" = out_tbl %>%
        dplyr::bind_cols(
          iter %>%
            tibble::as_tibble_row()
        ),
      "tbl_df" = , # nolint
      "data.frame" = {
        if (nrow(iter) != 1L) {
          stop("If iter is a dataframe, then it must have one row exactly")
        }
        out_tbl %>%
          dplyr::bind_cols(iter)
      }
    )
  }

  out_tbl
}
