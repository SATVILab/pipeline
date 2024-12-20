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
#' dir_proj (path to project, relative to package directory,
#'            i.e. excluding the package name),
#' fn (a column where each element is a list containing all the relative paths
#'     to files in specified sub-directories),
#' and all the columns from \code{iter}, specifying the model setup.
.get_out_tbl <- function(dir_base,
                         dir_proj,
                         dir_sub = NULL,
                         iter = NULL,
                         p_dots = NULL) {

  out_tbl <- tibble::tibble(
    dir_proj = stringr::str_remove(
      dir_proj, paste0(here::here(), "/")
    )
  )

  if (!is.null(iter)) {
    if (!"data.frame" %in% class(iter)) {
      iter <- tibble::as_tibble_row(iter)
    }
  }

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

  out_tbl <- out_tbl %>%
    dplyr::mutate(
      iter = list(iter)
    )

  if (is.null(p_dots)) {
    p_dots <- list()
  }
  out_tbl <- out_tbl %>%
    dplyr::mutate(p_dots = list(p_dots))
  path_encoding <- file.path(
    dir_base, "long_to_short_list_var.rds"
  )
  long_to_short_list_var <- switch(as.character(file.exists(path_encoding)),
    "TRUE" = readRDS(path_encoding),
    list()
  )
  for (i in seq_along(iter)) {
    text_ref <- pipeline:::.create_text_ref(iter[[i]])
    tbl_add <- tibble::tibble(x = text_ref)
    colnames(tbl_add) <- colnames(iter)[i]
    out_tbl <- out_tbl %>%
      dplyr::bind_cols(tbl_add)
  }

  out_tbl
}
