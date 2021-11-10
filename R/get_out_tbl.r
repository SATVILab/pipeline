#' @title Collate model output file objects and model parameters
#'
#' @param dir_proj character.
#' Path to directory where individual
#' model results are kept.
#' @param iter dataframe with one row (or,
#' alternatively, a list that is coerced to
#' a dataframe (tibble) with one row).
#' Specifies arguments to specific
#' model run.
#'
#' @return A tibble with one row, containing the
#' relative paths to the objects saved within
#' each folder by the model run, as well as the
#' arguments supplied to the model run.
.get_out_tbl <- function(dir_proj, iter = NULL) {
  out_tbl <- tibble::tibble(
    path = stringr::str_remove(
      dir_proj, paste0(dirname(here::here()), "/")
      ),
    params = list(list.files(file.path(dir_proj, "params"),
      recursive = TRUE,
      include.dirs = TRUE
    )),
    explore = list(list.files(file.path(dir_proj, "exp"),
      recursive = TRUE,
      include.dirs = TRUE
    )),
    fit = list(list.files(file.path(dir_proj, "fit"),
      recursive = TRUE,
      include.dirs = TRUE
    )),
    validate = list(list.files(file.path(dir_proj, "val"),
      recursive = TRUE,
      include.dirs = TRUE
    )),
    extract = list(list.files(file.path(dir_proj, "extr"),
      recursive = TRUE,
      include.dirs = TRUE
    )),
    display = list(list.files(file.path(dir_proj, "disp"),
      recursive = TRUE,
      include.dirs = TRUE
    )),
    output = file.path(dir_proj, "output.html")
  )
  cn_to_dir <- c(
    "params" = "params",
    "explore" = "exp",
    "fit" = "fit",
    "validate" = "val",
    "extract" = "extr",
    "display" = "disp"
  )
  for (x in c(
    "explore", "fit", "validate",
    "extract", "display"
  )) {
    if (length(out_tbl[[x]][[1]]) == 0) next
    out_tbl[[x]] <- list(paste0(cn_to_dir[[x]], "/", out_tbl[[x]][[1]]))
  }

  if (!is.null(iter)) {
    out_tbl <- iter_inner_non_null %>%
      tibble::as_tibble() %>%
      dplyr::bind_cols(
        out_tbl
      )
  }
  out_tbl
}