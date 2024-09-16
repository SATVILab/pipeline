#' @title Save objects to specified directory
#'
#' @description
#' Saves objects specified using dots and/or a list.
#' Ensures that all names are unique (appending with "-uni_<ind>" for
#' duplicated names). Ensures that all objects have names (appending with "-obj_<ind>")
#' for objects without names.
#'
#' @param ... Objects to be saved. Preferrably specify name-value pairings,
#' otherwise default values will be given. Objects are saved with the name appended by ".rds".
#' @param obj_list list. List where the names (if supplied) are the
#' names to save the corresponding values in the list as. If names
#' are not supplied, then they will be created. Note that if you want to save ggplot2 plots as "PNG" or "PDF" images with
#' custom names, then you need to pass them to the function as a named list to the \code{obj_list} parameter, e.g.
#' \code{obj_list = list("Exploratory plot" = p_exp)} will save the plot \code{p_exp} with name "Exploratory plot.png" (or .pdf, if
#' that's chosen below.)
#' @param dir_proj character.
#' @param dir_sub character vector or character list. Each element appends a new directory
#' to dir_proj, in the order they're given. If not provided, then \code{dir_proj} is the final
#' directory.
#' @param empty logical. If \code{TRUE}, then the
#' directory to be saved to is attempted to be deleted before
#' saving any new files to it. This attempt
#' fails on Windows 10, at least, if any files
#' from these directories are open, or even if just
#' a Windows Explorer object is open to that directory or a
#' a sub-directory. Default is \code{TRUE}.
#' @param silent logical. If \code{FALSE}, then warnings and messages
#' are suppressed. Default is \code{TRUE}.
#' @param gg_device 'rds', 'png' or 'pdf. Specifies device to
#' save objects of class \code{'gg'}. They are saved using \code{cowplot::ggsave2} function. If
#' gg_device does not contain any of rds, png or pdf, then saved as rds. Default is \code{'png'}.
#' @param width,height numeric. Height and width to pass to \code{cowplot::ggsave2}. Default is
#' 12 and 10.
#'
#' @details Files that already exist before they are saved are deleted. This is to ensure
#' that on Windows the "Date Modified" column in Windows Explorer updates.
#'
#' @return \code{invisible{TRUE}}.
#'
#' @examples
#' # standard use
#' dir_proj <- tempdir()
#' x <- "a"
#' y <- "b"
#' save_objects(x = x, y = y, dir_proj = dir_proj, dir_sub = "test")
#' for (obj in c("x", "y")) {
#'   print(file.exists(file.path(
#'     dir_proj, "test",
#'     paste0(obj, ".rds")
#'   )))
#' }
#' readRDS(file.path(dir_proj, "test", "x.rds"))
#' readRDS(file.path(dir_proj, "test", "y.rds"))
#'
#' # ensure all objects have names
#' # warning normally printed
#' suppressWarnings(save_objects("a",
#'   obj_list = list("c", 100, z = 4),
#'   dir_proj = dir_proj, dir_sub = "test"
#' ))
#' readRDS(file.path(dir_proj, "test", "obj_1.rds"))
#' readRDS(file.path(dir_proj, "test", "obj_2.rds"))
#' readRDS(file.path(dir_proj, "test", "obj_3.rds"))
#' # objects with names given do not have names overwritten
#' readRDS(file.path(dir_proj, "test", "z.rds"))
#'
#' # ensure that objects have unique names
#' # warning printed
#' save_objects(x = "a", x = "b", dir_proj = dir_proj, dir_sub = "test")
#' readRDS(file.path(dir_proj, "test", "x-uni_1.rds"))
#' readRDS(file.path(dir_proj, "test", "x-uni_2.rds"))
#' @importFrom stats setNames
#' @export
save_objects <- function(...,
                         obj_list = NULL,
                         dir_proj,
                         dir_sub = NULL,
                         empty = TRUE,
                         silent = FALSE,
                         gg_device = "png",
                         embed = TRUE,
                         height = 10,
                         width = 12) {
  # =======================
  # Get objects
  # =======================

  # get objects to be saved as a list
  obj_dots <- rlang::list2(...)
  if ("gg" %in% class(obj_list)) {
    if (!silent) warning("Object passed to obj_list parameter has class 'gg'. Therefore \n
                        obj_list is saved as a gg object, rather than its individual
                        elements being saved. Remove class attributes of 'gg' object
                        if you wish to save individual elements of a 'gg' object.")
    obj_list <- list(obj_list)
  }
  obj_list <- obj_dots %>% append(obj_list)

  # skip rest of function if nothing to save
  if (length(obj_list) == 0) {
    if (!silent) message("obj_list has no objects to save")
    return(invisible(TRUE))
  }

  # =======================
  # Ensure appropriate names
  # =======================

  # unnamed objects
  # ------------------
  # if objects are not named, print a warning and then
  # give them names
  name_vec <- names(obj_list)
  k <- 0
  if (is.null(name_vec)) {
    if (!silent) warning("No objects to be saved have names. Names will be given.")
    name_vec <- paste0("obj_", seq_along(obj_list))
  }
  if (any(name_vec == "") && !silent) warning("Some objects to be saved do not have names. Names will be given for these.")
  for (i in seq_along(obj_list)) {
    if (name_vec[i] != "") next
    k <- k + 1
    name_vec[i] <- paste0("obj_", k)
  }

  # duplicate names
  # ------------------

  # if objects have duplicate names, print a warning and
  # then make their names unique
  if (length(name_vec) != length(unique(name_vec))) {
    # print warning
    if (!silent) warning("not all objects have unique names")
    # get unique names
    unique_name_vec <- unique(name_vec)

    # get indices of objects whose names match a unique name
    unique_name_index_list <- purrr::map(
      unique_name_vec,
      function(name_curr) {
        which(name_vec == name_curr)
      }
    ) %>%
      setNames(unique_name_vec)

    # for each unique name, if more than one
    # object has that name, then append "-uni_<ind>"
    # to the end, so that they have unique names
    # and which objects have been forced to have unique
    # names is obvious
    for (i in seq_along(unique_name_index_list)) {
      # get indices of names matching a particular
      # unique name
      uni_name_match_ind_vec <- unique_name_index_list[[i]]
      # get number of matches
      n_uni_names <- length(uni_name_match_ind_vec)
      # if number of matches is 1, then do nothing
      if (n_uni_names == 1) next
      # if number of matches is greater than 1,
      # then append "-obj_<ind>" to each with
      # ind incrementing
      for (j in 1:n_uni_names) {
        ind_curr <- uni_name_match_ind_vec[j]
        name_rep <- paste0(
          names(unique_name_index_list)[i],
          "-uni_", j
        )
        name_vec[ind_curr] <- name_rep
      }
    }
  }

  # rename obj_list
  obj_list <- setNames(obj_list, name_vec)

  # ================
  # Directory
  # ================

  # create directory to be saved to
  dir_save <- dir_proj
  for (x in dir_sub) dir_save <- file.path(dir_save, x)
  dir_save <- suppressWarnings(normalizePath(dir_save))
  if (dir.exists(dir_save) && empty) {
    try(unlink(dir_save, recursive = TRUE),
      silent = TRUE
    )
  }
  if (!dir.exists(dir_save)) dir.create(dir_save, recursive = TRUE)

  # ==================
  # Save
  # ==================

  # save objects
  gg_warning_given <- FALSE
  # loop over objects
  for (i in seq_along(obj_list)) {
    # if not a ggplot2 object or ggplot2 objects to be saved as rds objects
    if (!"gg" %in% class(obj_list[[i]]) || stringr::str_detect(gg_device, ".rds")) {
      # save file, deleting the previous version if it exists
      fn <- file.path(dir_save, paste0(names(obj_list)[i], ".rds"))
      fn <- suppressWarnings(normalizePath(fn))
      if (file.exists(fn)) try(file.remove(fn), silent = TRUE)
      saveRDS(obj_list[[i]], fn)
      # if it is a ggplot2 object
    } else if ("gg" %in% class(obj_list[[i]])) {
      # if it is a ggplot2 object and gg_device not rds but not pdf or png either - then
      # give a warning and save as rds
      if (!any(purrr::map_lgl(c("pdf", "png"), function(x) stringr::str_detect(gg_device, x)))) {
        if (!silent && !gg_warning_given) {
          warning("gg_device not 'rds', 'pdf' or 'png' for a ggplot2 object. Saved as rds.\n This may consume a lot of space, as the data is then saved as well.")
          gg_warning_given <- TRUE
        }
        fn <- file.path(dir_save, paste0(names(obj_list)[i], ".rds"))
        fn <- suppressWarnings(normalizePath(fn))
        if (file.exists(fn)) try(file.remove(fn), silent = TRUE)
        saveRDS(obj_list[[i]], fn)
      } else {
        # if it is a ggplot2 object and gg_device is either pdf or png
        gg_device <- stringr::str_remove_all(gg_device, "[[:punct:]]") # remove full stop if given
        fn <- file.path(dir_save, paste0(names(obj_list)[i], ".", gg_device))
        fn <- suppressWarnings(normalizePath(fn))
        if (file.exists(fn)) try(file.remove(fn), silent = TRUE)
        ggplot2::ggsave(
          filename = fn,
          plot = obj_list[[i]],
          width = width,
          height = height,
          units = "cm"
        )
        if (embed) {
          embed_pdf(fn = fn)
        }
      }
    }
  }

  # output
  invisible(TRUE)
}

#' @title Embed PDF fonts
embed_pdf <- function(fn) {
  if (!stringr::str_sub(fn, start = -3) == "pdf") {
    return(invisible(TRUE))
  }
  out <- try(grDevices::embedFonts(file = fn))
  if (class(out) == "try-error") {
    if (!nzchar(Sys.getenv(x = "R_GSCMD"))) {
      warning("\nR env variable `R_GSCMD` not set.\n\nFirst ensure that GhostScript is installed (it's a program, and not an R package).\nThen set to path to GhostScript executable using `Sys.setenv` (inside R).\nFor example, on Windows it may be `Sys.setenv(R_GSCMD = 'C:\\Program Files (x86)\\gs\\gs9.54.0\\bin\\gswin32c.exe')` (replace '9.54.0' with the relevant version; go to 'C:\\Program Files (x86)\\gs' to see which). Note that you must choose the executable ending in c, e.g. 'gswin32c.exe' and not 'gswin32.exe'. \n\nEmbedding not performed.") # nolint
    } else {
      warning(paste0("embedding of ", fn, " failed for some reason."))
    }
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

#' @title Return RDS objects in a folder
#'
#' @return A named list, where the names are the names of the files and
#' the elements are the RDS objects. If there are no RDS files in folder,
#' then an empty list is returned.
#' @export
return_rds <- function(dir_obj) {
  fn_vec <- list.files(dir_obj, pattern = "rds")
  if (length(fn_vec) == 0) {
    return(list())
  }

  obj_list <- purrr::map(fn_vec, function(fn) {
    readRDS(file.path(dir_obj, fn))
  }) %>%
    setNames(stringr::str_remove(fn_vec, ".rds"))
  obj_list
}
