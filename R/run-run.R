#' @title Run GAMLSS pipeline
#'
#' @description
#' Takes arguments that specify the running of the key steps in running a GAMLSS pipeline:
#' \itemize{
#'   \item{project directory creation,}
#'   \item{data pre-processing,}
#'   \item{creating exploratory plots,}
#'   \item{fitting the model,}
#'   \item{creating validation plots, and}
#'   \item{creating results plots.}
#' }
#' The purpose of this is to make it easier to do all the steps in a reproducible way that
#' saves the results automatically.
#'
#' @param dir_proj \code{character} or \code{function}.
#' \itemize{
#'   \item{If \code{character}, then this folder is created if it doesn't exist.}
#'   \item{If a \code{function}, then it should be a function that
#'   has a single argument that expects the named list produced by \code{rlang::list2(...)}.
#'   This function should simply return. the project directory as a character.
#'   This folder is created if it doesn't exist.}
#' }
#' @param dir_proj_empty \code{logical}. If \code{TRUE}, then the project directory
#' is emptied before any results are saved. Default is \code{FALSE}.
#' TODO: Wrap each of the *_fn functions so that whatever they return is saved to the
#' correct folder.
#' TODO: Enable debugging via a parameter even sub-functions of the main functions.
#' TODO: Ensure that functions are not run in global environment but run in own environment (or a clean environment, as in knitr);
#' if simply ensuring that they're not run in global environment, then add the function
#' to save object to global environment if need be, and then delete it upon function
#' exit.
#' @param preprocess,explore,fit,extract,display,validate functions or \code{NULL}. Functions
#' run in the order specified, passing data_raw, data_mod and p_dots from one to the other. Some functions
#' expect extra output.
#' @param data_raw dataframe. Contains raw data to be processed by \code{preprocess} to create \code{data_mod}.
#' @param debug character vector or \code{FALSE}. Functions provided to \code{run}
#' that at least partially match characters in \code{debug} at the start are run in \code{debugonce}.
#' @param ... Named arguments passed onto all functions (preprocess to validate).
#' @param force_rerun logical.
#' If \code{TRUE}, then the pipeline
#' is rerun even if \code{file.path(dir_proj, "output.html")} exists.
#' Default is \code{TRUE}.
#' @export
.run <- function(iter,
                 p_dots,
                 dir_proj,
                 delete_old_run = TRUE,
                 data_raw,
                 preprocess = NULL,
                 explore = NULL,
                 fit = NULL,
                 validate = NULL,
                 extract = NULL,
                 display = NULL,
                 debug = FALSE,
                 force_rerun = "all",
                 settings_to_print = NULL,
                 ...) {

  # ====================================
  # Preparation
  # ====================================

  fn_list <- list(
    "preprocess" = preprocess,
    "explore" = explore,
    "fit" = fit,
    "validate" = validate,
    "extract" = extract,
    "display" = display
  )


  stage_vec <- c(
    "preprocess", "explore", "fit",
    "validate", "extract", "display"
  )
  stage_vec_pattern <- paste0("^", substr(stage_vec, start = 1, stop = 3))

  if (!all(grepl(paste0("^all$|^none?$|",
    paste0(stage_vec_pattern, collapse = "|"),
    collapse = ""
  ), force_rerun))) {
    stop(paste0(
      "force_rerun  must include only ",
      " include elements that ",
      "match at least the first three letters from:\n'",
      paste0(stage_vec, collapse = "', '"),
      ", 'all' or 'none'."
    ))
  }

  if (any(grepl("^all$|^none$", force_rerun)) &&
    length(force_rerun) > 1) {
    stop(paste0(
      "force_rerun must only have ",
      "'all' or 'none' ",
      "on their own"
    ))
  }

  if (grepl("^none?$", force_rerun)) {
    if (file.exists(file.path(dir_proj, "output.html"))) {
      return(invisible(dir_proj))
    } else {
      stages_to_force_vec <- "none"
    }
  } else {
    if (grepl("^all$", force_rerun)) {
      stages_to_force_vec <- stage_vec
    } else {
      stages_to_force_vec <- vapply(force_rerun, function(stg) {
        stage_vec[vapply(
          stage_vec_pattern,
          function(x) grepl(x, stg), logical(1)
        )]
      }, character(1)) %>%
        setNames(NULL)
    }
  }

  if (!identical(stages_to_force_vec[1], "none")) {
    stage_link_list <- list(
      "preprocess" = stage_vec,
      "explore" = stage_vec[2],
      "fit" = stage_vec[3:6],
      "validate" = stage_vec[4],
      "extract" = stage_vec[5:6],
      "display" = stage_vec[6]
    )
    stages_to_force_vec <- lapply(stages_to_force_vec, function(stg) {
      stage_link_list[stg]
    }) %>%
      unlist() %>%
      unique()
  }
  stages_to_run_if_needed_vec <- setdiff(stage_vec, stages_to_force_vec)

  # get project directory, creating it if need be
  stage_to_basename_vec <- c(
    "preprocess" = "prep",
    "explore" = "exp",
    "fit" = "fit",
    "display" = "disp",
    "extract" = "extr",
    "validation" = "val"
  )
  if (dir.exists(dir_proj)) {
    for (stg in stage_vec) {
      if (stg %in% stages_to_force_vec &&
        delete_old_run) {
        dir_stg <- file.path(dir_proj, stage_to_basename_vec[stg])
        unlink(dir_stg, recursive = TRUE)
      }
    }
  } else {
    dir.create(dir_proj, recursive = TRUE)
  }

  print(dir_proj)

  # check that data is supplied
  if (missing(data_raw)) stop("data_raw must be supplied.")

  # create expected parameters
  # remember to change get_expected_params and its test when this is changed
  expected_params <- list(
    "preprocess" = c(
      "data_raw", "iter",
      "p_dots", "dir_proj"
      ),
    "explore" = c(
      "data_raw", "iter", "data_mod",
      "dir_proj", "p_dots"
      ),
    "fit" = c(
      "data_mod", "iter",
      "p_dots", "dir_proj"
      ),
    "extract" = c(
      "data_raw", "iter",
      "data_mod", "dir_proj",
      "p_dots", "fit_obj"
    ),
    "display" = c(
      "data_raw", "iter", "data_mod", "dir_proj",
      "p_dots", "fit_obj", "fit_stats"
    ),
    "validate" = c(
      "data_raw", "iter", "data_mod", "dir_proj",
      "p_dots", "fit_obj"
    )
  )

  # check that functions have correct parameters, if not NULL
  # .validate_fns(env = env_main, expected_params = NULL)

  # save dot parameters and functions
  dir_params <- file.path(dir_proj, "params")
  if (identical(stages_to_force_vec, stage_vec)) {
    unlink(dir_params, recursive = TRUE)
  }
  stages_to_run_vec <- vapply(
    stage_vec,
    function(x) {
      if (x %in% stages_to_force_vec) {
        return(x)
      }
      completed_stage_indicator <- file.path(
        dir_proj, stage_to_basename_vec[x], "completed.rds"
      )
      ifelse(file.exists(completed_stage_indicator),
        "", x
      )
    },
    character(1)
  )
  stages_to_run_vec <- stages_to_run_vec[!stages_to_run_vec == ""] %>%
    setNames(NULL)

  stages_to_run_vec <- stages_to_run_vec[
    vapply(stages_to_run_vec, function(stg) {
      !is.null(fn_list[[stg]])
    }, logical(1))
  ]
  if (length(stages_to_run_vec) == 0) {
    message("no stages to run")

    if (!file.exists(file.path(dir_proj, "output.html"))) {
      message("creating output.html")
      # create rmd
      rmarkdown::render(
        input = system.file("extdata", "collate_output.Rmd",
          package = "pipeline"
        ),
        output_file = file.path(dir_proj, "output.html"),
        params = list(
          p_dots = p_dots,
          dir_proj = dir_proj
        ),
        quiet = TRUE
      )
    }

    message("run complete")

    return(invisible(dir_proj))
  }

  save_objects(
    obj_list = fn_list[stages_to_run_vec],
    dir_proj = dir_proj,
    dir_sub = "params"
  )

  if (!all(grepl(paste0("^all$|^none?$|",
    paste0(stage_vec_pattern, collapse = "|"),
    collapse = ""
  ), debug))) {
    stop(paste0(
      "debug must include only ",
      " include elements that ",
      "match at least the first three letters from:\n'",
      paste0(stage_vec, collapse = "', '"),
      ", 'all' or 'none'."
    ))
  }

  if (any(grepl("^all$|^none$", debug)) &&
    length(debug) > 1) {
    stop(paste0(
      "debug must only have ",
      "'all' or 'none' ",
      "on their own"
    ))
  }


  if (grepl("^all$", debug)) {
    fn_to_debug_vec <- stage_vec
  } else if (!all(grepl("^none$", debug))) {
    fn_to_debug_vec <- vapply(debug, function(stg) {
      stage_vec[vapply(
        stage_vec_pattern,
        function(x) grepl(x, stg), logical(1)
      )]
    }, character(1)) %>%
      setNames(NULL)
  } else {
    fn_to_debug_vec <- NULL
  }

  for (i in seq_along(fn_to_debug_vec)) {
    if (!is.function(fn_list[[fn_to_debug_vec[i]]])) next
    parse_text <- paste0(
      "debugonce(", fn_to_debug_vec[i], ")"
    )
    eval(parse(text = parse_text))
    parse_text <- paste0(
      "on.exit(try(suppressWarnings(undebug(",
      fn_to_debug_vec[i],
      ")), silent = TRUE), add = TRUE)"
    )
    eval(parse(text = parse_text))
  }

  # ====================================
  # Analysis
  # ====================================

  if (!"preprocess" %in% stages_to_run_vec) {
    data_mod <- readRDS(
      file.path(
        dir_proj,
        stage_to_basename_vec["preprocess"],
        "data_mod.rds"
      )
    )
  }
  if (!"fit" %in% stages_to_run_vec) {
    fit_obj <- readRDS(
      file.path(
        dir_proj,
        stage_to_basename_vec["fit"],
        "mod_list.rds"
      )
    )
  }
  if (!"extract" %in% stages_to_run_vec) {
    fit_stats <- readRDS(
      file.path(
        dir_proj,
        stage_to_basename_vec["extract"],
        "fit_stats.rds"
      )
    )
  }

  for (stg in stages_to_run_vec) {
    completion_indicator_path <- file.path(
      dir_proj,
      stage_to_basename_vec[stg],
      "completed.rds"
    )
    stg_to_nm_vec <- c(
      "preprocess" = "data_mod",
      "explore" = "x",
      "fit" = "fit_obj",
      "validate" = "x",
      "extract" = "fit_stats",
      "display" = "x"
    )
    nm <- stg_to_nm_vec[stg]
    dir_stg <- file.path(
      dir_proj,
      stage_to_basename_vec[stg]
    )
    unlink(
      dir_stg,
      recursive = TRUE
    )
    dir.create(
      dir_stg,
      recursive = TRUE
    )
    iter$dir_stg <- dir_stg

    parse_text <- paste0(
      nm,
      " <- ",
      stg,
      "(data_raw = data_raw, ",
      "dir_proj = dir_proj, ",
      "iter = iter, ",
      "p_dots = p_dots"
    )
    if (!stg == "preprocess") {
      parse_text <- paste0(parse_text, ", data_mod = data_mod")
    }

    if (stg %in% c("validate", "extract", "display")) {
      parse_text <- paste0(parse_text, ", fit_obj = fit_obj")
    }
    if (stg == "display") {
      parse_text <- paste0(parse_text, ", fit_stats = fit_stats")
    }
    parse_text <- paste0(parse_text, ")")
    eval(parse(text = parse_text))

    if (stg == "preprocess") {
      if (!file.exists(file.path(dir_stg, "data_mod.rds"))) {
        saveRDS(data_mod, file.path(dir_stg, "data_mod.rds"))
      }
    }

    saveRDS(TRUE, completion_indicator_path)
  }

  try(rm("x"), silent = TRUE)

  # create rmd
  message("creating output.html")
  # create rmd
  rmarkdown::render(
    input = system.file("extdata", "collate_output.Rmd",
      package = "pipeline"
    ),
    output_file = file.path(dir_proj, "output.html"),
    params = list(
      settings_to_print = settings_to_print,
      dir_proj = dir_proj
    ),
    quiet = TRUE
  )

  message("pipeline run complete")

  return(invisible(dir_proj))
}
