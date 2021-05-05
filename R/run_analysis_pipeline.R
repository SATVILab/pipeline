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
#' @param preprocess_fn,plot_exp_fn,fit_fn,get_fit_stats_fn,plot_fit_fn,plot_val_fn functions or \code{NULL}. Functions
#' run in the order specified, passing data_raw, data_mod and params_dots from one to the other. Some functions
#' expect extra output.
#' @param data_raw dataframe. Contains raw data to be processed by \code{preprocess_fn} to create \code{data_mod}.
#' @param debug_live character vector or \code{FALSE}. Functions provided to \code{run_analysis_pipeline}
#' that at least partially match characters in \code{debug_live} at the start are run in \code{debugonce}.
#' @param ... Named arguments passed onto all functions (preprocess_fn to plot_val_fn).
#'
#' @export
run_analysis_pipeline <- function(dir_proj,
                                  dir_proj_empty = FALSE,
                                  data_raw,
                                  preprocess_fn = NULL,
                                  plot_exp_fn = NULL,
                                  fit_fn = NULL,
                                  get_fit_stats_fn = NULL,
                                  plot_fit_fn = NULL,
                                  plot_val_fn = NULL,
                                  debug_live = FALSE,
                                  ...){

  # ====================================
  # Preparation
  # ====================================

  # check that data is supplied
  if(missing(data_raw)) stop("data_raw must be supplied.")

  # create expected parameters
  # remember to change get_expected_params and its test when this is changed
  expected_params <- list("preprocess_fn" = c("data_raw", "params_dots", 'dir_proj'),
                          "plot_exp_fn" = c("data_raw", "data_mod", "dir_proj", "params_dots"),
                          'fit_fn' = c("data_mod", "params_dots", "dir_proj"),
                          'get_fit_stats_fn' = c("data_raw", "data_mod", "dir_proj",
                                                 "params_dots", "fit_obj"),
                          "plot_fit_fn" = c("data_raw", "data_mod", "dir_proj",
                                            "params_dots", "fit_obj", "fit_stats"),
                          "plot_val_fn" = c("data_raw", "data_mod", "dir_proj",
                                            "params_dots", "fit_obj"))

  # save current environment as a variable
  env_main <- environment()

  # collect dots
  params_dots <- rlang::list2(...)

  # get project directory, creating it if need be
  dir_proj <- .setup_proj_dir(dir_proj = dir_proj,
                              dir_proj_empty = dir_proj_empty,
                              params_dots = params_dots)

  # Convenience function to open folder inside project directory.
  # Overrides analysispipeline::open_folder
  open_folder <- function(path = dir_proj, ...) invisible(rstudioapi::selectFile(path = path, ...))
  open_dir <- function(path = dir_proj, ...) invisible(rstudioapi::selectFile(path = path, ...))

  # replace NULL functions
  .replace_null_fns(env = env_main, expected_params = expected_params)

  # check that functions have correct parameters, if not NULL
  .validate_fns(env = env_main, expected_params = NULL)

  # save dot parameters and functions
  save_objects(params_dots = params_dots,
               preprocess_fn = preprocess_fn,
               plot_exp_fn = plot_exp_fn, fit_fn = fit_fn,
               get_fit_stats_fn = get_fit_stats_fn,
               plot_fit_fn = plot_fit_fn, plot_val_fn = plot_val_fn,
               dir_proj = dir_proj, dir_sub = c("params"),
               empty = TRUE)

  # debug function if requested


  # debug all functions
  if(!identical(debug, FALSE)){
    on.exit(try(undebug(preprocess_fn), silent = TRUE), add = TRUE)
    on.exit(try(undebug(plot_exp_fn), silent = TRUE), add = TRUE)
    on.exit(try(undebug(fit_fn), silent = TRUE), add = TRUE)
    on.exit(try(undebug(get_fit_stats_fn), silent = TRUE), add = TRUE)
    on.exit(try(undebug(plot_fit_fn), silent = TRUE), add = TRUE)
    on.exit(try(undebug(plot_val_fn), silent = TRUE), add = TRUE)
  }

  .add_debug(fn_name = names(expected_params),
             debug_live = debug_live, env = env_main)


  # pre-process data
  data_mod <- preprocess_fn(data_raw = data_raw, params_dots = params_dots,
                            dir_proj = dir_proj)

  # ====================================
  # Analysis
  # ====================================

  # make exploratory plots
  plot_exp_fn(data_raw = data_raw, data_mod = data_mod,
              dir_proj = dir_proj,
              params_dots = params_dots)

  # fit model
  fit_obj <- fit_fn(data_mod = data_mod, params_dots = params_dots,
                    dir_proj = dir_proj)

  # make validation plots
  plot_val_fn(data_raw = data_raw, data_mod = data_mod,
              dir_proj = dir_proj, fit_obj = fit_obj,
              params_dots = params_dots)

  # tables
  fit_stats <- get_fit_stats_fn(data_raw = data_raw, data_mod = data_mod,
                                fit_obj = fit_obj, params_dots = params_dots,
                                dir_proj = dir_proj)


  # make results plots
  plot_fit_fn(data_raw = data_raw, data_mod = data_mod,
              dir_proj = dir_proj, fit_obj = fit_obj,
              params_dots = params_dots, fit_stats = fit_stats)

  # create rmd
  rmarkdown::render(input = system.file('extdata', 'collate_output.Rmd',
                                        package = 'analysispipeline'),
                    output_file = file.path(dir_proj, "output.Rmd"),
                    params = lsit(params_dots = params_dots,
                                  dir_proj = dir_proj))

  message('pipeline run complete')
  invisible(TRUE)
}
