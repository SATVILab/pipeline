#' @title Check that functions have correct parameters
#'
#' @description
#' Each function supplied to \code{run} must have certain parameters.
#' They alaways include \code{p_dots}.
#'
#' @param env environment. Environment in which the functions are found and to be saved.
#' @param expected_params list. Names are names of functions to be validated and values are expected parameters.
#' If \code{NULL}, then a default list is used. Run \code{body(analysispipeline:::.validate_fns)} to see default.
#' Default is \code{NULL}.
#'
#' @examples
#' env_curr <- environment()
#' fn1 <- function(x) x; fn2 <- function(x) y
#' expected_params <- list(fn1 = 'x', fn2 = 'y')
#' # error
#' testthat::expect_error(analysispipeline:::.validate_fns(env = env_curr,
#'                                  expected_params = expected_params))
#'
#' # no error given if fn is NULL
#' fn2 <- NULL
#' analysispipeline:::.validate_fns(env = env_curr,
#'                                  expected_params = expected_params)
.validate_fns <- function(env,
                          expected_params = NULL){
  purrr::walk(names(expected_params), function(fn_name){
    .validate_fn(fn_name = fn_name, expected_params = expected_params[[fn_name]],
                 env = env)
  })

  invisible(TRUE)
}

#' @title Validate parameters for individual functions
#'
#' @inheritParams .validate_fns
#' @param fn_name character. Name of single function.
.validate_fn <- function(fn_name, expected_params = NULL, env){

  fn <- env[[fn_name]]
  if(is.null(fn)) return(invisible(TRUE))
  formals_list <- formals(fn)
  missing_params <- purrr::map(expected_params, function(x){
    if(!x %in% names(formals_list)) return(x)
    NULL
  }) %>%
    purrr::compact()
  if(!length(missing_params) == 0) stop(paste0(fn_name, " is missing the following params: ",
                                               paste0(missing_params, collapse = " and ")))
  invisible(TRUE)
}