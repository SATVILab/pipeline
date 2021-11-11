#' @title Replace NULL functions
#'
#' @description
#' Replaces functions that are NULL or missing in a specified environment with functions whose body is \code{NULL; NULL} and
#' have empty arguments as specified.
#'
#' @param env environment. Environment in which the functions are to be found and replaced.
#' @param expected_params named list. Names are names of functions in \code{env} and values
#' are character vectors specifying parameters for functions.
#'
#' @examples
#' env_curr <- environment()
#' fn1 <- function(x) x
#' fn2 <- function(x = 2) y
#' fn3 <- NULL
#' expected_params <- list(fn1 = "x", fn2 = "y", fn3 = "x")
#' analysispipeline:::.replace_null_fns(env = env_curr, expected_params = expected_params)
#' fn3 # replaced to have a function with arguments as specified by expected_params
#' fn2
#' fn1 # no change in either
.replace_null_fns <- function(env, expected_params) {
  purrr::walk(names(expected_params), function(fn_name) {
    .replace_null_fn(
      fn_name = fn_name, env = env,
      expected_params = expected_params[[fn_name]]
    )
  })
  invisible(TRUE)
}

#' @title Replace NULL function of a given name
#'
#' @inheritParams .replace_null_fns
#' @param fn_name character. Name of single function.
#' @param env environment. Environment in which function is found.
.replace_null_fn <- function(fn_name, env, expected_params) {
  # get function from environment
  fn <- env[[fn_name]]

  # return fn if it's not NULL
  if (!is.null(fn)) {
    return(invisible(TRUE))
  }

  # create base function
  if (fn_name == "preprocess_fn") {
    fn <- function() {
      NULL
      env$data_raw
    }
  } else {
    fn <- function() {
      NULL
      NULL
    }
  }

  # change parameters of function to what's required
  formals(fn) <- purrr::map(seq_along(expected_params), function(x) NULL) %>%
    setNames(expected_params)

  # assign function to environment
  assign(x = fn_name, value = fn, envir = env)

  # output
  invisible(TRUE)
}
