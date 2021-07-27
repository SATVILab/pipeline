#' @title Print the expected parameters for each function
#'
#' @description
#' Print the names of each function along with the respective expected parameters.
#'
#' @export
get_expected_params <- function(){
  expected_params <- list("preprocess_fn" = c("data_raw", "p_dots", 'dir_proj'),
                          "plot_exp_fn" = c("data_raw", "data_mod", "dir_proj", "p_dots"),
                          'fit_fn' = c("data_mod", "p_dots", "dir_proj"),
                          'get_fit_stats_fn' = c("data_raw", "data_mod", "dir_proj",
                                                 "p_dots", "fit_obj"),
                          "plot_fit_fn" = c("data_raw", "data_mod", "dir_proj",
                                            "p_dots", "fit_obj", "fit_stats"),
                          "plot_val_fn" = c("data_raw", "data_mod", "dir_proj",
                                            "p_dots", "fit_obj"))
  expected_params
}
