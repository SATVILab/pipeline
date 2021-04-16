test_that("get_expected_params() matches expected_params in run_analysis_pipeline",{
  expect_identical(get_expected_params(),
                   list("preprocess_fn" = c("data_raw", "params_dots", 'dir_proj'),
                        "plot_exp_fn" = c("data_raw", "data_mod", "dir_proj", "params_dots"),
                        'fit_fn' = c("data_mod", "params_dots", "dir_proj"),
                        'get_fit_stats_fn' = c("data_raw", "data_mod", "dir_proj",
                                               "params_dots", "fit_obj"),
                        "plot_fit_fn" = c("data_raw", "data_mod", "dir_proj",
                                          "params_dots", "fit_obj", "fit_stats"),
                        "plot_val_fn" = c("data_raw", "data_mod", "dir_proj",
                                          "params_dots", "fit_obj")))
})
