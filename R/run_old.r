run_old <- function(iter,
                dir_base,
                dir_var_exc = NULL,
                dir_var_encode = NULL,
                dir_var_fn = NULL,
                prep_data_raw = NULL,
                prep_iter = NULL,
                preprocess,
                explore = NULL,
                fit = NULL,
                validate = NULL,
                extract = NULL,
                display = NULL,
                delete_old_run = TRUE,
                debug = FALSE,
                force_rerun = "all",
                remove_non_run_projects = FALSE,
                ...) {

  if (nrow(iter) == 0L) {
    warning("iter has no rows")
    return(invisible(tibble::tibble()))
  }

  p_dots <- list(...)
  if (any(identical(names(p_dots), ""))) {
    stop("All dotted arguments must be named.")
  }

  results_tbl_init <- purrr::map_df(
    seq_len(nrow(iter)),
    function(i) {

      print(paste0(i, " of ", nrow(iter), " outer combinations"))

      iter_row <- iter[i, ]

      # data_raw
      # removed params$rmd - so must not be a part of iter
      if (is.null(prep_data_raw)) {
        data_raw_outer <- NULL
      } else {
        data_raw_outer <- prep_data_raw(
          iter = iter_row,
          p_dots = p_dots,
          stage = "outer"
        )
        if (nrow(data_raw_outer) == 0) {
          print_iter(
            iter = iter_row,
            ind = 1
          )
          message("no data, skipping")
          out_tbl <- tibble::tibble(
            dir_proj = NULL,
            iter = list(iter_row),
            p_dots = p_dots
          )
          return(out_tbl)
        }
      }

      # refine iter
      if (is.null(prep_iter)) {
        iter_outer <- iter_row
      } else {
        iter_outer <- prep_iter(
          iter = iter_row,
          p_dots = p_dots,
          data_raw = data_raw_outer
        )

        if (nrow(iter_outer) == 0) {
          print_iter(
            iter = iter_outer,
            ind = 1
          )
          message("no data, skipping")
          out_tbl <- tibble::tibble(
            dir_proj = NULL,
            iter = list(iter_outer),
            p_dots = p_dots
          )
          return(out_tbl)
        }
      }

      j <- 1
      purrr::map_df(seq_len(nrow(iter_outer)), function(j) {
        iter_inner <- iter_outer[j, ]
        print(paste0(j, " of ", nrow(iter_outer), " inner combinations"))

        # print iteration
        print_iter(
          iter = iter_inner,
          ind = ifelse(i == 1 && j == 1, i, max(i, j))
        )

        ind_exc <- which(purrr::map_lgl(
          colnames(iter_inner),
          function(x) x %in% dir_var_exc
        ))

        if (length(ind_exc) > 0) {
          iter_inner_dir <- iter_inner[, -ind_exc]
        } else {
          iter_inner_dir <- iter_inner
        }
        var_to_exc <- switch(as.character(is.null(dir_var_exc)),
          "TRUE" = "~none~",
          dir_var_exc
        )
        var_to_encode <- switch(
          as.character(is.null(dir_var_encode)),
          "TRUE" = "~none~",
          "FALSE" = switch(
            paste0(dir_var_encode, collapse = "", sep = ""),
            "~all~" = , # nolint
            "all" = colnames(iter_inner_dir),
            dir_var_encode
          )
        )
        var_to_fn <- switch(as.character(is.null(dir_var_fn)),
          "TRUE" = list(),
          dir_var_fn
        )

        dir_proj <- .create_dir_proj(
          dir_base = dir_base,
          iter = iter_inner,
          var_to_exc = var_to_exc,
          var_to_encode = var_to_encode,
          var_to_fn = var_to_fn,
          delete_pre_existing = delete_old_run
        )

        if (!is.null(prep_data_raw)) {
          data_raw_inner <- prep_data_raw(
            iter = iter_inner,
            p_dots = p_dots,
            stage = "inner",
            data_raw = data_raw_outer
          )

          if (nrow(data_raw_inner) == 0) {
            # print_iter(
            #  iter = it,
            #  ind = 1
            # )
            message("no data, skipping")
            out_tbl <- tibble::tibble(
              dir_proj = NULL,
              iter = iter
            )
            return(out_tbl)
          }
        } else {
          data_raw_inner <- NULL
        }

        # set "none" to NULL
        # iter_inner <- set_none_to_null(iter_inner)

        out_tbl <- .get_out_tbl(
          dir_base = dir_base,
          dir_proj = dir_proj,
          iter = iter_inner,
          p_dots = p_dots
        )

        # run
        try(.run(
          dir_proj = dir_proj,
          delete_old_run = delete_old_run,
          data_raw = data_raw_inner,
          preprocess = preprocess,
          explore = explore,
          fit = fit,
          validate = validate,
          extract = extract,
          display = display,
          debug = debug,
          force_rerun = force_rerun,
          settings_to_print = out_tbl %>%
            dplyr::select(-fn),
          iter = iter_inner,
          p_dots = p_dots
        ))

        out_tbl_inner <- tibble::tibble(
          dir_proj = dir_proj,
          iter = list(iter_inner),
          p_dots = p_dots
        )

        obj_to_remove_vec <- setdiff(ls(), "out_tbl")
        rm(obj_to_remove_vec)

        out_tbl
      })
    }
  )

  results_tbl <- purrr::map_df(
    seq_len(nrow(results_tbl_init)),
    function(i) {
      .get_out_tbl(
        dir_base = dir_base,
        dir_proj = results_tbl_init$dir_proj[[i]],
        iter = results_tbl_init$iter[[i]],
        p_dots = p_dots
      )
    }
  )

  if (remove_non_run_projects) {
    clean_dir(
      clean_dir = dir_base,
      keep_dir_sub = results_tbl$dir_proj,
      keep_base_name = "^exc~*\\.txt$",
      keep_base_name_strict = TRUE
    )
  }

  saveRDS(
    results_tbl,
    file.path(dir_save_base, "results_tbl.rds")
  )

  message("all runs complete")
  results_tbl
}
