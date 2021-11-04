      .get_out_tbl <- function(dir_proj) {
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
        out_tbl
      }