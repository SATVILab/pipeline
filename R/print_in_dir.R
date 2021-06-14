#' @title Output code to print plots in a directory in a Knitr document
#' @export
print_plots_in_dir <- function(dir_plot, level = 3, format = c("png", "pdf")){
  # get plot paths
  plot_path_vec <- purrr::map(format, function(x){
    out <- list.files(dir_plot, full.names = TRUE, pattern = x)
    if(length(out) == 0) return(NULL)
    out
  }) %>%
    purrr::compact() %>%
    unlist()

  plot_name_vec <- purrr::map(format, function(x){
    out <- list.files(dir_plot, full.names = FALSE, pattern = x)
    if(length(out) == 0) return(NULL)
    out
  }) %>%
    purrr::compact() %>%
    unlist() %>%
    stringr::str_sub(end = -5)

  # print plots for each batch
  for(i in seq_along(plot_name_vec)){
    # print fcs name
    pander::pandoc.header(plot_name_vec[i], level = level)
    # print plot
    cat(paste0("![](", plot_path_vec[i], ")"), "\n")
  }
  invisible(TRUE)
}

#' @title Output code to print tables in a directory in a Knitr document
#'
#' @export
print_tbl_in_dir <- function(dir_tbl, level = 3){
  obj_list <- return_rds(dir_tbl)
  obj_list <- obj_list[purrr::map_lgl(obj_list, function(x) is.data.frame(x))]
  if(length(obj_list) == 0) return(invisible(TRUE))
  for(i in seq_along(obj_list)){
    pander::pandoc.header(names(obj_list)[i], level = level)
    pander::pandoc.table(obj_list[[i]], split.tables = Inf, split.cells = Inf)
  }
}


#' @title Return RDS objects in a folder
#'
#' @return A named list, where the names are the names of the files and
#' the elements are the RDS objects. If there are no RDS files in folder,
#' then an empty list is returned.
#'
#' @export
return_rds <- function(dir_obj){
  fn_vec <- list.files(dir_obj, pattern = 'rds')
  if(length(fn_vec) == 0) return(list())

  obj_list <- purrr::map(fn_vec, function(fn){
    readRDS(file.path(dir_obj, fn))
  }) %>%
    setNames(stringr::str_remove(fn_vec, ".rds"))
  obj_list
}
