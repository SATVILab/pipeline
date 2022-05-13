
.fortify_var <- function(elem) {
  if (is.null(elem)) return(list(NULL))
  if (identical(list(NULL), elem)) return(elem)
  if (!is.list(elem) && is.vector(elem)) {
    elem_out <- lapply(elem, function(x) {
      list(
        cn_orig = x,
        cn_new = x,
        label = x,
        trans = NULL,
        expansion = NULL
      )
    })
  } else if (is.list(elem)) {
    if (any(vapply(elem, Negate(is.list), FUN.VALUE = logical(1)))) {
      stop(
        "Variables must be specified as a list of lists",
        call. = FALSE
      )
    }
    elem_out <- lapply(elem, function(x) {
      if (!("cn_orig" %in% names(x))) {
        stop("cn_orig not supplied")
      }
      if (!"cn_new" %in% names(x)) {
        x[["cn_new"]] <- setNames(unlist(x[["cn_orig"]]), NULL)
      }
      if (!"label" %in% names(x)) {
        x[["label"]] <- setNames(unlist(x[["cn_orig"]]), NULL)
      }
      if (!"trans" %in% names(x)) x <- append(x, list("trans" = NULL))
      if (!"expansion" %in% names(x)) x <- append(x, list("expansion" = NULL))
      if (!is.null(x[["expansion"]])) {
        stopifnot("fn" %in% names(x$expansion))
        if ("args" %in% names(x$expansion)) stopifnot(is.list(x$expansion$args))
        extra_elems <- setdiff(names(x$expansion), c("pkg", "fn", "args"))
        if (length(extra_elems) > 0) {
          stop(
            paste0(
              "The following are extra arguments to expansion: ",
              paste0(extra_elems, collapse = "; ")
            )
          )
        }
      }
      if (!is.null(x$trans)) stopifnot(is.function(x$trans))
      x[c("cn_orig", "cn_new", "label", "trans", "expansion")]
    })
  } else {
    stop("elem not either a list or a vector (or NULL)")
  }
  elem_out
}
