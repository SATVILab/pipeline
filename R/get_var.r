get_var <- function(dep,
                    exp = NULL,
                    conf = NULL,
                    re = NULL,
                    offset = NULL,
                    non_mod = NULL,
                    interaction = NULL,
                    combn = "cross",
                    tidy_models = FALSE,
                    ...) {
  if (missing(dep)) stop("dep must be supplied")
  if (is.null(dep)) stop("dep must be non-NULL")
  if (is.list(dep)) {
    null_any <- any(unlist(lapply(dep, function(x) {
      if (is.null(x)) {
        return(TRUE)
      }
      if (identical(x, list(NULL))) {
        return(TRUE)
      }
      identical(x, list(list(NULL)))
    })))
    if (null_any) {
      stop("No element for dep may be NULL or a NULL list")
    }
    if (is.character(dep[[1]]) && length(dep) == 1) {
      dep <- list(as.list(dep[[1]]))
    }
  }
  # convert NULL elements to lists of length
  # 1 with one element a list of just NULL
  # to prevent working with NULL a lot
  if (is.null(exp)) exp <- list(list(NULL))
  if (is.null(conf)) conf <- list(list(NULL))
  if (is.null(re)) re <- list(list(NULL))
  if (is.null(offset)) offset <- list(list(NULL))
  if (is.null(non_mod)) non_mod <- list(list(NULL))
  # convert character vectors to a a list of length 1
  # to enforce that they're a part of the same model
  if (is.character(exp)) exp <- list(exp)
  if (is.character(conf)) conf <- list(conf)
  if (is.character(re)) re <- list(re)
  if (is.character(offset)) offset <- list(offset)
  if (is.character(non_mod)) non_mod <- list(non_mod)

  # convert to full form
  var_list <- list(
    dep = lapply(dep, .fortify_vars_mod),
    exp = lapply(exp, .fortify_vars_mod),
    conf = lapply(conf, .fortify_vars_mod),
    re = lapply(re, .fortify_vars_mod),
    offset = lapply(offset, .fortify_vars_mod),
    non_mod = lapply(non_mod, .fortify_vars_mod)
  )
  var <- purrr::cross_df(var_list)

  # check for interactions
  add_interaction(var = var, interaction = interaction)
}

.fortify_vars_mod <- function(x) {
  lapply(x, function(.x) {
    if (is.null(.x)) {
      if (identical(x, list(NULL))) {
        return(get_var_ind_list(list(cn_orig = NULL)))
      } else {
        stop(".x is NULL but combined with non-NULL variables")
      }
    }
    if (identical(.x, list(NULL))) {
      if (identical(x, list(list(NULL)))) {
        return(get_var_ind_list(list(cn_orig = NULL)))
      } else {
        stop(".x is list(NULL) but combined with non-NULL variables")
      }
    }
    switch(class(.x)[length(class(.x))],
        "character" = get_var_ind_chr(.x),
        "list" = get_var_ind_list(.x),
        stop(".x has class ", paste0(class(.x), collapse = " & "), " and not just character or list") # nolint
      )
  })
}

#' @export
get_var_ind_list <- function(x) {
  if (!is.list(x)) {
    stop("x must be a list")
  }
  if (!("cn_orig" %in% names(x))) {
    stop("cn_orig must be supplied")
  }
  if (!"cn_new" %in% names(x)) {
    x <- append(x, stats::setNames(x["cn_orig"], "cn_new"))
  }
  if (!"label" %in% names(x)) {
    x <- append(x, stats::setNames(x["cn_orig"], "label"))
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
  x[c("cn_orig", "cn_new", "label", "trans", "expansion")]
}

#' @export
get_var_ind_chr <- function(x) {
  if (!is.character(x)) {
    stop("x must be a character")
  }
  get_var_ind_list(list(cn_orig = x))
}

add_interaction <- function(var, interaction = NULL) {
  
  # add interaction
  if (is.list(interaction)) {
    interaction <- interaction[vapply(interaction, Negate(is.null), logical(1))]
  }
  if (length(interaction) > 0) {
    if (is.character(interaction)) {
      interaction <- list(interaction)
    }
    interaction <- lapply(interaction, function(x) {
      if (length(x) <= 1) {
        return(NULL)
      }
      x
    })
    interaction <- interaction[vapply(interaction, Negate(is.null), logical(1))]
  }

  if (is.null(interaction)) interaction <- interaction <- list(NULL)
  if ((is.list(interaction) || is.vector(interaction)) && (length(interaction) == 0 || identical(interaction, ""))) { # nolint
      interaction <- list(NULL)
    }
  if (identical(list(NULL), interaction)) {
    var[["interaction"]] <- lapply(seq_len(nrow(var)), function(x) list(NULL))
  } else {
    var[["interaction"]] <- lapply(seq_len(nrow(var)), function(i) {
      exp_cn_new <- vapply(
        var[i, ]$exp[[1]], function(x) x$cn_new, character(1)
      )
      int_list_add <- lapply(interaction, function(elem) {
        found <- FALSE; n_tested <- 0; elem_add <- NULL
        while (!found | n_tested < length(interaction)) {
          found <- vapply(interaction[[n_tested + 1]], function(elem) {
            elem %in% exp_cn_new
          }, logical(1)) |>
            all()
          if (found) elem_add <- elem
          n_tested <- n_tested + 1
        }
        elem_add
      })
      int_list_add <- int_list_add[
        vapply(int_list_add, Negate(is.null), logical(1))
      ]
      if (length(int_list_add) == 0) int_list_add <- list(NULL)
      int_list_add
    })
  }
  var
}