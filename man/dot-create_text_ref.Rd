% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_dir_proj.r
\name{.create_text_ref}
\alias{.create_text_ref}
\title{Create directory addition}
\usage{
.create_text_ref(elem, max_len = 40, collapse = "")
}
\arguments{
\item{elem}{some R object.
If the object is named (i.e. names(elem) is not \code{NULL}),
then the name is the addition
(multiple names are concatenated to a length-one
character vector).
If the object is not named, then the value
itself is coerced to a character vector of length 1.
If this does not work, an error is returned.}
}
\description{
Create addition to add to directory.
The basic behaviour is that either an object's
elements are named (e.g. \code{c("a" = 1)} instead of
the unnamed \code{c(1)}),
in which case the name(s) creates the sub-directory,
or else the object is not named (e.g \code{c(1)}) above)
in which case the
}
\examples{
.create_text_ref(c("a" = "b"))
.create_text_ref(c("b"))
.create_text_ref(list("a" = "b"))
.create_text_ref(list("b"))
.create_text_ref(list("a" = function(x) x))
# will create an error if a function is passed not
# in a named list
try(.create_text_ref(list(function(x) x)))
# it will automatically "lift up" elements
# that are lower down:r
.create_text_ref(list(list("a")))
.create_text_ref(list(list("a", "b")))
.create_text_ref(list(list("a", "b", function(x) x)))
}
