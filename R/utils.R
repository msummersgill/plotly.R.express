`%||%` <- function(x, y) if (is.null(x)) y else x

# "numeric vector", "character", ... for error messages
value_type <- function(v) {
  if (is.factor(v)) {
    return("factor")
  }
  cls <- class(v)[[1L]]
  if (is.atomic(v) && length(v) != 1L && !is.matrix(v)) {
    paste0(cls, " vector")
  } else {
    cls
  }
}

stats_named <- function(x, nm) {
  names(x) <- nm
  x
}
