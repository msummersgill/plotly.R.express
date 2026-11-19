#' @keywords internal
"_PACKAGE"

#' @import plotly.core
#' @importFrom data.table :=
#' @importFrom utils globalVariables
NULL

# Let data.table syntax work inside this namespace.
.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  reset_px_defaults()
}

# data.table's .SD, used in px hierarchy aggregation
utils::globalVariables(c(".SD", ".I", ".N"))
