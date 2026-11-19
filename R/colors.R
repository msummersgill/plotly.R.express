#' Color sequences and scales
#'
#' plotly.py's colour sequences and scales, as `px.colors` in plotly.py: the
#' same object as [plotly.core::plotly_colors]. `px_colors$qualitative` holds
#' discrete colour sequences (e.g. `Plotly`, `D3`, `Set1`), for
#' `color_discrete_sequence`; `px_colors$sequential`, `$diverging` and
#' `$cyclical` hold continuous colour scales (e.g. `Viridis`, `RdBu`,
#' `Twilight`), for `color_continuous_scale`.
#'
#' @format A list of four named lists of character vectors of colours.
#' @export
#' @examples
#' names(px_colors$qualitative)
#' px_colors$sequential$Viridis
px_colors <- plotly.core::plotly_colors
