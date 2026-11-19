# Linked views (crosstalk) for px_*() figures: a SharedData given as
# `data_frame` links each trace to its rows' keys, with plotly.core's widget
# settings ("keys", "sets", "frameKeys"; see plotly.core::highlight()).

# Trace types whose points are the data frame's rows, so they can carry its
# keys (aggregating traces, such as pies, sunbursts and heatmaps, can't)
PX_KEYED_TYPES <- c(
  "scatter",
  "scattergl",
  "scatter3d",
  "scatterpolar",
  "scatterpolargl",
  "scatterternary",
  "scattergeo",
  "scattermap",
  "bar",
  "histogram",
  "box",
  "violin",
  "funnel",
  "choropleth",
  "choroplethmap",
  "splom"
)

# The column of px data frames holding the SharedData's keys
PX_KEY_COL <- ".plotly_crosstalk_key"

# A SharedData given as `data_frame`: its data, with its keys and group kept
# in args
px_shared_data <- function(args) {
  sd <- args$data_frame
  if (!inherits(sd, "SharedData")) {
    return(args)
  }
  args$data_frame <- sd$origData()
  args$crosstalk_key <- as.character(sd$key())
  args$crosstalk_group <- sd$groupName()
  args
}

# Keys of a px trace made from `trace_data`
px_trace_key <- function(args, ctor, spec, trace_data) {
  if (is.null(args$crosstalk_key) || !PX_KEY_COL %in% names(trace_data)) {
    return(NULL)
  }
  k <- as.character(trace_data[[PX_KEY_COL]])
  if ("trendline" %in% spec$attrs) {
    return(list(simple = unique(k)))
  }
  if (!ctor %in% PX_KEYED_TYPES) {
    return(NULL)
  }
  k
}

# Set the figure's keys from those of the px traces of each frame
# (`frame_keys`: per frame, a list with each trace's keys or NULL)
px_set_keys <- function(fig, args, frame_keys) {
  if (is.null(args$crosstalk_key)) {
    return(fig)
  }
  sets_of <- function(keys) {
    lapply(keys, function(k) if (!is.null(k)) args$crosstalk_group)
  }
  first <- if (length(frame_keys)) frame_keys[[1L]] else list()
  if (all(vapply(first, is.null, logical(1)))) {
    warning(
      "This kind of figure can't be linked with crosstalk: its traces don't ",
      "show the data frame's rows as points",
      call. = FALSE
    )
    return(fig)
  }
  n <- length(fig$data)
  fig <- set_widget_setting(fig, "keys", pad_list(first, n))
  fig <- set_widget_setting(fig, "sets", pad_list(sets_of(first), n))
  if (length(frame_keys) > 1L) {
    fig <- set_widget_setting(
      fig,
      "frameKeys",
      lapply(frame_keys, function(k) list(keys = k, sets = sets_of(k)))
    )
  }
  fig
}
