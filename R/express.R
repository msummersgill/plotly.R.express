# Plotly Express functions (the R counterpart of plotly/express/_chart_types.py).
# Signatures and argument documentation follow plotly.py; see R/express-core.R and
# R/express-figure.R for the shared implementation.

#' Scatter plot
#'
#' In a scatter plot, each row of `data_frame` is represented by a symbol
#' mark in 2D space.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_iris() |>
#'   px_scatter(x = ~sepal_width, y = ~sepal_length, color = ~species,
#'              size = ~petal_length, hover_data = ~petal_width)
#' px_data_tips() |> px_scatter(x = ~total_bill, y = ~tip, facet_col = ~time, trendline = "ols")
px_scatter <- function(
  data_frame = NULL,
  x = NULL,
  y = NULL,
  color = NULL,
  symbol = NULL,
  size = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  text = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  error_x = NULL,
  error_x_minus = NULL,
  error_y = NULL,
  error_y_minus = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  orientation = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  symbol_sequence = NULL,
  symbol_map = NULL,
  opacity = NULL,
  size_max = NULL,
  marginal_x = NULL,
  marginal_y = NULL,
  trendline = NULL,
  trendline_options = NULL,
  trendline_color_override = NULL,
  trendline_scope = "trace",
  log_x = FALSE,
  log_y = FALSE,
  range_x = NULL,
  range_y = NULL,
  render_mode = "auto",
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "scatter")
}

#' Density contour plot
#'
#' In a density contour plot, rows of `data_frame` are grouped together into
#' contour marks to visualize the 2D distribution of an aggregate function
#' `histfunc` (e.g. the count or sum) of the value `z`.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_tips() |> px_density_contour(x = ~total_bill, y = ~tip)
px_density_contour <- function(
  data_frame = NULL,
  x = NULL,
  y = NULL,
  z = NULL,
  color = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  hover_name = NULL,
  hover_data = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  orientation = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  marginal_x = NULL,
  marginal_y = NULL,
  trendline = NULL,
  trendline_options = NULL,
  trendline_color_override = NULL,
  trendline_scope = "trace",
  log_x = FALSE,
  log_y = FALSE,
  range_x = NULL,
  range_y = NULL,
  histfunc = NULL,
  histnorm = NULL,
  nbinsx = NULL,
  nbinsy = NULL,
  text_auto = FALSE,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "histogram2dcontour",
    trace_patch = list(
      contours = list(coloring = "none"),
      histfunc = histfunc,
      histnorm = histnorm,
      nbinsx = nbinsx,
      nbinsy = nbinsy,
      xbingroup = "x",
      ybingroup = "y"
    )
  )
}

#' Density heatmap
#'
#' In a density heatmap, rows of `data_frame` are grouped together into
#' colored rectangular tiles to visualize the 2D distribution of an aggregate
#' function `histfunc` (e.g. the count or sum) of the value `z`.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_tips() |> px_density_heatmap(x = ~total_bill, y = ~tip, marginal_x = "histogram")
px_density_heatmap <- function(
  data_frame = NULL,
  x = NULL,
  y = NULL,
  z = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  hover_name = NULL,
  hover_data = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  orientation = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  marginal_x = NULL,
  marginal_y = NULL,
  opacity = NULL,
  log_x = FALSE,
  log_y = FALSE,
  range_x = NULL,
  range_y = NULL,
  histfunc = NULL,
  histnorm = NULL,
  nbinsx = NULL,
  nbinsy = NULL,
  text_auto = FALSE,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "histogram2d",
    trace_patch = list(
      histfunc = histfunc,
      histnorm = histnorm,
      nbinsx = nbinsx,
      nbinsy = nbinsy,
      xbingroup = "x",
      ybingroup = "y"
    )
  )
}

#' Line plot
#'
#' In a 2D line plot, each row of `data_frame` is represented as a vertex of
#' a polyline mark in 2D space.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_gapminder()[continent == "Oceania"] |>
#'   px_line(x = ~year, y = ~lifeExp, color = ~country, markers = TRUE)
px_line <- function(
  data_frame = NULL,
  x = NULL,
  y = NULL,
  line_group = NULL,
  color = NULL,
  line_dash = NULL,
  symbol = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  text = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  error_x = NULL,
  error_x_minus = NULL,
  error_y = NULL,
  error_y_minus = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  orientation = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  line_dash_sequence = NULL,
  line_dash_map = NULL,
  symbol_sequence = NULL,
  symbol_map = NULL,
  markers = FALSE,
  log_x = FALSE,
  log_y = FALSE,
  range_x = NULL,
  range_y = NULL,
  line_shape = NULL,
  render_mode = "auto",
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "scatter")
}

#' Stacked area chart
#'
#' In a stacked area plot, each row of `data_frame` is represented as a
#' vertex of a polyline mark in 2D space. The area between successive
#' polylines is filled.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_gapminder() |>
#'   px_area(x = ~year, y = ~pop, color = ~continent, line_group = ~country)
px_area <- function(
  data_frame = NULL,
  x = NULL,
  y = NULL,
  line_group = NULL,
  color = NULL,
  pattern_shape = NULL,
  symbol = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  text = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  pattern_shape_sequence = NULL,
  pattern_shape_map = NULL,
  symbol_sequence = NULL,
  symbol_map = NULL,
  markers = FALSE,
  orientation = NULL,
  groupnorm = NULL,
  log_x = FALSE,
  log_y = FALSE,
  range_x = NULL,
  range_y = NULL,
  line_shape = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "scatter",
    trace_patch = list(stackgroup = "1", mode = "lines", groupnorm = groupnorm)
  )
}

#' Bar chart
#'
#' In a bar plot, each row of `data_frame` is represented as a rectangular
#' mark.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_tips() |> px_bar(x = ~day, y = ~total_bill, color = ~sex, barmode = "group")
px_bar <- function(
  data_frame = NULL,
  x = NULL,
  y = NULL,
  color = NULL,
  pattern_shape = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  text = NULL,
  base = NULL,
  error_x = NULL,
  error_x_minus = NULL,
  error_y = NULL,
  error_y_minus = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  color_continuous_scale = NULL,
  pattern_shape_sequence = NULL,
  pattern_shape_map = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  opacity = NULL,
  orientation = NULL,
  barmode = "relative",
  log_x = FALSE,
  log_y = FALSE,
  range_x = NULL,
  range_y = NULL,
  text_auto = FALSE,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "bar",
    trace_patch = list(textposition = "auto"),
    layout_patch = list(barmode = barmode)
  )
}

#' Timeline (Gantt chart)
#'
#' In a timeline plot, each row of `data_frame` is represented as a
#' rectangular mark on an x axis of type `date`, spanning from `x_start` to
#' `x_end`.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' data.frame(task = c("A", "B", "C"),
#'            start = c("2009-01-01", "2009-03-05", "2009-02-20"),
#'            finish = c("2009-02-28", "2009-04-15", "2009-05-30")) |>
#'   px_timeline(x_start = ~start, x_end = ~finish, y = ~task)
px_timeline <- function(
  data_frame = NULL,
  x_start = NULL,
  x_end = NULL,
  y = NULL,
  color = NULL,
  pattern_shape = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  text = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  pattern_shape_sequence = NULL,
  pattern_shape_map = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  opacity = NULL,
  range_x = NULL,
  range_y = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "timeline",
    trace_patch = list(textposition = "auto", orientation = "h"),
    layout_patch = list(barmode = "overlay")
  )
}

#' Histogram
#'
#' In a histogram, rows of `data_frame` are grouped together into a
#' rectangular mark to visualize the 1D distribution of an aggregate function
#' `histfunc` (e.g. the count or sum) of the value `y` (or `x` if
#' `orientation` is `"h"`).
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_tips() |> px_histogram(x = ~total_bill, color = ~sex, marginal = "rug", nbins = 20)
px_histogram <- function(
  data_frame = NULL,
  x = NULL,
  y = NULL,
  color = NULL,
  pattern_shape = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  hover_name = NULL,
  hover_data = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  pattern_shape_sequence = NULL,
  pattern_shape_map = NULL,
  marginal = NULL,
  opacity = NULL,
  orientation = NULL,
  barmode = "relative",
  barnorm = NULL,
  histnorm = NULL,
  log_x = FALSE,
  log_y = FALSE,
  range_x = NULL,
  range_y = NULL,
  histfunc = NULL,
  cumulative = NULL,
  nbins = NULL,
  text_auto = FALSE,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "histogram",
    trace_patch = list(
      histnorm = histnorm,
      histfunc = histfunc,
      cumulative = list(enabled = cumulative)
    ),
    layout_patch = list(barmode = barmode, barnorm = barnorm)
  )
}

#' Empirical cumulative distribution
#'
#' In a Empirical Cumulative Distribution Function (ECDF) plot, rows of
#' `data_frame` are sorted by the value `x` (or `y` if `orientation` is
#' `"h"`) and their cumulative count (or the cumulative sum of `y` if
#' supplied and `orientation` is `h`) is drawn as a line.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_tips() |> px_ecdf(x = ~total_bill, color = ~sex)
px_ecdf <- function(
  data_frame = NULL,
  x = NULL,
  y = NULL,
  color = NULL,
  text = NULL,
  line_dash = NULL,
  symbol = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  hover_name = NULL,
  hover_data = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  markers = FALSE,
  lines = TRUE,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  line_dash_sequence = NULL,
  line_dash_map = NULL,
  symbol_sequence = NULL,
  symbol_map = NULL,
  marginal = NULL,
  opacity = NULL,
  orientation = NULL,
  ecdfnorm = "probability",
  ecdfmode = "standard",
  render_mode = "auto",
  log_x = FALSE,
  log_y = FALSE,
  range_x = NULL,
  range_y = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "scatter")
}

#' Violin plot
#'
#' In a violin plot, rows of `data_frame` are grouped together into a curved
#' mark to visualize their distribution.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_tips() |> px_violin(y = ~total_bill, x = ~day, color = ~smoker, box = TRUE)
px_violin <- function(
  data_frame = NULL,
  x = NULL,
  y = NULL,
  color = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  orientation = NULL,
  violinmode = NULL,
  log_x = FALSE,
  log_y = FALSE,
  range_x = NULL,
  range_y = NULL,
  points = NULL,
  box = FALSE,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "violin",
    trace_patch = list(
      points = points,
      box = list(visible = box),
      scalegroup = "True",
      x0 = " ",
      y0 = " "
    ),
    layout_patch = list(violinmode = violinmode)
  )
}

#' Box plot
#'
#' In a box plot, rows of `data_frame` are grouped together into a
#' box-and-whisker mark to visualize their distribution. Each box spans from
#' quartile 1 (Q1) to quartile 3 (Q3). The second quartile (Q2) is marked by
#' a line inside the box. By default, the whiskers correspond to the box'
#' edges +/- 1.5 times the interquartile range (IQR: Q3-Q1), see "points" for
#' other options.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_tips() |> px_box(x = ~time, y = ~total_bill, points = "all")
px_box <- function(
  data_frame = NULL,
  x = NULL,
  y = NULL,
  color = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  orientation = NULL,
  boxmode = NULL,
  log_x = FALSE,
  log_y = FALSE,
  range_x = NULL,
  range_y = NULL,
  points = NULL,
  notched = FALSE,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "box",
    trace_patch = list(
      boxpoints = points,
      notched = notched,
      x0 = " ",
      y0 = " "
    ),
    layout_patch = list(boxmode = boxmode)
  )
}

#' Strip plot
#'
#' In a strip plot each row of `data_frame` is represented as a jittered mark
#' within categories.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_tips() |> px_strip(x = ~total_bill, y = ~day, color = ~sex)
px_strip <- function(
  data_frame = NULL,
  x = NULL,
  y = NULL,
  color = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  orientation = NULL,
  stripmode = NULL,
  log_x = FALSE,
  log_y = FALSE,
  range_x = NULL,
  range_y = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "box",
    trace_patch = list(
      boxpoints = "all",
      pointpos = 0,
      hoveron = "points",
      fillcolor = "rgba(255,255,255,0)",
      line = list(color = "rgba(255,255,255,0)"),
      x0 = " ",
      y0 = " "
    ),
    layout_patch = list(boxmode = stripmode)
  )
}

#' 3D scatter plot
#'
#' In a 3D scatter plot, each row of `data_frame` is represented by a symbol
#' mark in 3D space.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_iris() |>
#'   px_scatter_3d(x = ~sepal_length, y = ~sepal_width, z = ~petal_width, color = ~species)
px_scatter_3d <- function(
  data_frame = NULL,
  x = NULL,
  y = NULL,
  z = NULL,
  color = NULL,
  symbol = NULL,
  size = NULL,
  text = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  error_x = NULL,
  error_x_minus = NULL,
  error_y = NULL,
  error_y_minus = NULL,
  error_z = NULL,
  error_z_minus = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  size_max = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  symbol_sequence = NULL,
  symbol_map = NULL,
  opacity = NULL,
  log_x = FALSE,
  log_y = FALSE,
  log_z = FALSE,
  range_x = NULL,
  range_y = NULL,
  range_z = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "scatter3d")
}

#' 3D line plot
#'
#' In a 3D line plot, each row of `data_frame` is represented as a vertex of
#' a polyline mark in 3D space.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_gapminder()[continent == "Europe"] |>
#'   px_line_3d(x = ~gdpPercap, y = ~pop, z = ~year, color = ~country)
px_line_3d <- function(
  data_frame = NULL,
  x = NULL,
  y = NULL,
  z = NULL,
  color = NULL,
  line_dash = NULL,
  text = NULL,
  line_group = NULL,
  symbol = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  error_x = NULL,
  error_x_minus = NULL,
  error_y = NULL,
  error_y_minus = NULL,
  error_z = NULL,
  error_z_minus = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  line_dash_sequence = NULL,
  line_dash_map = NULL,
  symbol_sequence = NULL,
  symbol_map = NULL,
  markers = FALSE,
  log_x = FALSE,
  log_y = FALSE,
  log_z = FALSE,
  range_x = NULL,
  range_y = NULL,
  range_z = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "scatter3d")
}

#' Ternary scatter plot
#'
#' In a ternary scatter plot, each row of `data_frame` is represented by a
#' symbol mark in ternary coordinates.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_election() |>
#'   px_scatter_ternary(a = ~Joly, b = ~Coderre, c = ~Bergeron, color = ~winner)
px_scatter_ternary <- function(
  data_frame = NULL,
  a = NULL,
  b = NULL,
  c = NULL,
  color = NULL,
  symbol = NULL,
  size = NULL,
  text = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  symbol_sequence = NULL,
  symbol_map = NULL,
  opacity = NULL,
  size_max = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "scatterternary")
}

#' Ternary line plot
#'
#' In a ternary line plot, each row of `data_frame` is represented as a
#' vertex of a polyline mark in ternary coordinates.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_election()[1:5] |> px_line_ternary(a = ~Joly, b = ~Coderre, c = ~Bergeron)
px_line_ternary <- function(
  data_frame = NULL,
  a = NULL,
  b = NULL,
  c = NULL,
  color = NULL,
  line_dash = NULL,
  line_group = NULL,
  symbol = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  text = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  line_dash_sequence = NULL,
  line_dash_map = NULL,
  symbol_sequence = NULL,
  symbol_map = NULL,
  markers = FALSE,
  line_shape = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "scatterternary")
}

#' Polar scatter plot
#'
#' In a polar scatter plot, each row of `data_frame` is represented by a
#' symbol mark in polar coordinates.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_wind() |> px_scatter_polar(r = ~frequency, theta = ~direction, color = ~strength)
px_scatter_polar <- function(
  data_frame = NULL,
  r = NULL,
  theta = NULL,
  color = NULL,
  symbol = NULL,
  size = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  text = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  symbol_sequence = NULL,
  symbol_map = NULL,
  opacity = NULL,
  direction = "clockwise",
  start_angle = 90,
  size_max = NULL,
  range_r = NULL,
  range_theta = NULL,
  log_r = FALSE,
  render_mode = "auto",
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "scatterpolar")
}

#' Polar line plot
#'
#' In a polar line plot, each row of `data_frame` is represented as a vertex
#' of a polyline mark in polar coordinates.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_wind() |>
#'   px_line_polar(r = ~frequency, theta = ~direction, color = ~strength, line_close = TRUE)
px_line_polar <- function(
  data_frame = NULL,
  r = NULL,
  theta = NULL,
  color = NULL,
  line_dash = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  line_group = NULL,
  text = NULL,
  symbol = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  line_dash_sequence = NULL,
  line_dash_map = NULL,
  symbol_sequence = NULL,
  symbol_map = NULL,
  markers = FALSE,
  direction = "clockwise",
  start_angle = 90,
  line_close = FALSE,
  line_shape = NULL,
  render_mode = "auto",
  range_r = NULL,
  range_theta = NULL,
  log_r = FALSE,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "scatterpolar")
}

#' Polar bar chart
#'
#' In a polar bar plot, each row of `data_frame` is represented as a wedge
#' mark in polar coordinates.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_wind() |> px_bar_polar(r = ~frequency, theta = ~direction, color = ~strength)
px_bar_polar <- function(
  data_frame = NULL,
  r = NULL,
  theta = NULL,
  color = NULL,
  pattern_shape = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  base = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  color_continuous_scale = NULL,
  pattern_shape_sequence = NULL,
  pattern_shape_map = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  barnorm = NULL,
  barmode = "relative",
  direction = "clockwise",
  start_angle = 90,
  range_r = NULL,
  range_theta = NULL,
  log_r = FALSE,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "barpolar",
    layout_patch = list(barnorm = barnorm, barmode = barmode)
  )
}

#' Choropleth map
#'
#' In a choropleth map, each row of `data_frame` is represented by a colored
#' region mark on a map.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_gapminder(year = 2007) |>
#'   px_choropleth(locations = ~iso_alpha, color = ~lifeExp, hover_name = ~country)
px_choropleth <- function(
  data_frame = NULL,
  lat = NULL,
  lon = NULL,
  locations = NULL,
  locationmode = NULL,
  geojson = NULL,
  featureidkey = NULL,
  color = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  projection = NULL,
  scope = NULL,
  center = NULL,
  fitbounds = NULL,
  basemap_visible = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "choropleth",
    trace_patch = list(locationmode = locationmode)
  )
}

#' Scatter plot on a geographic map
#'
#' In a geographic scatter plot, each row of `data_frame` is represented by a
#' symbol mark on a map.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_gapminder(year = 2007) |>
#'   px_scatter_geo(locations = ~iso_alpha, size = ~pop, color = ~continent)
px_scatter_geo <- function(
  data_frame = NULL,
  lat = NULL,
  lon = NULL,
  locations = NULL,
  locationmode = NULL,
  geojson = NULL,
  featureidkey = NULL,
  color = NULL,
  text = NULL,
  symbol = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  size = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  symbol_sequence = NULL,
  symbol_map = NULL,
  opacity = NULL,
  size_max = NULL,
  projection = NULL,
  scope = NULL,
  center = NULL,
  fitbounds = NULL,
  basemap_visible = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "scattergeo",
    trace_patch = list(locationmode = locationmode)
  )
}

#' Line plot on a geographic map
#'
#' In a geographic line plot, each row of `data_frame` is represented as a
#' vertex of a polyline mark on a map.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_gapminder(year = 2007)[continent == "Oceania"] |>
#'   px_line_geo(locations = ~iso_alpha)
px_line_geo <- function(
  data_frame = NULL,
  lat = NULL,
  lon = NULL,
  locations = NULL,
  locationmode = NULL,
  geojson = NULL,
  featureidkey = NULL,
  color = NULL,
  line_dash = NULL,
  text = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  line_group = NULL,
  symbol = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  line_dash_sequence = NULL,
  line_dash_map = NULL,
  symbol_sequence = NULL,
  symbol_map = NULL,
  markers = FALSE,
  projection = NULL,
  scope = NULL,
  center = NULL,
  fitbounds = NULL,
  basemap_visible = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "scattergeo",
    trace_patch = list(locationmode = locationmode)
  )
}

#' Scatter plot on a tile map
#'
#' In a scatter map, each row of `data_frame` is represented by a symbol mark
#' on the map.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_carshare() |>
#'   px_scatter_map(lat = ~centroid_lat, lon = ~centroid_lon, color = ~peak_hour,
#'                  size = ~car_hours, zoom = 10)
px_scatter_map <- function(
  data_frame = NULL,
  lat = NULL,
  lon = NULL,
  color = NULL,
  text = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  size = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  opacity = NULL,
  size_max = NULL,
  zoom = NULL,
  center = NULL,
  map_style = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "scattermap")
}

#' Choropleth map (tile-based)
#'
#' In a choropleth map, each row of `data_frame` is represented by a colored
#' region on the map.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_election() |>
#'   px_choropleth_map(geojson = px_data_election_geojson(), locations = ~district,
#'                     featureidkey = "properties.district", color = ~Bergeron,
#'                     center = list(lat = 45.5517, lon = -73.7073), zoom = 9)
px_choropleth_map <- function(
  data_frame = NULL,
  geojson = NULL,
  featureidkey = NULL,
  locations = NULL,
  color = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  opacity = NULL,
  zoom = NULL,
  center = NULL,
  map_style = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "choroplethmap")
}

#' Density map
#'
#' In a density map, each row of `data_frame` contributes to the intensity of
#' the color of the region around the corresponding point on the map.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_carshare() |>
#'   px_density_map(lat = ~centroid_lat, lon = ~centroid_lon, z = ~car_hours, radius = 10)
px_density_map <- function(
  data_frame = NULL,
  lat = NULL,
  lon = NULL,
  z = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  opacity = NULL,
  zoom = NULL,
  center = NULL,
  map_style = NULL,
  radius = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "densitymap", trace_patch = list(radius = radius))
}

#' Line plot on a tile map
#'
#' In a line map, each row of `data_frame` is represented as a vertex of a
#' polyline mark on the map.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_carshare()[1:20] |>
#'   px_line_map(lat = ~centroid_lat, lon = ~centroid_lon, zoom = 10)
px_line_map <- function(
  data_frame = NULL,
  lat = NULL,
  lon = NULL,
  color = NULL,
  text = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  line_group = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  zoom = NULL,
  center = NULL,
  map_style = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "scattermap")
}

#' Scatter plot matrix
#'
#' In a scatter plot matrix (or SPLOM), each row of `data_frame` is
#' represented by a multiple symbol marks, one in each cell of a grid of 2D
#' scatter plots, which plot each pair of `dimensions` against each other.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_iris() |>
#'   px_scatter_matrix(dimensions = ~c(sepal_width, sepal_length, petal_width), color = ~species)
px_scatter_matrix <- function(
  data_frame = NULL,
  dimensions = NULL,
  color = NULL,
  symbol = NULL,
  size = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  symbol_sequence = NULL,
  symbol_map = NULL,
  opacity = NULL,
  size_max = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "splom", layout_patch = list(dragmode = "select"))
}

#' Parallel coordinates plot
#'
#' In a parallel coordinates plot, each row of `data_frame` is represented by
#' a polyline mark which traverses a set of parallel axes, one for each of
#' the `dimensions`.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_iris() |>
#'   px_parallel_coordinates(color = ~species_id,
#'                           dimensions = ~c(sepal_width, sepal_length, petal_width, petal_length))
px_parallel_coordinates <- function(
  data_frame = NULL,
  dimensions = NULL,
  color = NULL,
  labels = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "parcoords")
}

#' Parallel categories plot
#'
#' In a parallel categories (or parallel sets) plot, each row of `data_frame`
#' is grouped with other rows that share the same values of `dimensions` and
#' then plotted as a polyline mark through a set of parallel axes, one for
#' each of the `dimensions`.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_tips() |> px_parallel_categories(dimensions = ~c(sex, smoker, day), color = ~size)
px_parallel_categories <- function(
  data_frame = NULL,
  dimensions = NULL,
  color = NULL,
  labels = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL,
  dimensions_max_cardinality = 50
) {
  args <- as.list(environment())
  make_figure(args, "parcats")
}

#' Pie chart
#'
#' In a pie plot, each row of `data_frame` is represented as a sector of a
#' pie.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_tips() |> px_pie(values = ~tip, names = ~day, hole = 0.3)
px_pie <- function(
  data_frame = NULL,
  names = NULL,
  values = NULL,
  color = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  category_orders = NULL,
  labels = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL,
  opacity = NULL,
  hole = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "pie",
    trace_patch = list(showlegend = !is.null(names), hole = hole),
    layout_patch = if (!is.null(color_discrete_sequence)) {
      list(piecolorway = color_discrete_sequence)
    } else {
      list()
    }
  )
}

#' Sunburst chart
#'
#' A sunburst plot represents hierarchial data as sectors laid out over
#' several levels of concentric rings.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_tips() |> px_sunburst(path = ~c(day, time, sex), values = ~total_bill)
px_sunburst <- function(
  data_frame = NULL,
  names = NULL,
  values = NULL,
  parents = NULL,
  path = NULL,
  ids = NULL,
  color = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  labels = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL,
  branchvalues = NULL,
  maxdepth = NULL
) {
  args <- as.list(environment())
  px_hierarchy(
    args,
    "sunburst",
    path,
    ids,
    parents,
    branchvalues,
    maxdepth,
    color_discrete_sequence
  )
}

#' Treemap
#'
#' A treemap plot represents hierarchial data as nested rectangular sectors.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_gapminder(year = 2007) |>
#'   px_treemap(path = list(px_constant("world"), ~continent, ~country),
#'              values = ~pop, color = ~lifeExp)
px_treemap <- function(
  data_frame = NULL,
  names = NULL,
  values = NULL,
  parents = NULL,
  ids = NULL,
  path = NULL,
  color = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  labels = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL,
  branchvalues = NULL,
  maxdepth = NULL
) {
  args <- as.list(environment())
  px_hierarchy(
    args,
    "treemap",
    path,
    ids,
    parents,
    branchvalues,
    maxdepth,
    color_discrete_sequence
  )
}

#' Icicle chart
#'
#' An icicle plot represents hierarchial data with adjoined rectangular
#' sectors that all cascade from root down to leaf in one direction.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_data_tips() |> px_icicle(path = ~c(day, time, sex), values = ~total_bill)
px_icicle <- function(
  data_frame = NULL,
  names = NULL,
  values = NULL,
  parents = NULL,
  path = NULL,
  ids = NULL,
  color = NULL,
  color_continuous_scale = NULL,
  range_color = NULL,
  color_continuous_midpoint = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  labels = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL,
  branchvalues = NULL,
  maxdepth = NULL
) {
  args <- as.list(environment())
  px_hierarchy(
    args,
    "icicle",
    path,
    ids,
    parents,
    branchvalues,
    maxdepth,
    color_discrete_sequence
  )
}

#' Funnel chart
#'
#' In a funnel plot, each row of `data_frame` is represented as a rectangular
#' sector of a funnel.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' data.frame(stage = c("Visit", "Download", "Sign up", "Purchase"),
#'            number = c(39, 27.4, 20.6, 11)) |>
#'   px_funnel(x = ~number, y = ~stage)
px_funnel <- function(
  data_frame = NULL,
  x = NULL,
  y = NULL,
  color = NULL,
  facet_row = NULL,
  facet_col = NULL,
  facet_col_wrap = 0,
  facet_row_spacing = NULL,
  facet_col_spacing = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  text = NULL,
  animation_frame = NULL,
  animation_group = NULL,
  category_orders = NULL,
  labels = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  opacity = NULL,
  orientation = NULL,
  log_x = FALSE,
  log_y = FALSE,
  range_x = NULL,
  range_y = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL
) {
  args <- as.list(environment())
  make_figure(args, "funnel")
}

#' Funnel area chart
#'
#' In a funnel area plot, each row of `data_frame` is represented as a
#' trapezoidal sector of a funnel.
#'
#' @inheritParams px_args
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' data.frame(stage = c("Visit", "Download", "Sign up", "Purchase"),
#'            number = c(39, 27.4, 20.6, 11)) |>
#'   px_funnel_area(values = ~number, names = ~stage)
px_funnel_area <- function(
  data_frame = NULL,
  names = NULL,
  values = NULL,
  color = NULL,
  color_discrete_sequence = NULL,
  color_discrete_map = NULL,
  hover_name = NULL,
  hover_data = NULL,
  custom_data = NULL,
  labels = NULL,
  title = NULL,
  subtitle = NULL,
  template = NULL,
  width = NULL,
  height = NULL,
  opacity = NULL
) {
  args <- as.list(environment())
  make_figure(
    args,
    "funnelarea",
    trace_patch = list(showlegend = !is.null(names)),
    layout_patch = if (!is.null(color_discrete_sequence)) {
      list(funnelareacolorway = color_discrete_sequence)
    } else {
      list()
    }
  )
}
