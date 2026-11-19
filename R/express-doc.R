# Shared argument documentation for the px_*() functions (from plotly.py's
# plotly/express/_doc.py, adapted to R).

#' Plotly Express arguments
#'
#' Arguments shared by the Plotly Express functions; see [px_scatter()] and the other
#' functions in the family.
#'
#' @param data_frame A data.frame, data.table, list or matrix. Needed for
#'   column names to be used in the other arguments; if `NULL`, a data frame is
#'   built from the vectors given in the other arguments.
#' @param x A column name in `data_frame`, a vector of values, a one-sided
#'   formula evaluated in `data_frame` (e.g. `~ log(pop)`), [px_constant()] or
#'   [px_range()]. Values from this column or vector are used to position marks
#'   along the x axis in cartesian coordinates.
#' @param y A column name in `data_frame`, a vector of values, a one-sided
#'   formula evaluated in `data_frame` (e.g. `~ log(pop)`), [px_constant()] or
#'   [px_range()]. Values from this column or vector are used to position marks
#'   along the y axis in cartesian coordinates.
#' @param color A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to assign color to marks.
#' @param symbol A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to assign symbols to marks.
#' @param size A column name in `data_frame`, a vector of values, a one-sided
#'   formula evaluated in `data_frame` (e.g. `~ log(pop)`), [px_constant()] or
#'   [px_range()]. Values from this column or vector are used to assign mark
#'   sizes.
#' @param hover_name A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector appear
#'   in bold in the hover tooltip.
#' @param hover_data Extra data to show in the hover tooltip: column names (a
#'   character vector or a formula such as `~c(a, b)`), or a named list with column names as names and values
#'   `TRUE` (default formatting), `FALSE` (hide the column, e.g. one shown by
#'   default), a d3-format string such as `":.3f"`, a vector of data, or a list
#'   of a format string or `TRUE` and a vector of data.
#' @param custom_data Column names (a character vector or a formula such as
#'   `~c(a, b)`), or a list of column names, formulas and vectors. Values from these columns are added to the traces'
#'   `customdata` (e.g. for use in Shiny events) without appearing in the hover
#'   tooltip.
#' @param text A column name in `data_frame`, a vector of values, a one-sided
#'   formula evaluated in `data_frame` (e.g. `~ log(pop)`), [px_constant()] or
#'   [px_range()]. Values from this column or vector appear in the figure as
#'   text labels.
#' @param facet_row A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to assign marks to facetted subplots in the vertical direction.
#' @param facet_col A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to assign marks to facetted subplots in the horizontal direction.
#' @param facet_col_wrap Maximum number of facet columns. Wraps the column
#'   variable at this width, so that the column facets span multiple rows.
#'   Ignored if 0, and forced to 0 if `facet_row` or a `marginal` is set.
#' @param facet_row_spacing Spacing between facet rows, in paper units.
#'   Default is 0.03 or 0.07 when facet_col_wrap is used.
#' @param facet_col_spacing Spacing between facet columns, in paper units
#'   Default is 0.02.
#' @param error_x A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to size x-axis error bars. If `error_x_minus` is `NULL`, error bars
#'   will be symmetrical, otherwise `error_x` is used for the positive
#'   direction only.
#' @param error_x_minus A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to size x-axis error bars in the negative direction. Ignored if
#'   `error_x` is `NULL`.
#' @param error_y A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to size y-axis error bars. If `error_y_minus` is `NULL`, error bars
#'   will be symmetrical, otherwise `error_y` is used for the positive
#'   direction only.
#' @param error_y_minus A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to size y-axis error bars in the negative direction. Ignored if
#'   `error_y` is `NULL`.
#' @param animation_frame A column name in `data_frame`, a vector of values,
#'   a one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to assign marks to animation frames.
#' @param animation_group A column name in `data_frame`, a vector of values,
#'   a one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to provide object-constancy across animation frames: rows with
#'   matching `animation_group`s will be treated as if they describe the same
#'   object in each frame.
#' @param category_orders Named list mapping column names to the order of
#'   their values in axes, legends and facets, e.g. `list(day = c("Thur",
#'   "Fri", "Sat", "Sun"))`. By default values appear in the order first
#'   encountered, or in level order for factors.
#' @param labels Named character vector or list mapping column names to the
#'   labels to use for axis titles, legend entries and hover text, e.g.
#'   `c(gdpPercap = "GDP per capita")`.
#' @param orientation (default `"v"` if `x` and `y` are provided and both
#'   continuous or both categorical,  otherwise `"v"`(`"h"`) if `x`(`y`) is
#'   categorical and `y`(`x`) is continuous,  otherwise `"v"`(`"h"`) if only
#'   `x`(`y`) is provided)
#' @param color_discrete_sequence Character vector of colors, e.g.
#'   `px_colors$qualitative$Set1`. Values of `color` are assigned colors in
#'   order (cycling), unless set in `color_discrete_map`. Defaults to the
#'   template's `colorway`.
#' @param color_discrete_map Named character vector or list mapping values of
#'   `color` to colors, overriding `color_discrete_sequence`. If the values of
#'   `color` are colors, `px_identity()` (or `"identity"`) uses them directly.
#' @param color_continuous_scale Character vector of colors, e.g.
#'   `px_colors$sequential$Viridis`, or the name of a scale in [px_colors].
#'   Used when `color` is numeric. Defaults to the template's sequential
#'   colorscale.
#' @param range_color If provided, overrides auto-scaling on the continuous
#'   color scale.
#' @param color_continuous_midpoint If set, computes the bounds of the
#'   continuous color scale to have the desired midpoint. Setting this value is
#'   recommended when using `plotly.express.colors.diverging` color scales as
#'   the inputs to `color_continuous_scale`.
#' @param symbol_sequence Strings should define valid plotly.js symbols. When
#'   `symbol` is set, values in that column are assigned symbols by cycling
#'   through `symbol_sequence` in the order described in `category_orders`,
#'   unless the value of `symbol` is a key in `symbol_map`.
#' @param symbol_map Named character vector or list mapping values of
#'   `symbol` to marker symbols, overriding `symbol_sequence`. `px_identity()`
#'   uses the values of `symbol` directly.
#' @param opacity Value between 0 and 1. Sets the opacity for markers.
#' @param size_max Set the maximum mark size when using `size`.
#' @param marginal_x One of `"rug"`, `"box"`, `"violin"`, or `"histogram"`.
#'   If set, a horizontal subplot is drawn above the main plot, visualizing the
#'   x-distribution.
#' @param marginal_y One of `"rug"`, `"box"`, `"violin"`, or `"histogram"`.
#'   If set, a vertical subplot is drawn to the right of the main plot,
#'   visualizing the y-distribution.
#' @param trendline One of `"ols"` (ordinary least squares regression),
#'   `"lowess"` (locally weighted smoothing), `"rolling"`, `"expanding"` or
#'   `"ewm"` (exponentially weighted moving average). A trendline is drawn for
#'   each discrete color/symbol group; configure it with `trendline_options`.
#'   Fitted OLS models are available from [px_get_trendline_results()].
#' @param trendline_options Named list of options for the trendline function:
#'   for `"ols"`, `add_constant`, `log_x` and `log_y`; for `"lowess"`, `frac`;
#'   for `"rolling"`, `window`, `min_periods`, `center` and `function`
#'   (`"mean"`, `"median"`, `"sum"`, `"min"`, `"max"`, `"std"`, `"var"`,
#'   `"count"`); for `"expanding"`, `min_periods` and `function`; for `"ewm"`,
#'   one of `com`, `span`, `halflife` or `alpha`, and `adjust`.
#' @param trendline_color_override Valid CSS color. If provided, and if
#'   `trendline` is set, all trendlines will be drawn in this color rather than
#'   in the same color as the traces from which they draw their inputs.
#' @param trendline_scope If `"trace"`, then one trendline is drawn per trace
#'   (i.e. per color, symbol, facet, animation frame etc) and if `"overall"`
#'   then one trendline is computed for the entire dataset, and replicated
#'   across all facets.
#' @param log_x If `TRUE`, the x-axis is log-scaled in cartesian coordinates.
#' @param log_y If `TRUE`, the y-axis is log-scaled in cartesian coordinates.
#' @param range_x If provided, overrides auto-scaling on the x-axis in
#'   cartesian coordinates.
#' @param range_y If provided, overrides auto-scaling on the y-axis in
#'   cartesian coordinates.
#' @param render_mode One of `"auto"`, `"svg"` or `"webgl"`, default `"auto"`
#'   Controls the browser API used to draw marks. `"svg"` is appropriate for
#'   figures of less than 1000 data points, and will allow for fully-vectorized
#'   output. `"webgl"` is likely necessary for acceptable performance above
#'   1000 points but rasterizes part of the output.  `"auto"` uses heuristics
#'   to choose the mode.
#' @param title The figure title.
#' @param subtitle The figure subtitle.
#' @param template A template name (see [template_names()]) or a template
#'   list. Defaults to [default_template()].
#' @param width The figure width in pixels.
#' @param height The figure height in pixels.
#' @param z A column name in `data_frame`, a vector of values, a one-sided
#'   formula evaluated in `data_frame` (e.g. `~ log(pop)`), [px_constant()] or
#'   [px_range()]. Values from this column or vector are used to position marks
#'   along the z axis in cartesian coordinates.
#' @param histfunc One of `"count"`, `"sum"`, `"avg"`, `"min"`, or `"max"`.
#'   Function used to aggregate values for summarization (note: can be
#'   normalized with `histnorm`).
#' @param histnorm One of `"percent"`, `"probability"`, `"density"`, or
#'   `"probability density"` If `NULL`, the output of `histfunc` is used as is.
#'   If `"probability"`, the output of `histfunc` for a given bin is divided by
#'   the sum of the output of `histfunc` for all bins. If `"percent"`, the
#'   output of `histfunc` for a given bin is divided by the sum of the output
#'   of `histfunc` for all bins and multiplied by 100. If `"density"`, the
#'   output of `histfunc` for a given bin is divided by the size of the bin. If
#'   `"probability density"`, the output of `histfunc` for a given bin is
#'   normalized such that it corresponds to the probability that a random event
#'   whose distribution is described by the output of `histfunc` will fall into
#'   that bin.
#' @param nbinsx Positive integer. Sets the number of bins along the x axis.
#' @param nbinsy Positive integer. Sets the number of bins along the y axis.
#' @param text_auto If `TRUE` or a string, the x or y or z values will be
#'   displayed as text, depending on the orientation A string like `".2f"` will
#'   be interpreted as a `texttemplate` numeric formatting directive.
#' @param line_group A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to group rows of `data_frame` into lines.
#' @param line_dash A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to assign dash-patterns to lines.
#' @param line_dash_sequence Strings should define valid plotly.js
#'   dash-patterns. When `line_dash` is set, values in that column are assigned
#'   dash-patterns by cycling through `line_dash_sequence` in the order
#'   described in `category_orders`, unless the value of `line_dash` is a key
#'   in `line_dash_map`.
#' @param line_dash_map Named character vector or list mapping values of
#'   `line_dash` to dash patterns, overriding `line_dash_sequence`.
#'   `px_identity()` uses the values of `line_dash` directly.
#' @param markers If `TRUE`, markers are shown on lines.
#' @param line_shape One of `"linear"`, `"spline"`, `"hv"`, `"vh"`, `"hvh"`,
#'   or `"vhv"`
#' @param pattern_shape A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to assign pattern shapes to marks.
#' @param pattern_shape_sequence Strings should define valid plotly.js
#'   patterns-shapes. When `pattern_shape` is set, values in that column are
#'   assigned patterns-shapes by cycling through `pattern_shape_sequence` in
#'   the order described in `category_orders`, unless the value of
#'   `pattern_shape` is a key in `pattern_shape_map`.
#' @param pattern_shape_map Named character vector or list mapping values of
#'   `pattern_shape` to pattern shapes, overriding `pattern_shape_sequence`.
#'   `px_identity()` uses the values directly.
#' @param groupnorm One of `"fraction"` or `"percent"`. If `"fraction"`, the
#'   value of each point is divided by the sum of all values at that location
#'   coordinate. `"percent"` is the same but multiplied by 100 to show
#'   percentages. `NULL` will stack up all values at each location coordinate.
#' @param base A column name in `data_frame`, a vector of values, a one-sided
#'   formula evaluated in `data_frame` (e.g. `~ log(pop)`), [px_constant()] or
#'   [px_range()]. Values from this column or vector are used to position the
#'   base of the bar.
#' @param barmode One of `"group"`, `"overlay"` or `"relative"` In
#'   `"relative"` mode, bars are stacked above zero for positive values and
#'   below zero for negative values. In `"overlay"` mode, bars are drawn on top
#'   of one another. In `"group"` mode, bars are placed beside each other.
#' @param x_start A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. (required) Values from this column or
#'   vector are used to position marks along the x axis in cartesian
#'   coordinates.
#' @param x_end A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. (required) Values from this column or
#'   vector are used to position marks along the x axis in cartesian
#'   coordinates.
#' @param marginal One of `"rug"`, `"box"`, `"violin"`, or `"histogram"`. If
#'   set, a subplot is drawn alongside the main plot, visualizing the
#'   distribution.
#' @param barnorm One of `"fraction"` or `"percent"`. If `"fraction"`, the
#'   value of each bar is divided by the sum of all values at that location
#'   coordinate. `"percent"` is the same but multiplied by 100 to show
#'   percentages. `NULL` will stack up all values at each location coordinate.
#' @param cumulative If `TRUE`, histogram values are cumulative.
#' @param nbins Positive integer. Sets the number of bins.
#' @param lines If `FALSE`, lines are not drawn (forced to `TRUE` if
#'   `markers` is `FALSE`).
#' @param ecdfnorm One of `"probability"` or `"percent"` If `NULL`, values
#'   will be raw counts or sums. If `"probability", values will be
#'   probabilities normalized from 0 to 1. If `"percent", values will be
#'   percentages normalized from 0 to 100.
#' @param ecdfmode One of `"standard"`, `"complementary"` or `"reversed"` If
#'   `"standard"`, the ECDF is plotted such that values represent data at or
#'   below the point. If `"complementary"`, the CCDF is plotted such that
#'   values represent data above the point. If `"reversed"`, a variant of the
#'   CCDF is plotted such that values represent data at or above the point.
#' @param violinmode One of `"group"` or `"overlay"` In `"overlay"` mode,
#'   violins are on drawn top of one another. In `"group"` mode, violins are
#'   placed beside each other.
#' @param points One of `"outliers"`, `"suspectedoutliers"`, `"all"`, or
#'   `FALSE`. If `"outliers"`, only the sample points lying outside the
#'   whiskers are shown. If `"suspectedoutliers"`, all outlier points are shown
#'   and those less than 4*Q1-3*Q3 or greater than 4*Q3-3*Q1 are highlighted
#'   with the marker's `"outliercolor"`. If `"outliers"`, only the sample
#'   points lying outside the whiskers are shown. If `"all"`, all sample points
#'   are shown. If `FALSE`, no sample points are shown and the whiskers extend
#'   to the full range of the sample.
#' @param box If `TRUE`, boxes are drawn inside the violins.
#' @param boxmode One of `"group"` or `"overlay"` In `"overlay"` mode, boxes
#'   are on drawn top of one another. In `"group"` mode, boxes are placed
#'   beside each other.
#' @param notched If `TRUE`, boxes are drawn with notches.
#' @param stripmode One of `"group"` or `"overlay"` In `"overlay"` mode,
#'   strips are on drawn top of one another. In `"group"` mode, strips are
#'   placed beside each other.
#' @param error_z A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to size z-axis error bars. If `error_z_minus` is `NULL`, error bars
#'   will be symmetrical, otherwise `error_z` is used for the positive
#'   direction only.
#' @param error_z_minus A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to size z-axis error bars in the negative direction. Ignored if
#'   `error_z` is `NULL`.
#' @param log_z If `TRUE`, the z-axis is log-scaled in cartesian coordinates.
#' @param range_z If provided, overrides auto-scaling on the z-axis in
#'   cartesian coordinates.
#' @param a A column name in `data_frame`, a vector of values, a one-sided
#'   formula evaluated in `data_frame` (e.g. `~ log(pop)`), [px_constant()] or
#'   [px_range()]. Values from this column or vector are used to position marks
#'   along the a axis in ternary coordinates.
#' @param b A column name in `data_frame`, a vector of values, a one-sided
#'   formula evaluated in `data_frame` (e.g. `~ log(pop)`), [px_constant()] or
#'   [px_range()]. Values from this column or vector are used to position marks
#'   along the b axis in ternary coordinates.
#' @param c A column name in `data_frame`, a vector of values, a one-sided
#'   formula evaluated in `data_frame` (e.g. `~ log(pop)`), [px_constant()] or
#'   [px_range()]. Values from this column or vector are used to position marks
#'   along the c axis in ternary coordinates.
#' @param r A column name in `data_frame`, a vector of values, a one-sided
#'   formula evaluated in `data_frame` (e.g. `~ log(pop)`), [px_constant()] or
#'   [px_range()]. Values from this column or vector are used to position marks
#'   along the radial axis in polar coordinates.
#' @param theta A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to position marks along the angular axis in polar coordinates.
#' @param direction One of "`counterclockwise"` or `"clockwise"`. Default is
#'   `"clockwise"` Sets the direction in which increasing values of the angular
#'   axis are drawn.
#' @param start_angle Sets start angle for the angular axis, with 0 being due
#'   east and 90 being due north.
#' @param range_r If provided, overrides auto-scaling on the radial axis in
#'   polar coordinates.
#' @param range_theta If provided, overrides auto-scaling on the angular axis
#'   in polar coordinates.
#' @param log_r If `TRUE`, the radial axis is log-scaled in polar
#'   coordinates.
#' @param line_close If `TRUE`, an extra line segment is drawn between the
#'   first and last point.
#' @param lat A column name in `data_frame`, a vector of values, a one-sided
#'   formula evaluated in `data_frame` (e.g. `~ log(pop)`), [px_constant()] or
#'   [px_range()]. Values from this column or vector are used to position marks
#'   according to latitude on a map.
#' @param lon A column name in `data_frame`, a vector of values, a one-sided
#'   formula evaluated in `data_frame` (e.g. `~ log(pop)`), [px_constant()] or
#'   [px_range()]. Values from this column or vector are used to position marks
#'   according to longitude on a map.
#' @param locations A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are to
#'   be interpreted according to `locationmode` and mapped to
#'   longitude/latitude.
#' @param locationmode One of "ISO-3", "USA-states", or 'country names'
#'   Determines the set of locations used to match entries in `locations` to
#'   regions on the map.
#' @param geojson GeoJSON as a list (e.g. from [jsonlite::read_json()]), with
#'   features whose ids are referenced by `locations`.
#' @param featureidkey Path to field in GeoJSON feature object with which to
#'   match the values passed in to `locations`.The most common alternative to
#'   the default is of the form `'properties.<key>`.
#' @param projection One of `"equirectangular"`, `"mercator"`,
#'   `"orthographic"`, `"natural earth"`, `"kavrayskiy7"`, `"miller"`,
#'   `"robinson"`, `"eckert4"`, `"azimuthal equal area"`, `"azimuthal
#'   equidistant"`, `"conic equal area"`, `"conic conformal"`, `"conic
#'   equidistant"`, `"gnomonic"`, `"stereographic"`, `"mollweide"`, `"hammer"`,
#'   `"transverse mercator"`, `"albers usa"`, `"winkel tripel"`, `"aitoff"`, or
#'   `"sinusoidal"`Default depends on `scope`.
#' @param scope One of `"world"`, `"usa"`, `"europe"`, `"asia"`, `"africa"`,
#'   `"north america"`, or `"south america"`Default is `"world"` unless
#'   `projection` is set to `"albers usa"`, which forces `"usa"`.
#' @param center Dict keys are `"lat"` and `"lon"` Sets the center point of
#'   the map.
#' @param fitbounds One of `FALSE`, `locations` or `geojson`.
#' @param basemap_visible Force the basemap visibility.
#' @param zoom Between 0 and 20. Sets initial map zoom level. Higher values
#'   are more zoomed-in. If not provided, the map will select its own zoom
#'   level.
#' @param map_style Identifier of base map style. Allowed values are
#'   `"basic"`, `"carto-darkmatter"`, `"carto-darkmatter-nolabels"`,
#'   `"carto-positron"`, `"carto-positron-nolabels"`, `"carto-voyager"`,
#'   `"carto-voyager-nolabels"`, `"dark"`, `"light"`, `"open-street-map"`,
#'   `"outdoors"`, `"satellite"`, `"satellite-streets"`, `"streets"`,
#'   `"white-bg"`.
#' @param radius Sets the radius of influence of each point.
#' @param dimensions Column names in `data_frame` (a character vector or a
#'   formula such as `~c(a, b)`), or a list of column names, formulas and
#'   vectors. Values from these columns are used for
#'   multidimensional visualization.
#' @param dimensions_max_cardinality When `dimensions` is `NULL` and
#'   `data_frame` is provided, columns with more than this number of unique
#'   values are excluded from the output. Not used when `dimensions` is passed.
#' @param names A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used as labels for sectors.
#' @param values A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used to set values associated to sectors.
#' @param hole Number between 0 and 1: the fraction of the radius cut out of
#'   the pie to make a donut chart.
#' @param parents A column name in `data_frame`, a vector of values, a
#'   one-sided formula evaluated in `data_frame` (e.g. `~ log(pop)`),
#'   [px_constant()] or [px_range()]. Values from this column or vector are
#'   used as parents in sunburst and treemap charts.
#' @param path Column names in `data_frame` (a character vector or a formula
#'   such as `~c(a, b)`), or a list of column names, formulas and vectors. The columns define the hierarchy of sectors,
#'   from root to leaves. Cannot be combined with `ids` or `parents`.
#' @param ids A column name in `data_frame`, a vector of values, a one-sided
#'   formula evaluated in `data_frame` (e.g. `~ log(pop)`), [px_constant()] or
#'   [px_range()]. Values from this column or vector are used to set ids of
#'   sectors
#' @param branchvalues "total" or "remainder" Determines how the items in
#'   `values` are summed. Whenset to "total", items in `values` are taken to be
#'   valueof all its descendants. When set to "remainder", itemsin `values`
#'   corresponding to the root and the branches:sectors are taken to be the
#'   extra part not part of thesum of the values at their leaves.
#' @param maxdepth Positive integer Sets the number of rendered sectors from
#'   any given `level`. Set `maxdepth` to -1 to render all thelevels in the
#'   hierarchy.
#' @name px_args
#' @keywords internal
NULL
