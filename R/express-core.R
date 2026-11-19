# Plotly Express core (a port of plotly.py's plotly/express/_core.py).
#
# Every px_*() function collects its arguments into `args` and calls
# make_figure() with a trace type ("constructor"), plus trace and layout
# patches. make_figure():
#   1. applies defaults (px_defaults and the template),
#   2. builds a long-form data.table with one column per mapped argument
#      (build_dataframe(), including wide-form data and hierarchies),
#   3. works out which arguments map to trace properties directly and which
#      split the data into groups (infer_config()),
#   4. builds one trace per group (and per marginal/trendline), facets them
#      into a subplot grid, and adds frames for animations.
# Data handling uses data.table; the logic follows plotly.py closely so the
# output matches.

NO_COLOR <- "px_no_color_constant"

DIRECT_ATTRABLES <- c(
  "base",
  "x",
  "y",
  "z",
  "a",
  "b",
  "c",
  "r",
  "theta",
  "size",
  "x_start",
  "x_end",
  "hover_name",
  "text",
  "names",
  "values",
  "parents",
  "wide_cross",
  "ids",
  "error_x",
  "error_x_minus",
  "error_y",
  "error_y_minus",
  "error_z",
  "error_z_minus",
  "lat",
  "lon",
  "locations",
  "animation_group"
)
ARRAY_ATTRABLES <- c(
  "dimensions",
  "custom_data",
  "hover_data",
  "path",
  "wide_variable"
)
GROUP_ATTRABLES <- c("animation_frame", "facet_row", "facet_col", "line_group")
RENAMEABLE_GROUP_ATTRABLES <- c("color", "symbol", "line_dash", "pattern_shape")
ALL_ATTRABLES <- c(
  DIRECT_ATTRABLES,
  ARRAY_ATTRABLES,
  GROUP_ATTRABLES,
  RENAMEABLE_GROUP_ATTRABLES
)
CARTESIAN_TYPES <- c(
  "scatter",
  "scattergl",
  "bar",
  "funnel",
  "box",
  "violin",
  "histogram",
  "histogram2d",
  "histogram2dcontour"
)

# Defaults ------------------------------------------------------------------------

#' Plotly Express defaults
#'
#' `px_defaults` is an environment of default values used by the `px_*()`
#' functions when the corresponding argument is `NULL`, like plotly.py's
#' `px.defaults`: `template`, `width`, `height`, `color_discrete_sequence`,
#' `color_discrete_map`, `color_continuous_scale`, `symbol_sequence`,
#' `symbol_map`, `line_dash_sequence`, `line_dash_map`,
#' `pattern_shape_sequence`, `pattern_shape_map`, `size_max`,
#' `category_orders` and `labels`. `reset_px_defaults()` restores the
#' initial values.
#'
#' @format An environment.
#' @return `reset_px_defaults()` returns `px_defaults` invisibly.
#' @export
#' @examples
#' px_defaults$template <- "plotly_white"
#' px_defaults$color_continuous_scale <- px_colors$sequential$Blackbody
#' reset_px_defaults()
px_defaults <- new.env(parent = emptyenv())

#' @rdname px_defaults
#' @export
reset_px_defaults <- function() {
  vals <- list(
    template = NULL,
    width = NULL,
    height = NULL,
    color_discrete_sequence = NULL,
    color_discrete_map = list(),
    color_continuous_scale = NULL,
    symbol_sequence = NULL,
    symbol_map = list(),
    line_dash_sequence = NULL,
    line_dash_map = list(),
    pattern_shape_sequence = NULL,
    pattern_shape_map = list(),
    size_max = 20,
    category_orders = list(),
    labels = list()
  )
  rm(list = ls(px_defaults), envir = px_defaults)
  for (n in names(vals)) {
    assign(n, vals[n][[1L]], envir = px_defaults)
  }
  invisible(px_defaults)
}
PX_DEFAULT_NAMES <- c(
  "template",
  "width",
  "height",
  "color_discrete_sequence",
  "color_discrete_map",
  "color_continuous_scale",
  "symbol_sequence",
  "symbol_map",
  "line_dash_sequence",
  "line_dash_map",
  "pattern_shape_sequence",
  "pattern_shape_map",
  "size_max",
  "category_orders",
  "labels"
)

# Special inputs -----------------------------------------------------------------------

#' Constant and range inputs for Plotly Express
#'
#' Pass `px_constant()` to an argument that expects a column, to use the
#' same value for every row (with an optional legend/axis `label`).
#' `px_range()` uses the integers 0, 1, 2, ... `px_identity()` can be passed
#' to `color_discrete_map`, `symbol_map`, `line_dash_map` or
#' `pattern_shape_map` to use the data values themselves (e.g. colors).
#'
#' @param value The constant value.
#' @param label Name used for the column in labels and hover text.
#' @return An object recognised by the `px_*()` functions.
#' @export
#' @examples
#' px_data_iris() |>
#'   px_scatter(x = ~sepal_width, y = ~sepal_length, color = px_constant("all flowers"))
#' px_data_iris() |> px_scatter(x = px_range("index"), y = ~sepal_length)
px_constant <- function(value, label = NULL) {
  structure(list(value = value, label = label), class = "px_constant")
}

#' @rdname px_constant
#' @export
px_range <- function(label = NULL) {
  structure(list(label = label), class = "px_range")
}

#' @rdname px_constant
#' @export
px_identity <- function() "identity"

# Helpers ---------------------------------------------------------------------------------

compact <- function(x) x[!vapply(x, is.null, logical(1))]

# Drop NULLs recursively, and lists emptied by doing so (like plotly.py
# ignoring dict(enabled=None)).
compact_deep <- function(x) {
  for (n in names(x)) {
    v <- x[[n]]
    if (is.list(v) && !is.data.frame(v) && !is.null(names(v)) && length(v)) {
      v <- compact_deep(v)
      x[n] <- if (length(v)) list(v) else list(NULL)
    }
  }
  compact(x)
}

get_label <- function(args, column) {
  if (is.null(column)) {
    return(NULL)
  }
  lbl <- args$labels[[column]]
  if (is.null(lbl)) column else lbl
}

invert_label <- function(args, column) {
  labels <- args$labels
  hit <- names(labels)[vapply(labels, identical, logical(1), column)]
  if (length(hit)) hit[[1L]] else column
}

is_continuous <- function(df, col) {
  v <- df[[col]]
  is.numeric(v) && !is.factor(v)
}

# Python's str() of a value, used for trace names and frame names.
py_str <- function(x) {
  if (length(x) != 1L) {
    return(as.character(x))
  }
  if (is.na(x)) {
    return(if (is.numeric(x)) "nan" else "None")
  }
  if (is.logical(x)) {
    return(if (x) "True" else "False")
  }
  if (is.double(x) && is.finite(x) && x == round(x) && abs(x) < 1e16) {
    return(sprintf("%.1f", x))
  }
  if (inherits(x, "Date")) {
    return(format(x))
  }
  if (inherits(x, "POSIXt")) {
    return(format_datetime(x))
  }
  as.character(x)
}

# Map keys for group values (NA has its own key).
val_key <- function(x) {
  if (is.na(x)) {
    return("\001NA")
  }
  key <- py_str(x)
  if (identical(key, "")) "\002" else key
}

escape_col_name <- function(columns, name, extra = character()) {
  if (is.null(columns)) {
    return(name)
  }
  while (name %in% columns || name %in% extra) {
    name <- paste0("_", name)
  }
  name
}

get_decorated_label <- function(args, column, role) {
  label <- original <- get_label(args, column)
  if (
    "histfunc" %in%
      names(args) &&
      (identical(role, "z") ||
        (identical(role, "x") && identical(args$orientation, "h")) ||
        (identical(role, "y") && identical(args$orientation, "v")))
  ) {
    histfunc <- args$histfunc %||% "count"
    label <- if (histfunc != "count") {
      sprintf("%s of %s", histfunc, label)
    } else {
      "count"
    }
    histnorm <- args$histnorm
    if (!is.null(histnorm)) {
      if (label == "count") {
        label <- histnorm
      } else if (histfunc == "sum") {
        label <- if (histnorm == "probability") {
          sprintf("fraction of %s", label)
        } else if (histnorm == "percent") {
          sprintf("%s of %s", histnorm, label)
        } else {
          sprintf("%s weighted by %s", histnorm, original)
        }
      } else if (histnorm == "probability") {
        label <- sprintf("fraction of sum of %s", label)
      } else if (histnorm == "percent") {
        label <- sprintf("percent of sum of %s", label)
      } else {
        label <- sprintf("%s of %s", histnorm, label)
      }
    }
    if (!is.null(args$barnorm)) {
      label <- sprintf("%s (normalized as %s)", label, args$barnorm)
    }
  }
  label
}

# Set a nested value in a list by path, merging with existing lists.
set_path <- function(x, path, value) {
  if (length(path) == 1L) {
    x[path] <- list(value)
    return(x)
  }
  x[[path[[1L]]]] <- set_path(x[[path[[1L]]]] %||% list(), path[-1L], value)
  x
}

# Whether trace type `type` has the property at `path` (e.g. c("marker", "symbol")).
has_property <- function(type, path) {
  !is.null(get_validator(paste(c(type, path), collapse = ".")))
}

# Defaults cascade ---------------------------------------------------------------------------

apply_default_cascade <- function(args, constructor) {
  for (p in PX_DEFAULT_NAMES) {
    if (p %in% names(args) && is.null(args[[p]])) {
      assign(p, px_defaults[[p]], envir = args)
    }
  }
  tpl <- args$template %||% default_template()
  args$template <- if (is.character(tpl)) {
    get_template(tpl)
  } else {
    check_template(tpl)
  }

  if ("color_continuous_scale" %in% names(args)) {
    ccs <- args$color_continuous_scale
    if (is.character(ccs) && length(ccs) == 1L) {
      ccs <- named_colorscale(ccs) %||% ccs
    }
    if (is.null(ccs)) {
      seq_scale <- template_layout_value(
        args$template,
        c("colorscale", "sequential")
      )
      if (length(seq_scale)) {
        ccs <- vapply(seq_scale, function(s) as.character(s[[2L]]), "")
      }
    }
    args$color_continuous_scale <- ccs %||% px_colors$sequential$Viridis
  }
  if ("color_discrete_sequence" %in% names(args)) {
    cds <- args$color_discrete_sequence
    if (is.null(cds) && !is.null(constructor)) {
      trace_type <- if (constructor == "timeline") "bar" else constructor
      defs <- args$template[["data"]][[trace_type]]
      cols <- unlist(lapply(defs, function(d) d[["marker"]][["color"]]))
      if (length(cols)) cds <- cols
    }
    if (is.null(cds)) {
      cds <- unlist(template_layout_value(args$template, "colorway"))
    }
    args$color_discrete_sequence <- cds %||% px_colors$qualitative$D3
  }
  seq_default <- function(name, from, fallback) {
    if (!name %in% names(args)) {
      return(args)
    }
    val <- args[[name]]
    if (is.null(val)) {
      val <- unlist(lapply(from, function(d) d))
    }
    if (!length(val)) {
      val <- fallback
    }
    args[[name]] <<- val
    args
  }
  scatter_defs <- args$template[["data"]][["scatter"]]
  seq_default(
    "symbol_sequence",
    lapply(scatter_defs, function(d) d[["marker"]][["symbol"]]),
    c("circle", "diamond", "square", "x", "cross")
  )
  seq_default(
    "line_dash_sequence",
    lapply(scatter_defs, function(d) d[["line"]][["dash"]]),
    c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot")
  )
  seq_default(
    "pattern_shape_sequence",
    lapply(args$template[["data"]][["bar"]], function(d) {
      d[["marker"]][["pattern"]][["shape"]]
    }),
    c("", "/", "\\", "x", "+", ".")
  )
  args
}

# Building the data frame ---------------------------------------------------------------------

as_px_data <- function(df) {
  if (is.null(df)) {
    return(NULL)
  }
  if (data.table::is.data.table(df)) {
    return(df)
  }
  if (is.data.frame(df) || is.list(df) || is.matrix(df)) {
    return(data.table::as.data.table(df))
  }
  stop(
    "`data_frame` must be a data.frame, data.table, list or matrix, not ",
    value_type(df),
    call. = FALSE
  )
}

get_reserved_col_names <- function(args) {
  out <- character()
  for (field in intersect(names(args), ALL_ATTRABLES)) {
    vals <- if (field %in% ARRAY_ATTRABLES) {
      if (is.list(args[[field]]) && !is.null(names(args[[field]]))) {
        names(args[[field]])
      } else {
        args[[field]]
      }
    } else {
      list(args[[field]])
    }
    for (v in vals) {
      if (is.character(v) && length(v) == 1L) out <- c(out, v)
    }
  }
  unique(out)
}

check_name_not_reserved <- function(field, reserved) {
  if (field %in% reserved) {
    stop(
      sprintf(
        "A name conflict was encountered for argument '%s'. A column with name '%s' is ambiguous.",
        field,
        field
      ),
      call. = FALSE
    )
  }
  field
}

# Whether `arg` is a list of column references (wide-form x or y).
is_col_list <- function(columns, arg) {
  if (is.null(arg) || inherits(arg, c("px_constant", "px_range", "formula"))) {
    return(FALSE)
  }
  if (is.character(arg)) {
    return(length(arg) > 1L && !is.null(columns) && all(arg %in% columns))
  }
  if (is.list(arg) && !is.data.frame(arg)) {
    return(all(vapply(
      arg,
      function(e) {
        if (is.character(e) && length(e) == 1L) {
          !is.null(columns) && e %in% columns
        } else {
          is.atomic(e) && length(e) > 1L
        }
      },
      logical(1)
    )))
  }
  FALSE
}

as_ref_list <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.character(x)) {
    return(as.list(x))
  }
  # ~c(a, b) refers to several columns: one formula per element
  if (inherits(x, "formula")) {
    rhs <- x[[length(x)]]
    if (is.call(rhs) && identical(rhs[[1L]], as.name("c"))) {
      return(lapply(as.list(rhs)[-1L], function(e) {
        f <- x
        f[[length(f)]] <- e
        f
      }))
    }
  }
  if (
    is.list(x) &&
      !is.data.frame(x) &&
      !inherits(x, c("px_constant", "px_range"))
  ) {
    return(x)
  }
  list(x)
}

# Normalise hover_data given as a named list into (show/format, data) pairs.
normalise_hover_data <- function(hd, df_input) {
  for (k in names(hd)) {
    v <- hd[[k]]
    pair <- if (is.list(v) && length(v) == 2L && is.null(names(v))) {
      v
    } else if ((is.logical(v) || is.character(v)) && length(v) == 1L) {
      list(v, NULL)
    } else {
      list(TRUE, v)
    }
    if (!is.null(df_input) && !is.null(pair[[2L]]) && k %in% names(df_input)) {
      stop(
        sprintf(
          "Ambiguous input: values for '%s' appear both in hover_data and data_frame",
          k
        ),
        call. = FALSE
      )
    }
    hd[[k]] <- pair
  }
  hd
}

process_args_into_dataframe <- function(args, wide_mode, var_name, value_name) {
  df_input <- args$data_frame
  df_provided <- !is.null(df_input)
  out <- list()
  constants <- list()
  ranges <- character()
  wide_id_vars <- character()
  reserved <- if (df_provided) get_reserved_col_names(args) else character()

  if ("dimensions" %in% names(args) && is.null(args$dimensions)) {
    if (!df_provided) {
      stop(
        "No data were provided. Please provide data either with `data_frame` or with `dimensions`.",
        call. = FALSE
      )
    }
    for (col in names(df_input)) {
      out[[col]] <- df_input[[col]]
    }
  }
  hover_is_dict <- is.list(args$hover_data) && !is.null(names(args$hover_data))
  if (hover_is_dict) {
    args$hover_data <- normalise_hover_data(args$hover_data, df_input)
  }

  cur_len <- function() if (length(out)) length(out[[1L]]) else 0L
  check_len <- function(n, what) {
    if (cur_len() && n != cur_len()) {
      stop(
        sprintf(
          "All arguments should have the same length. The length of %s is %d, whereas the length of previously-processed arguments %s is %d",
          what,
          n,
          paste(names(out), collapse = ", "),
          cur_len()
        ),
        call. = FALSE
      )
    }
  }

  for (field_name in ALL_ATTRABLES) {
    if (!field_name %in% names(args) || is.null(args[[field_name]])) {
      next
    }
    is_array <- field_name %in% ARRAY_ATTRABLES
    argument_list <- if (!is_array) {
      list(args[[field_name]])
    } else if (field_name == "hover_data" && hover_is_dict) {
      as.list(names(args$hover_data))
    } else {
      as_ref_list(args[[field_name]])
    }
    for (i in seq_along(argument_list)) {
      argument <- argument_list[[i]]
      if (is.null(argument)) {
        next
      }
      field <- if (is_array) paste0(field_name, "_", i - 1L) else field_name
      col_name <- NULL
      if (inherits(argument, c("px_constant", "px_range"))) {
        col_name <- check_name_not_reserved(
          if (!is.null(argument$label)) as.character(argument$label) else field,
          reserved
        )
        if (inherits(argument, "px_constant")) {
          constants[[col_name]] <- argument$value
        } else {
          ranges <- c(ranges, col_name)
        }
      } else if (inherits(argument, "formula")) {
        rhs <- argument[[length(argument)]]
        if (
          df_provided && is.name(rhs) && as.character(rhs) %in% names(df_input)
        ) {
          col_name <- as.character(rhs)
          vals <- df_input[[col_name]]
        } else {
          col_name <- paste(deparse(rhs), collapse = " ")
          vals <- eval(
            rhs,
            if (df_provided) df_input else list(),
            environment(argument)
          )
          if (
            length(vals) == 1L &&
              (cur_len() > 1L || (df_provided && nrow(df_input) > 1L))
          ) {
            vals <- rep(vals, if (cur_len()) cur_len() else nrow(df_input))
          }
        }
        check_len(length(vals), sprintf("argument `%s`", field))
        out[[col_name]] <- vals
      } else if (is.character(argument) && length(argument) == 1L) {
        if (
          field_name == "hover_data" &&
            hover_is_dict &&
            !is.null(args$hover_data[[argument]][[2L]])
        ) {
          col_name <- argument
          vals <- args$hover_data[[argument]][[2L]]
          check_len(length(vals), sprintf("hover_data key `%s`", argument))
          out[[col_name]] <- vals
        } else if (!df_provided) {
          stop(
            sprintf(
              "String arguments are only possible when a data frame is provided in the `data_frame` argument. No data frame was provided, but argument '%s' is a string.",
              field
            ),
            call. = FALSE
          )
        } else if (!argument %in% names(df_input)) {
          if (wide_mode && argument %in% c(value_name, var_name)) {
            next
          }
          stop(
            sprintf(
              "Value of '%s' is not the name of a column in 'data_frame'. Expected one of %s but received: %s",
              field,
              paste0("'", names(df_input), "'", collapse = ", "),
              argument
            ),
            call. = FALSE
          )
        } else {
          check_len(nrow(df_input), sprintf("column argument `df[%s]`", field))
          col_name <- argument
          out[[col_name]] <- df_input[[argument]]
        }
      } else {
        if (!is.atomic(argument) && !is.list(argument)) {
          stop(
            sprintf(
              "Argument '%s' must be a column name, a vector, a formula, px_constant() or px_range()",
              field
            ),
            call. = FALSE
          )
        }
        col_name <- check_name_not_reserved(field, reserved)
        check_len(length(argument), sprintf("argument `%s`", field))
        out[[col_name]] <- if (is.factor(argument)) {
          argument
        } else {
          unname(argument)
        }
      }
      if (!is_array) {
        args[[field_name]] <- col_name
      } else if (!(field_name == "hover_data" && hover_is_dict)) {
        args[[field_name]][[i]] <- col_name
      }
      if (field_name != "wide_variable") {
        wide_id_vars <- union(wide_id_vars, col_name)
      }
    }
    if (is_array && !(field_name == "hover_data" && hover_is_dict)) {
      args[[field_name]] <- unlist(args[[field_name]])
    }
  }
  n <- cur_len()
  for (col in ranges) {
    out[[col]] <- seq_len(n) - 1L
  }
  for (col in names(constants)) {
    out[[col]] <- rep(constants[[col]], n)
  }
  df <- if (length(out)) {
    data.table::as.data.table(out)
  } else {
    data.table::data.table()
  }
  if (length(out)) {
    data.table::setnames(df, names(out))
  }
  list(args = args, df = df, wide_id_vars = wide_id_vars)
}

build_dataframe <- function(args, constructor) {
  for (field in intersect(names(args), ARRAY_ATTRABLES)) {
    if (
      !is.null(args[[field]]) &&
        !(is.list(args[[field]]) && !is.null(names(args[[field]])))
    ) {
      args[[field]] <- as_ref_list(args[[field]])
    }
  }
  args$data_frame <- as_px_data(args$data_frame)
  df_provided <- !is.null(args$data_frame)
  columns <- if (df_provided) names(args$data_frame) else NULL

  no_x <- is.null(args$x)
  no_y <- is.null(args$y)
  wide_x <- !no_x && is_col_list(columns, args$x)
  wide_y <- !no_y && is_col_list(columns, args$y)
  wide_mode <- FALSE
  var_name <- NULL
  wide_cross_name <- NULL
  value_name <- NULL
  hist2d_types <- c("histogram2d", "histogram2dcontour")
  hist1d_orientation <- identical(constructor, "histogram") ||
    "ecdfmode" %in% names(args)
  wide_orientation <- NULL

  if (constructor %in% CARTESIAN_TYPES) {
    if (wide_x && wide_y) {
      stop(
        "Cannot accept list of column references or list of columns for both `x` and `y`.",
        call. = FALSE
      )
    }
    if (df_provided && no_x && no_y) {
      wide_mode <- TRUE
      args$wide_variable <- as.list(columns)
      var_name <- "variable"
      wide_orientation <- args$orientation %||%
        if (constructor == "funnel") "h" else "v"
      args$orientation <- wide_orientation
      assign("wide_cross", NULL, envir = args)
    } else if (wide_x != wide_y) {
      wide_mode <- TRUE
      args$wide_variable <- as_ref_list(if (wide_y) args$y else args$x)
      var_name <- "variable"
      wide_orientation <- if (hist1d_orientation) {
        if (wide_x) "v" else "h"
      } else {
        if (wide_y) "v" else "h"
      }
      assign(if (wide_y) "y" else "x", NULL, envir = args)
      assign("wide_cross", NULL, envir = args)
      if (!no_x && !no_y) wide_cross_name <- if (wide_y) "__x__" else "__y__"
    }
  }
  if (wide_mode) {
    value_name <- escape_col_name(columns, "value")
    var_name <- escape_col_name(columns, var_name)
  }

  missing_bar_dim <- NULL
  if (
    constructor %in%
      c("scatter", "bar", "funnel", hist2d_types) &&
      !hist1d_orientation
  ) {
    if (!wide_mode && no_x != no_y) {
      for (ax in c("x", "y")) {
        if (is.null(args[[ax]])) {
          # plotly.py uses the pandas index ("index") when a data frame is given
          args[[ax]] <- px_range(
            if (df_provided) {
              escape_col_name(columns, "index", c(var_name, value_name))
            } else {
              escape_col_name(columns, ax, c(var_name, value_name))
            }
          )
          if (constructor == "bar") {
            missing_bar_dim <- ax
          } else if (is.null(args$orientation)) {
            args$orientation <- if (ax == "x") "v" else "h"
          }
        }
      }
    }
    if (wide_mode && is.null(wide_cross_name)) {
      if (no_x != no_y && is.null(args$orientation)) {
        args$orientation <- if (no_x) "v" else "h"
      }
      args$wide_cross <- px_range(escape_col_name(
        columns,
        "index",
        c(var_name, value_name)
      ))
    }
  }

  no_color <- identical(args$color, NO_COLOR)
  if (no_color) {
    assign("color", NULL, envir = args)
  }

  res <- process_args_into_dataframe(args, wide_mode, var_name, value_name)
  args <- res$args
  df <- res$df
  wide_id_vars <- res$wide_id_vars
  # a SharedData's keys, carried with the rows (and repeated for each
  # variable of wide-form data)
  if (!is.null(args$crosstalk_key) && nrow(df) == length(args$crosstalk_key)) {
    df[[PX_KEY_COL]] <- args$crosstalk_key
    wide_id_vars <- c(wide_id_vars, PX_KEY_COL)
  }
  count_name <- escape_col_name(names(df), "count", c(var_name, value_name))

  if (!wide_mode && !is.null(missing_bar_dim) && constructor == "bar") {
    other_dim <- if (missing_bar_dim == "y") "x" else "y"
    if (!is_continuous(df, args[[other_dim]])) {
      args[[missing_bar_dim]] <- count_name
      df[[count_name]] <- rep(1L, nrow(df))
    } else if (is.null(args$orientation)) {
      args$orientation <- if (missing_bar_dim == "x") "v" else "h"
    }
  }
  if (constructor %in% hist2d_types && exists("orientation", envir = args)) {
    rm("orientation", envir = args)
  }

  if (wide_mode) {
    wide_value_vars <- setdiff(unlist(args$wide_variable), wide_id_vars)
    rm("wide_variable", envir = args)
    wide_cross_name <- if (identical(wide_cross_name, "__x__")) {
      args$x
    } else if (identical(wide_cross_name, "__y__")) {
      args$y
    } else {
      args$wide_cross
    }
    rm("wide_cross", envir = args)
    types <- unique(vapply(
      wide_value_vars,
      function(v) if (is.numeric(df[[v]])) "number" else class(df[[v]])[[1L]],
      ""
    ))
    if (length(types) > 1L) {
      stop(
        "Plotly Express cannot process wide-form data with columns of different type.",
        call. = FALSE
      )
    }
    df <- data.table::melt(
      df,
      id.vars = wide_id_vars,
      measure.vars = wide_value_vars,
      variable.name = var_name,
      value.name = value_name,
      variable.factor = FALSE
    )
    df[[var_name]] <- as.character(df[[var_name]])
    orient_v <- identical(wide_orientation, "v")
    xy <- function(v) if (v) "x" else "y"
    if (hist1d_orientation) {
      args[[xy(orient_v)]] <- value_name
      assign(xy(!orient_v), wide_cross_name, envir = args)
      args$color <- args$color %||% var_name
    } else if (constructor %in% c("scatter", "funnel", hist2d_types)) {
      assign(xy(orient_v), wide_cross_name, envir = args)
      args[[xy(!orient_v)]] <- value_name
      if (constructor != "histogram2d") {
        args$color <- args$color %||% var_name
      }
      if ("line_group" %in% names(args)) {
        args$line_group <- args$line_group %||% var_name
      }
    } else if (constructor == "bar") {
      if (is_continuous(df, value_name)) {
        assign(xy(orient_v), wide_cross_name, envir = args)
        args[[xy(!orient_v)]] <- value_name
      } else {
        args[[xy(orient_v)]] <- value_name
        args[[xy(!orient_v)]] <- count_name
        df[[count_name]] <- rep(1L, nrow(df))
      }
      args$color <- args$color %||% var_name
    } else if (constructor %in% c("violin", "box")) {
      args[[xy(orient_v)]] <- wide_cross_name %||% var_name
      args[[xy(!orient_v)]] <- value_name
    }
  }

  if (hist1d_orientation && constructor == "scatter") {
    if (!is.null(args$x) && !is.null(args$y)) {
      args$histfunc <- "sum"
    } else if (is.null(args$x)) {
      assign("histfunc", NULL, envir = args)
      args$orientation <- "h"
      args$x <- count_name
      df[[count_name]] <- rep(1L, nrow(df))
    } else {
      assign("histfunc", NULL, envir = args)
      args$orientation <- "v"
      args$y <- count_name
      df[[count_name]] <- rep(1L, nrow(df))
    }
  }
  if (no_color) {
    assign("color", NULL, envir = args)
  }
  args$data_frame <- df
  args
}

# Hierarchies (sunburst, treemap, icicle with `path`) -----------------------------------

check_dataframe_all_leaves <- function(df) {
  cols <- names(df)
  ord <- do.call(order, c(unname(as.list(df)), list(na.last = TRUE)))
  df <- df[ord]
  null_mask <- lapply(df, is.na)
  any_null <- Reduce(`|`, null_mask)
  if (any(any_null)) {
    nm <- lapply(null_mask, `[`, any_null)
    for (j in seq_along(cols)[-1L]) {
      bad <- !nm[[j]] & nm[[j - 1L]]
      if (any(bad)) {
        stop("None entries cannot have not-None children", call. = FALSE)
      }
    }
  }
  # only rows with missing values can be non-leaves (plotly.py checks every
  # pair of consecutive rows; rows without missing values never fail)
  cand <- which(any_null)
  cand <- cand[cand > 1L]
  if (!length(cand)) {
    return(invisible())
  }
  strs <- do.call(
    paste0,
    lapply(df, function(v) {
      s <- as.character(v)
      s[is.na(v)] <- ""
      s
    })
  )
  for (i in cand) {
    if (grepl(strs[[i]], strs[[i - 1L]], fixed = TRUE)) {
      stop(
        "Non-leaves rows are not permitted in the dataframe: row ",
        i,
        " is not a leaf.",
        call. = FALSE
      )
    }
  }
}

process_dataframe_hierarchy <- function(args) {
  df <- data.table::copy(args$data_frame)
  path <- rev(unlist(args$path))
  check_dataframe_all_leaves(df[, rev(path), with = FALSE])
  color <- args$color
  discrete_color <- !is.null(color) && !is_continuous(df, color)
  row_index <- escape_col_name(names(df), "__px_row_index__")
  df[[row_index]] <- seq_len(nrow(df)) - 1L
  new_path <- paste0(path, "_path_copy")
  for (k in seq_along(path)) {
    df[[new_path[[k]]]] <- df[[path[[k]]]]
  }
  path <- new_path

  if (!is.null(args$values)) {
    v <- suppressWarnings(as.numeric(df[[args$values]]))
    if (anyNA(v[!is.na(df[[args$values]])])) {
      stop(
        sprintf(
          "Column `%s` of `df` could not be converted to a numerical data type.",
          args$values
        ),
        call. = FALSE
      )
    }
    df[[args$values]] <- v
    if (!is.null(color) && color == args$values) {
      new_name <- paste0(args$values, "_sum")
      df[[new_name]] <- df[[args$values]]
      args$values <- new_name
    }
    count_col <- args$values
  } else {
    count_col <- if (!"count" %in% names(df)) {
      "count"
    } else {
      paste(names(df), collapse = "")
    }
    df[[count_col]] <- rep(1L, nrow(df))
    args$values <- count_col
  }

  agg_cols <- c(count_col, row_index)
  discrete_aggs <- character()
  continuous_aggs <- character()
  if (!is.null(color)) {
    if (discrete_color) {
      discrete_aggs <- color
    } else {
      continuous_aggs <- color
    }
    agg_cols <- c(agg_cols, color)
  }
  cols <- setdiff(names(df), c(path, row_index))
  for (col in setdiff(cols, agg_cols)) {
    df[[col]] <- as.character(df[[col]])
    discrete_aggs <- c(discrete_aggs, col)
  }
  cols <- setdiff(cols, c("labels", "parent", "id"))
  if (!is.null(color) && !discrete_color) {
    df[[color]] <- df[[color]] * df[[count_col]]
  }

  first_or_unknown <- function(v) {
    if (length(unique(v)) == 1L) v[[1L]] else "(?)"
  }
  max_or_na <- function(v) {
    v <- v[!is.na(v)]
    if (length(v)) max(v) else NA
  }
  trees <- list()
  for (i in seq_along(path)) {
    keys <- path[i:length(path)]
    keep <- stats::complete.cases(df[, keys, with = FALSE])
    sub <- df[keep]
    g <- sub[,
      c(
        stats_named(list(sum(.SD[[1L]])), count_col),
        stats_named(list(min(.SD[[2L]])), row_index),
        lapply(
          stats_named(as.list(continuous_aggs), continuous_aggs),
          function(cc) sum(.SD[[cc]])
        ),
        lapply(
          stats_named(as.list(discrete_aggs), discrete_aggs),
          function(dc) {
            v <- .SD[[dc]]
            if (length(unique(v)) == 1L) max_or_na(v) %||% v[[1L]] else "(?)"
          }
        )
      ),
      by = keys,
      .SDcols = unique(c(count_col, row_index, continuous_aggs, discrete_aggs))
    ]
    data.table::setorderv(g, row_index)
    for (cc in continuous_aggs) {
      g[[cc]] <- g[[cc]] / g[[count_col]]
    }
    level <- path[[i]]
    labels <- as.character(g[[level]])
    if (i < length(path)) {
      anc <- do.call(
        paste,
        c(
          lapply(rev(path[(i + 1L):length(path)]), function(p) {
            as.character(g[[p]])
          }),
          sep = "/"
        )
      )
      parent <- sub("^/?", "", sub("/?$", "", paste0(anc, "/")))
      id <- paste(anc, labels, sep = "/")
    } else {
      parent <- rep("", nrow(g))
      id <- labels
    }
    tree <- data.table::data.table(labels = labels, parent = parent, id = id)
    for (col in cols) {
      tree[[col]] <- g[[col]]
    }
    trees[[i]] <- tree
  }
  all_trees <- data.table::rbindlist(trees, use.names = TRUE, fill = TRUE)
  if (!is.null(color) && discrete_color) {
    ord <- order(
      as.character(all_trees[[color]]),
      na.last = TRUE,
      method = "radix"
    )
    all_trees <- all_trees[ord]
  }
  args$data_frame <- all_trees
  assign("path", NULL, envir = args)
  args$ids <- "id"
  args$names <- "labels"
  args$parents <- "parent"
  if (!is.null(color)) {
    if (is.null(args$hover_data)) {
      args$hover_data <- color
    } else if (is.list(args$hover_data) && !is.null(names(args$hover_data))) {
      if (!isTRUE(args$hover_data[[color]][[1L]])) {
        args$hover_data[[color]] <- list(TRUE, NULL)
      }
    } else {
      args$hover_data <- c(args$hover_data, color)
    }
  }
  args
}

as_datetime <- function(v) {
  if (inherits(v, "POSIXct")) {
    return(v)
  }
  if (inherits(v, "Date")) {
    return(as.POSIXct(format(v), tz = "UTC"))
  }
  out <- tryCatch(as.POSIXct(v, tz = "UTC"), error = function(e) NULL)
  if (is.null(out) || anyNA(out[!is.na(v)])) {
    stop(
      "Both x_start and x_end must refer to data convertible to datetimes.",
      call. = FALSE
    )
  }
  out
}

process_dataframe_timeline <- function(args) {
  args$is_timeline <- TRUE
  if (is.null(args$x_start) || is.null(args$x_end)) {
    stop("Both x_start and x_end are required", call. = FALSE)
  }
  df <- data.table::copy(args$data_frame)
  start <- as_datetime(df[[args$x_start]])
  end <- as_datetime(df[[args$x_end]])
  df[[args$x_start]] <- start
  df[[args$x_end]] <- round((as.numeric(end) - as.numeric(start)) * 1000)
  args$data_frame <- df
  args$x <- args$x_end
  args$base <- args$x_start
  rm(list = c("x_start", "x_end"), envir = args)
  args
}

process_dataframe_pie <- function(args, trace_patch) {
  names_col <- args$names
  if (is.null(names_col)) {
    return(list(args = args, trace_patch = trace_patch))
  }
  order_in <- args$category_orders[[names_col]]
  if (!length(order_in)) {
    return(list(args = args, trace_patch = trace_patch))
  }
  df <- args$data_frame
  trace_patch$sort <- FALSE
  trace_patch$direction <- "clockwise"
  uniques <- unique(df[[names_col]])
  ord <- unique(c(order_in, uniques))
  ord <- ord[ord %in% uniques]
  o <- order(match(df[[names_col]], ord), method = "radix")
  args$data_frame <- df[o]
  list(args = args, trace_patch = trace_patch)
}

# Shared body of px_sunburst(), px_treemap() and px_icicle().
px_hierarchy <- function(
  args,
  type,
  path,
  ids,
  parents,
  branchvalues,
  maxdepth,
  color_discrete_sequence
) {
  if (!is.null(path) && (!is.null(ids) || !is.null(parents))) {
    stop(
      "Either `path` should be provided, or `ids` and `parents`. These parameters are mutually exclusive and cannot be passed together.",
      call. = FALSE
    )
  }
  if (!is.null(path) && is.null(branchvalues)) {
    branchvalues <- "total"
  }
  layout_patch <- if (!is.null(color_discrete_sequence)) {
    stats_named(list(color_discrete_sequence), paste0(type, "colorway"))
  } else {
    list()
  }
  make_figure(
    args,
    type,
    trace_patch = list(branchvalues = branchvalues, maxdepth = maxdepth),
    layout_patch = layout_patch
  )
}
