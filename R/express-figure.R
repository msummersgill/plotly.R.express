# Plotly Express figure assembly (continued port of plotly/express/_core.py):
# infer_config(), grouping, trace construction, faceting, axes, animation.

# Mappings --------------------------------------------------------------------------------

# A grouping variable and how its values map onto trace properties
# (plotly.py's Mapping namedtuple).
make_mapping <- function(args, variable) {
  if (variable %in% c("line_group", "animation_frame")) {
    return(list(
      show_in_trace_name = FALSE,
      grouper = args[[variable]],
      val_map = list(),
      identity = FALSE,
      sequence = "",
      variable = variable,
      path = NULL,
      facet = NULL
    ))
  }
  if (variable %in% c("facet_row", "facet_col")) {
    return(list(
      show_in_trace_name = FALSE,
      grouper = args[[variable]],
      val_map = list(),
      identity = FALSE,
      sequence = seq_len(999L),
      variable = if (variable == "facet_col") "x" else "y",
      path = NULL,
      facet = if (variable == "facet_row") "row" else "col"
    ))
  }
  parts <- strsplit(variable, ".", fixed = TRUE)[[1L]]
  var <- parts[[2L]]
  vprefix <- arg_name <- var
  if (var == "color") {
    vprefix <- "color_discrete"
  }
  if (var == "dash") {
    arg_name <- vprefix <- "line_dash"
  }
  if (var %in% c("pattern", "shape")) {
    arg_name <- vprefix <- "pattern_shape"
  }
  map <- args[[paste0(vprefix, "_map")]]
  identity <- identical(map, "identity")
  val_map <- if (identity || is.null(map)) list() else as.list(map)
  list(
    show_in_trace_name = TRUE,
    grouper = args[[arg_name]],
    val_map = val_map,
    identity = identity,
    sequence = args[[paste0(vprefix, "_sequence")]],
    variable = var,
    path = parts,
    facet = NULL
  )
}

mapped_value <- function(m, val) {
  if (m$identity) {
    return(val)
  }
  m$val_map[[val_key(val)]]
}

# Trace specs -------------------------------------------------------------------------------

trace_spec <- function(
  constructor,
  attrs,
  trace_patch = list(),
  marginal = NULL
) {
  list(
    constructor = constructor,
    attrs = attrs,
    trace_patch = trace_patch,
    marginal = marginal
  )
}

make_trace_spec <- function(args, constructor, attrs, trace_patch) {
  if (
    constructor %in%
      c("scatter", "scatterpolar") &&
      "render_mode" %in% names(args) &&
      (identical(args$render_mode, "webgl") ||
        (identical(args$render_mode, "auto") &&
          nrow(args$data_frame) > 1000 &&
          !identical(args$line_shape, "spline") &&
          is.null(args$animation_frame)))
  ) {
    if (constructor == "scatter") {
      constructor <- "scattergl"
      trace_patch$orientation <- NULL
    } else {
      constructor <- "scatterpolargl"
    }
  }
  result <- list(trace_spec(constructor, attrs, trace_patch))
  for (letter in c("x", "y")) {
    mkey <- paste0("marginal_", letter)
    marginal <- args[[mkey]]
    if (is.null(marginal)) {
      next
    }
    axis_map <- list(
      xaxis = if (letter == "x") "x1" else "x2",
      yaxis = if (letter == "y") "y1" else "y2"
    )
    spec <- switch(
      marginal,
      histogram = {
        mattrs <- c(letter, mkey)
        patch <- c(list(opacity = 0.5, bingroup = letter), axis_map)
        if (!is.null(args$z)) {
          mattrs <- c(mattrs, "z")
          patch$histfunc <- args$histfunc
          patch$orientation <- if (letter == "x") "v" else "h"
        }
        trace_spec("histogram", mattrs, patch, letter)
      },
      violin = trace_spec(
        "violin",
        c(letter, "hover_name", "hover_data"),
        list(scalegroup = letter),
        letter
      ),
      box = trace_spec(
        "box",
        c(letter, "hover_name", "hover_data"),
        list(notched = TRUE),
        letter
      ),
      rug = trace_spec(
        "box",
        c(letter, "hover_name", "hover_data"),
        list(
          fillcolor = "rgba(255,255,255,0)",
          line = list(color = "rgba(255,255,255,0)"),
          boxpoints = "all",
          jitter = 0,
          hoveron = "points",
          marker = list(
            symbol = if (letter == "x") "line-ns-open" else "line-ew-open"
          )
        ),
        letter
      ),
      heatmap = {
        if (constructor != "histogram2d") {
          stop(
            "`marginal_x`/`marginal_y` value `'heatmap'` is only supported for `density_heatmap`.",
            call. = FALSE
          )
        }
        other <- if (letter == "x") "y" else "x"
        patch <- c(
          list(coloraxis = "coloraxis1", histfunc = args$histfunc),
          axis_map
        )
        ov <- args$data_frame[[args[[other]]]]
        omin <- min(ov, na.rm = TRUE)
        omax <- max(ov, na.rm = TRUE)
        span <- omax - omin
        if (span == 0) {
          span <- 1
        }
        pad <- span * 0.001
        bins <- list(
          start = omin - pad,
          end = omax + pad,
          size = span + 2 * pad
        )
        if (letter == "x") {
          patch$xbingroup <- "x"
          patch$ybins <- bins
        } else {
          patch$ybingroup <- "y"
          patch$xbins <- bins
        }
        if (!isFALSE(args$text_auto) && !is.null(args$text_auto)) {
          patch$texttemplate <- if (isTRUE(args$text_auto)) {
            "%{z}"
          } else {
            paste0("%{z:", args$text_auto, "}")
          }
        }
        trace_spec("histogram2d", c(letter, other, "z"), compact(patch), letter)
      },
      stop(
        sprintf(
          "Invalid value '%s' for `%s`. Supported marginal plot types are: 'rug', 'box', 'violin', 'histogram', 'heatmap'.",
          marginal,
          mkey
        ),
        call. = FALSE
      )
    )
    if (
      spec$constructor != "histogram2d" &&
        ("color" %in% attrs || !"color" %in% names(args))
    ) {
      spec$trace_patch$marker <- c(
        spec$trace_patch$marker,
        list(color = args$color_continuous_scale[[1L]])
      )
    }
    result <- c(result, list(spec))
  }
  if (
    !is.null(args$trendline) &&
      identical(args$trendline_scope %||% "trace", "trace")
  ) {
    result <- c(result, list(make_trendline_spec(args, constructor)))
  }
  result
}

make_trendline_spec <- function(args, constructor) {
  spec <- trace_spec(
    if (constructor == "scattergl") "scattergl" else "scatter",
    "trendline",
    list(mode = "lines")
  )
  if (!is.null(args$trendline_color_override)) {
    spec$trace_patch$line <- list(color = args$trendline_color_override)
  }
  spec
}

# Configuration -------------------------------------------------------------------------------

infer_config <- function(args, constructor, trace_patch, layout_patch) {
  attrs <- intersect(c(DIRECT_ATTRABLES, ARRAY_ATTRABLES), names(args))
  grouped_attrs <- character()
  df <- args$data_frame
  sizeref <- 0
  if (!is.null(args$size)) {
    sizeref <- max(df[[args$size]], na.rm = TRUE) / args$size_max^2
  }

  show_colorbar <- FALSE
  if ("color" %in% names(args)) {
    if ("color_continuous_scale" %in% names(args)) {
      if (!"color_discrete_sequence" %in% names(args)) {
        attrs <- c(attrs, "color")
      } else if (!is.null(args$color) && is_continuous(df, args$color)) {
        attrs <- c(attrs, "color")
        args$color_is_continuous <- TRUE
      } else if (constructor %in% c("sunburst", "treemap", "icicle")) {
        attrs <- c(attrs, "color")
        args$color_is_continuous <- FALSE
      } else {
        grouped_attrs <- c(grouped_attrs, "marker.color")
      }
    } else if (
      "line_group" %in% names(args) || constructor == "histogram2dcontour"
    ) {
      grouped_attrs <- c(grouped_attrs, "line.color")
    } else if (constructor %in% c("pie", "funnelarea")) {
      attrs <- c(attrs, "color")
      if (!is.null(args$color)) {
        args$hover_data <- c(args$hover_data, args$color)
      }
    } else {
      grouped_attrs <- c(grouped_attrs, "marker.color")
    }
    show_colorbar <- "color" %in%
      attrs &&
      !is.null(args$color) &&
      !constructor %in% c("pie", "funnelarea") &&
      (!constructor %in% c("treemap", "sunburst", "icicle") ||
        isTRUE(args$color_is_continuous))
  }
  if ("line_dash" %in% names(args)) {
    grouped_attrs <- c(grouped_attrs, "line.dash")
  }
  if ("symbol" %in% names(args)) {
    grouped_attrs <- c(grouped_attrs, "marker.symbol")
  }
  if ("pattern_shape" %in% names(args)) {
    grouped_attrs <- c(
      grouped_attrs,
      if (constructor == "scatter") {
        "fillpattern.shape"
      } else {
        "marker.pattern.shape"
      }
    )
  }

  if ("orientation" %in% names(args)) {
    has_x <- !is.null(args$x)
    has_y <- !is.null(args$y)
    if (is.null(args$orientation)) {
      if (constructor %in% c("histogram", "scatter")) {
        if (has_y && !has_x) args$orientation <- "h"
      } else if (constructor %in% c("violin", "box", "bar", "funnel")) {
        if (has_x && !has_y) args$orientation <- "h"
      }
    }
    if (is.null(args$orientation) && has_x && has_y) {
      xc <- is_continuous(df, args$x)
      yc <- is_continuous(df, args$y)
      if (xc && !yc) {
        args$orientation <- "h"
      }
      if (yc && !xc) args$orientation <- "v"
    }
    if (is.null(args$orientation)) {
      args$orientation <- "v"
    }
    if (constructor == "histogram") {
      if (has_x && has_y && is.null(args$histfunc)) {
        args$histfunc <- trace_patch$histfunc <- "sum"
      }
      v <- args$orientation == "v"
      trace_patch["nbinsx"] <- list(if (v) args$nbins)
      trace_patch["nbinsy"] <- list(if (!v) args$nbins)
      trace_patch$bingroup <- if (v) "x" else "y"
    }
    trace_patch$orientation <- args$orientation
    if (constructor %in% c("violin", "box")) {
      mode <- if (constructor == "box") "boxmode" else "violinmode"
      if (is.null(layout_patch[[mode]]) && !is.null(args$color)) {
        if (identical(args$y, args$color) && args$orientation == "h") {
          layout_patch[[mode]] <- "overlay"
        } else if (identical(args$x, args$color) && args$orientation == "v") {
          layout_patch[[mode]] <- "overlay"
        }
      }
      if (is.null(layout_patch[[mode]])) layout_patch[[mode]] <- "group"
    }
  }
  if (
    constructor == "histogram2d" && !is.null(args$z) && is.null(args$histfunc)
  ) {
    args$histfunc <- trace_patch$histfunc <- "sum"
  }
  if (!is.null(args$text_auto) && !isFALSE(args$text_auto)) {
    letter <- if (constructor %in% c("histogram2d", "histogram2dcontour")) {
      "z"
    } else if (constructor == "bar") {
      if (args$orientation == "v") "y" else "x"
    } else {
      "value"
    }
    trace_patch$texttemplate <- if (isTRUE(args$text_auto)) {
      paste0("%{", letter, "}")
    } else {
      paste0("%{", letter, ":", args$text_auto, "}")
    }
  }
  if (constructor %in% c("histogram2d", "densitymap")) {
    show_colorbar <- TRUE
    trace_patch$coloraxis <- "coloraxis1"
  }
  if ("opacity" %in% names(args)) {
    if (is.null(args$opacity)) {
      if (identical(args$barmode, "overlay")) {
        trace_patch$marker <- list(opacity = 0.5)
      }
    } else if (
      constructor %in% c("densitymap", "pie", "funnel", "funnelarea")
    ) {
      trace_patch$opacity <- args$opacity
    } else {
      trace_patch$marker <- list(opacity = args$opacity)
    }
  }
  if ("line_group" %in% names(args) || "line_dash" %in% names(args)) {
    modes <- character()
    if (!isFALSE(args$lines)) {
      modes <- c(modes, "lines")
    }
    if (!is.null(args$text) || !is.null(args$symbol) || isTRUE(args$markers)) {
      modes <- c(modes, "markers")
    }
    if (!is.null(args$text)) {
      modes <- c(modes, "text")
    }
    if (!length(modes)) {
      modes <- "lines"
    }
    trace_patch$mode <- paste(sort(unique(modes)), collapse = "+")
  } else if (
    constructor != "splom" &&
      ("symbol" %in% names(args) || constructor == "scattermap")
  ) {
    trace_patch$mode <- paste0(
      "markers",
      if (!is.null(args$text)) "+text" else ""
    )
  }
  if ("line_shape" %in% names(args)) {
    trace_patch$line <- list(shape = args$line_shape)
  } else if ("ecdfmode" %in% names(args)) {
    trace_patch$line <- list(
      shape = if (identical(args$ecdfmode, "reversed")) "vh" else "hv"
    )
  }
  if ("geojson" %in% names(args)) {
    trace_patch$featureidkey <- args$featureidkey
    trace_patch$geojson <- args$geojson
  }
  if ("marginal" %in% names(args)) {
    pos <- if (args$orientation == "v") "marginal_x" else "marginal_y"
    other <- if (args$orientation == "h") "marginal_x" else "marginal_y"
    assign(pos, args$marginal, envir = args)
    assign(other, NULL, envir = args)
  }
  if (nrow(df) == 0L) {
    args$facet_row <- NULL
    args$facet_col <- NULL
  }
  if (!is.null(args$facet_col) && !is.null(args$marginal_y)) {
    assign("marginal_y", NULL, envir = args)
  }
  if (!is.null(args$facet_row) && !is.null(args$marginal_x)) {
    assign("marginal_x", NULL, envir = args)
  }
  if (
    !is.null(args$marginal_x) ||
      !is.null(args$marginal_y) ||
      !is.null(args$facet_row)
  ) {
    args$facet_col_wrap <- 0
  }
  if (
    !is.null(args$trendline) && !args$trendline %in% names(TRENDLINE_FUNCTIONS)
  ) {
    stop(
      sprintf(
        "Value '%s' for `trendline` must be one of %s",
        args$trendline,
        paste0("'", names(TRENDLINE_FUNCTIONS), "'", collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if ("trendline_options" %in% names(args) && is.null(args$trendline_options)) {
    args$trendline_options <- list()
  }
  if ("ecdfnorm" %in% names(args)) {
    if (
      !is.null(args$ecdfnorm) && !args$ecdfnorm %in% c("percent", "probability")
    ) {
      stop(
        sprintf(
          "`ecdfnorm` must be one of NULL, 'percent' or 'probability'. '%s' was provided.",
          args$ecdfnorm
        ),
        call. = FALSE
      )
    }
    assign("histnorm", args$ecdfnorm, envir = args)
  }
  grouped_attrs <- c(grouped_attrs, intersect(GROUP_ATTRABLES, names(args)))
  grouped_mappings <- lapply(grouped_attrs, make_mapping, args = args)
  list(
    args = args,
    trace_specs = make_trace_spec(
      args,
      constructor,
      attrs,
      compact_deep(trace_patch)
    ),
    grouped_mappings = grouped_mappings,
    sizeref = sizeref,
    show_colorbar = show_colorbar,
    trace_patch = trace_patch,
    layout_patch = layout_patch
  )
}

# Grouping ------------------------------------------------------------------------------------

# Values of a column in display order: factor levels for factors, else first
# appearance (plotly.py uses unique(maintain_order=True)).
column_uniques <- function(v) {
  if (is.factor(v)) {
    present <- levels(v)[levels(v) %in% as.character(v)]
    if (anyNA(v)) {
      present <- c(present, NA)
    }
    return(present)
  }
  unique(v)
}

get_groups_and_orders <- function(args, grouper) {
  orders <- args$category_orders %||% list()
  orders <- lapply(orders, function(o) o)
  df <- args$data_frame
  single <- list()
  for (col in grouper) {
    if (is.null(col)) {
      single <- c(single, list(""))
      next
    }
    u <- column_uniques(df[[col]])
    if (is.factor(df[[col]])) {
      u <- as.character(u)
    }
    if (length(u) == 1L) {
      single <- c(single, list(u[[1L]]))
    }
    orders[[col]] <- if (is.null(orders[[col]])) {
      u
    } else {
      unique(c(orders[[col]], u))
    }
  }
  if (length(single) == length(grouper)) {
    return(list(
      groups = list(list(key = single, rows = seq_len(nrow(df)))),
      orders = orders
    ))
  }
  required <- unique(unlist(grouper))
  required <- names(orders)[names(orders) %in% required]
  keys <- lapply(required, function(col) {
    v <- df[[col]]
    if (is.factor(v)) as.character(v) else v
  })
  names(keys) <- required
  keep <- Reduce(`&`, lapply(keys, function(k) !is.na(k)))
  idx <- which(keep)
  if (!length(idx)) {
    return(list(groups = list(), orders = orders))
  }
  pos <- lapply(required, function(col) match(keys[[col]][idx], orders[[col]]))
  gid <- do.call(paste, c(pos, sep = "\r"))
  first <- !duplicated(gid)
  ord <- do.call(order, unname(lapply(pos, function(p) p[first])))
  gids <- gid[first][ord]
  rows_by <- split(idx, factor(gid, levels = gids))
  groups <- lapply(seq_along(gids), function(g) {
    r <- rows_by[[g]]
    key <- lapply(grouper, function(col) {
      if (is.null(col)) "" else keys[[col]][[r[[1L]]]]
    })
    list(key = key, rows = r)
  })
  list(groups = groups, orders = orders)
}

# Trace construction ----------------------------------------------------------------------------

make_trace_kwargs <- function(args, spec, trace_data, mapping_labels, sizeref) {
  constructor <- spec$constructor
  if (isTRUE(args$line_close)) {
    trace_data <- rbind(trace_data, trace_data[1L])
  }
  patch <- spec$trace_patch
  fit_results <- NULL
  hover_header <- ""
  hist_types <- c("histogram", "histogram2d", "histogram2dcontour")
  domain_types <- c("sunburst", "treemap", "icicle", "pie", "funnelarea")
  # Columns are arrays even when a group has one row (I() keeps them boxed).
  col <- function(name) {
    v <- trace_data[[name]]
    if (length(v) == 1L) I(v) else v
  }

  for (attr_name in spec$attrs) {
    attr_value <- args[[attr_name]]
    attr_label <- get_decorated_label(
      args,
      if (is.character(attr_value) && length(attr_value) == 1L) attr_value,
      attr_name
    )
    if (attr_name == "dimensions") {
      dims <- Filter(
        function(name) {
          (is.null(attr_value) || name %in% attr_value) &&
            (constructor != "parcoords" ||
              is_continuous(args$data_frame, name)) &&
            (constructor != "parcats" ||
              (!is.null(attr_value) && name %in% attr_value) ||
              length(unique(args$data_frame[[name]])) <=
                args$dimensions_max_cardinality)
        },
        names(trace_data)
      )
      patch$dimensions <- lapply(dims, function(name) {
        list(label = get_label(args, name), values = col(name))
      })
      if (constructor == "splom") {
        patch$dimensions <- lapply(patch$dimensions, function(d) {
          c(d, list(axis = list(matches = TRUE)))
        })
        mapping_labels[["%{xaxis.title.text}"]] <- "%{x}"
        mapping_labels[["%{yaxis.title.text}"]] <- "%{y}"
      }
    } else if (!is.null(attr_value)) {
      if (attr_name == "size") {
        patch$marker$size <- col(attr_value)
        patch$marker$sizemode <- "area"
        patch$marker$sizeref <- sizeref
        mapping_labels[[attr_label]] <- "%{marker.size}"
      } else if (attr_name == "marginal_x") {
        if (constructor == "histogram" && is.null(args$z)) {
          mapping_labels[["count"]] <- "%{y}"
        }
      } else if (attr_name == "marginal_y") {
        if (constructor == "histogram" && is.null(args$z)) {
          mapping_labels[["count"]] <- "%{x}"
        }
      } else if (attr_name == "trendline") {
        tl <- compute_trendline(args, trace_data)
        if (!is.null(tl)) {
          patch$x <- tl$x
          patch$y <- tl$y
          hover_header <- tl$hover_header
          fit_results <- tl$fit_results
          mapping_labels[[get_label(args, args$x)]] <- "%{x}"
          mapping_labels[[get_label(args, args$y)]] <- "%{y} <b>(trend)</b>"
        }
      } else if (startsWith(attr_name, "error")) {
        error_xy <- substr(attr_name, 1L, 7L)
        arr <- if (endsWith(attr_name, "minus")) "arrayminus" else "array"
        patch[[error_xy]][[arr]] <- col(attr_value)
      } else if (attr_name == "custom_data") {
        cd_cols <- unique(attr_value)
        if (length(cd_cols)) {
          patch$customdata <- trace_data[, cd_cols, with = FALSE]
        }
      } else if (attr_name == "hover_name") {
        if (!constructor %in% hist_types) {
          patch$hovertext <- col(attr_value)
          if (hover_header == "") hover_header <- "<b>%{hovertext}</b><br><br>"
        }
      } else if (attr_name == "hover_data") {
        if (!constructor %in% hist_types) {
          is_dict <- is.list(attr_value) && !is.null(names(attr_value))
          hd_cols <- if (is_dict) names(attr_value) else attr_value
          customdata_cols <- unlist(args$custom_data) %||% character()
          for (c in hd_cols) {
            if (c %in% c(args$x, args$y, args$z, args$base)) {
              next
            }
            position <- match(c, args$custom_data)
            if (is.na(position)) {
              customdata_cols <- c(customdata_cols, c)
              position <- length(customdata_cols)
            }
            mapping_labels[[get_decorated_label(args, c, NULL)]] <- sprintf(
              "%%{customdata[%d]}",
              position - 1L
            )
          }
          cd_cols <- unique(customdata_cols)
          if (length(cd_cols)) {
            patch$customdata <- trace_data[, cd_cols, with = FALSE]
          }
        }
      } else if (attr_name == "color") {
        if (constructor %in% c("choropleth", "choroplethmap")) {
          patch$z <- col(attr_value)
          patch$coloraxis <- "coloraxis1"
          mapping_labels[[attr_label]] <- "%{z}"
        } else if (constructor %in% domain_types) {
          if (isTRUE(args$color_is_continuous)) {
            patch$marker$colors <- col(attr_value)
            patch$marker$coloraxis <- "coloraxis1"
            mapping_labels[[attr_label]] <- "%{color}"
          } else {
            mapping <- if (identical(args$color_discrete_map, "identity")) {
              list()
            } else {
              as.list(args$color_discrete_map %||% list())
            }
            seqc <- args$color_discrete_sequence
            colors <- character(nrow(trace_data))
            vals <- col(attr_value)
            for (k in seq_along(vals)) {
              key <- val_key(vals[[k]])
              if (is.null(mapping[[key]])) {
                mapping[[key]] <- seqc[[(length(mapping) %% length(seqc)) + 1L]]
              }
              colors[[k]] <- mapping[[key]]
            }
            patch$marker$colors <- colors
          }
        } else {
          colorable <- if (constructor %in% c("parcats", "parcoords")) {
            "line"
          } else {
            "marker"
          }
          patch[[colorable]]$color <- col(attr_value)
          patch[[colorable]]$coloraxis <- "coloraxis1"
          mapping_labels[[attr_label]] <- sprintf("%%{%s.color}", colorable)
        }
      } else if (attr_name == "animation_group") {
        patch$ids <- col(attr_value)
      } else if (attr_name == "locations") {
        patch$locations <- col(attr_value)
        mapping_labels[[attr_label]] <- "%{location}"
      } else if (attr_name == "values") {
        patch$values <- col(attr_value)
        mapping_labels[[
          if (attr_label == "values") "value" else attr_label
        ]] <- "%{value}"
      } else if (attr_name == "parents") {
        patch$parents <- col(attr_value)
        mapping_labels[[
          if (attr_label == "parents") "parent" else attr_label
        ]] <- "%{parent}"
      } else if (attr_name == "ids") {
        patch$ids <- col(attr_value)
        mapping_labels[[
          if (attr_label == "ids") "id" else attr_label
        ]] <- "%{id}"
      } else if (attr_name == "names") {
        if (constructor %in% domain_types) {
          patch$labels <- col(attr_value)
          mapping_labels[[
            if (attr_label == "names") "label" else attr_label
          ]] <- "%{label}"
        } else {
          patch$names <- col(attr_value)
        }
      } else if (attr_name == "z" && constructor == "histogram") {
        other <- if (identical(spec$marginal, "x")) "y" else "x"
        patch[[other]] <- col(attr_value)
        mapping_labels[[attr_label]] <- sprintf("%%{%s}", other)
      } else {
        patch[[attr_name]] <- col(attr_value)
        mapping_labels[[attr_label]] <- sprintf("%%{%s}", attr_name)
      }
    } else if (
      (constructor == "histogram" && attr_name %in% c("x", "y")) ||
        (constructor %in%
          c("histogram2d", "histogram2dcontour") &&
          attr_name == "z")
    ) {
      if (!is.null(attr_label)) {
        mapping_labels[[attr_label]] <- sprintf("%%{%s}", attr_name)
      }
    }
  }
  if (!constructor %in% c("parcoords", "parcats")) {
    labels_out <- mapping_labels
    hd <- args$hover_data
    if (is.list(hd) && !is.null(names(hd))) {
      for (k in names(mapping_labels)) {
        k_args <- invert_label(args, k)
        if (k_args %in% names(hd)) {
          formatter <- hd[[k_args]][[1L]]
          if (is.character(formatter)) {
            labels_out[[k]] <- gsub(
              "}",
              paste0(formatter, "}"),
              mapping_labels[[k]],
              fixed = TRUE
            )
          } else if (isFALSE(formatter)) {
            labels_out[[k]] <- NULL
          }
        }
      }
    }
    lines <- if (length(labels_out)) {
      paste0(names(labels_out), "=", unlist(labels_out))
    } else {
      character()
    }
    patch$hovertemplate <- paste0(
      hover_header,
      paste(lines, collapse = "<br>"),
      "<extra></extra>"
    )
  }
  list(patch = patch, fit_results = fit_results)
}

# Figure ----------------------------------------------------------------------------------------

init_figure <- function(
  args,
  subplot_type,
  nrows,
  ncols,
  col_labels,
  row_labels
) {
  specs <- rep(
    list(rep(list(list(type = subplot_type %||% "domain")), ncols)),
    nrows
  )
  column_widths <- rep(1, ncols)
  row_heights <- rep(1, nrows)
  wrap <- args$facet_col_wrap %||% 0
  has_color <- "color" %in% names(args) && !is.null(args$color)
  if (identical(subplot_type, "xy")) {
    if (!is.null(args$marginal_x)) {
      main <- if (identical(args$marginal_x, "histogram") || has_color) {
        0.74
      } else {
        0.84
      }
      row_heights <- c(rep(main, nrows - 1L), 1 - main)
      vertical_spacing <- 0.01
    } else {
      vertical_spacing <- args$facet_row_spacing %||% if (wrap) 0.07 else 0.03
    }
    if (!is.null(args$marginal_y)) {
      main <- if (identical(args$marginal_y, "histogram") || has_color) {
        0.74
      } else {
        0.84
      }
      column_widths <- c(rep(main, ncols - 1L), 1 - main)
      horizontal_spacing <- 0.005
    } else {
      horizontal_spacing <- args$facet_col_spacing %||% 0.02
    }
  } else {
    vertical_spacing <- args$facet_row_spacing %||% if (wrap) 0.07 else 0.03
    horizontal_spacing <- args$facet_col_spacing %||% 0.02
  }
  subplot_labels <- NULL
  if (wrap) {
    col_labels <- c(
      col_labels,
      rep(NA_character_, max(0L, nrows * ncols - length(col_labels)))
    )
    subplot_labels <- character(nrows * ncols)
    for (i in seq_len(nrows)) {
      for (j in seq_len(ncols)) {
        subplot_labels[(i - 1L) * ncols + j] <- col_labels[
          (nrows - i) * ncols + j
        ]
      }
    }
  }
  fig <- tryCatch(
    make_subplots(
      rows = nrows,
      cols = ncols,
      specs = specs,
      shared_xaxes = "all",
      shared_yaxes = "all",
      row_titles = if (wrap) NULL else rev(row_labels),
      column_titles = if (wrap) NULL else col_labels,
      subplot_titles = if (wrap) subplot_labels else character(),
      horizontal_spacing = horizontal_spacing,
      vertical_spacing = vertical_spacing,
      row_heights = row_heights,
      column_widths = column_widths,
      start_cell = "bottom-left"
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("Horizontal spacing", msg)) {
        msg <- paste0(
          msg,
          "\nUse the `facet_col_spacing` argument to adjust this spacing."
        )
      }
      if (grepl("Vertical spacing", msg)) {
        msg <- paste0(
          msg,
          "\nUse the `facet_row_spacing` argument to adjust this spacing."
        )
      }
      stop(msg, call. = FALSE)
    }
  )
  if (length(fig$layout$annotations)) {
    fig <- for_each_annotation(fig, function(a) {
      a$font <- list()
      a
    })
  }
  fig
}

make_figure <- function(
  args,
  constructor,
  trace_patch = list(),
  layout_patch = list()
) {
  # An environment, so `args$z` can't partially match e.g. `zoom` (as `$`
  # does on lists), and so NULL values are kept like Python's None.
  args <- list2env(args, envir = new.env(parent = emptyenv()))
  args <- px_shared_data(args)
  user_provided_colorscale <- !is.null(args$color_continuous_scale)
  args <- apply_default_cascade(args, constructor)
  args <- build_dataframe(args, constructor)
  if (
    constructor %in% c("treemap", "sunburst", "icicle") && !is.null(args$path)
  ) {
    args <- process_dataframe_hierarchy(args)
  }
  if (constructor == "pie") {
    res <- process_dataframe_pie(args, trace_patch)
    args <- res$args
    trace_patch <- res$trace_patch
  }
  if (constructor == "timeline") {
    constructor <- "bar"
    args <- process_dataframe_timeline(args)
  }
  if ("histogram" %in% c(args$marginal_x, args$marginal_y)) {
    layout_patch$barmode <- "overlay"
  }

  cfg <- infer_config(args, constructor, trace_patch, layout_patch)
  args <- cfg$args
  layout_patch <- cfg$layout_patch
  trace_specs <- cfg$trace_specs
  mappings <- cfg$grouped_mappings
  sizeref <- cfg$sizeref
  grouper <- lapply(mappings, `[[`, "grouper")
  if (!length(grouper)) {
    grouper <- list(NULL)
  }
  go <- get_groups_and_orders(args, grouper)
  groups <- go$groups
  orders <- go$orders

  col_labels <- row_labels <- character()
  nrows <- ncols <- 1L
  for (i in seq_along(mappings)) {
    m <- mappings[[i]]
    if (is.null(m$grouper) || is.null(orders[[m$grouper]])) {
      mappings[[i]]$val_map[[val_key("")]] <- m$sequence[[1L]]
      next
    }
    sorted_values <- orders[[m$grouper]]
    if (identical(m$facet, "col")) {
      col_labels <- paste0(
        get_label(args, args$facet_col),
        "=",
        vapply(sorted_values, py_str, "")
      )
      ncols <- length(col_labels)
    }
    if (identical(m$facet, "row")) {
      row_labels <- paste0(
        get_label(args, args$facet_row),
        "=",
        vapply(sorted_values, py_str, "")
      )
      nrows <- length(row_labels)
    }
    if (!m$identity) {
      for (val in as.list(sorted_values)) {
        key <- val_key(val)
        if (is.null(mappings[[i]]$val_map[[key]])) {
          n <- length(mappings[[i]]$val_map)
          mappings[[i]]$val_map[[key]] <- m$sequence[[
            (n %% length(m$sequence)) + 1L
          ]]
        }
      }
    }
  }

  subplot_type <- subplot_type_for_trace_type(constructor)
  trace_names_by_frame <- list()
  frames <- list()
  trendline_rows <- list()
  trace_name_labels <- list()
  wrap <- args$facet_col_wrap %||% 0
  no_legend_types <- c(
    "parcats",
    "parcoords",
    "choropleth",
    "choroplethmap",
    "densitymap",
    "histogram2d",
    "sunburst",
    "treemap",
    "icicle"
  )

  for (group in groups) {
    mapping_labels <- list()
    trace_name_labels <- list()
    frame_name <- ""
    for (i in seq_along(grouper)) {
      col <- grouper[[i]]
      if (is.null(col)) {
        next
      }
      val <- group$key[[i]]
      m <- mappings[[i]]
      key <- get_label(args, col)
      if (!m$identity) {
        mapping_labels[[key]] <- py_str(val)
        if (m$show_in_trace_name) trace_name_labels[[key]] <- py_str(val)
      }
      if (m$variable == "animation_frame") frame_name <- py_str(val)
    }
    trace_name <- paste(unlist(trace_name_labels), collapse = ", ")
    fkey <- paste0("f", frame_name)
    trace_names <- trace_names_by_frame[[fkey]] %||% character()
    # A lone variable in data.table's `[` is evaluated in the calling scope;
    # an expression like group$rows would see a column named "group".
    rows <- group$rows
    group_data <- args$data_frame[rows]

    for (s in seq_along(trace_specs)) {
      spec <- trace_specs[[s]]
      ctor <- spec$constructor
      trace <- list(type = ctor, name = trace_name)
      if (!ctor %in% no_legend_types) {
        trace$legendgroup <- trace_name
        trace$showlegend <- trace_name != "" && !trace_name %in% trace_names
      }
      barmode <- layout_patch$barmode
      if (
        ctor %in%
          c("bar", "box", "violin", "histogram") &&
          (is.null(barmode) || barmode == "group")
      ) {
        trace$alignmentgroup <- "True"
        trace$offsetgroup <- trace_name
      }
      trace_names <- union(trace_names, trace_name)
      row <- col <- 1L
      for (i in seq_along(mappings)) {
        m <- mappings[[i]]
        val <- group$key[[i]]
        mv <- mapped_value(m, val)
        if (!is.null(m$path)) {
          path <- c(m$path[[1L]], m$path[-1L])
          if (has_property(ctor, path)) {
            trace <- set_path(trace, path, mv)
          } else if (
            (s > 1L &&
              ctor %in% c("violin", "box") &&
              m$variable %in% c("symbol", "pattern", "dash")) ||
              (ctor == "histogram" && m$variable %in% c("symbol", "dash"))
          ) {
            # property not available on marginal traces
          } else if (s > 1L && ctor == "histogram" && m$variable == "color") {
            trace$marker$color <- mv
          } else if (
            ctor %in% c("choropleth", "choroplethmap") && m$variable == "color"
          ) {
            trace$z <- rep(1L, length(group$rows))
            trace$colorscale <- list(list(0, mv), list(1, mv))
            trace$showscale <- FALSE
            trace$showlegend <- TRUE
          } else {
            stop(
              sprintf(
                "Invalid property for trace type '%s': %s",
                ctor,
                paste(path, collapse = ".")
              ),
              call. = FALSE
            )
          }
        }
        # As in plotly.py, a cell index only takes effect when it is > 1.
        r <- if (identical(m$facet, "row")) {
          mv
        } else if (
          !is.null(args$marginal_x) && !identical(spec$marginal, "x")
        ) {
          2L
        } else {
          1L
        }
        if (identical(m$facet, "col")) {
          cc <- mv
          if (wrap) {
            r <- 1L + ((cc - 1L) %/% wrap)
            cc <- 1L + ((cc - 1L) %% wrap)
          }
        } else {
          cc <- if (identical(spec$marginal, "y")) 2L else 1L
        }
        if (r > 1L) {
          row <- r
        }
        if (cc > 1L) col <- cc
      }
      if (!length(mappings)) {
        if (!is.null(args$marginal_x) && !identical(spec$marginal, "x")) {
          row <- 2L
        }
        if (identical(spec$marginal, "y")) col <- 2L
      }
      if (
        trace_specs[[1L]]$constructor == "histogram2dcontour" &&
          ctor == "box" &&
          !is.null(trace$line$color)
      ) {
        trace$marker$color <- trace$line$color
      }
      tdata <- group_data
      if ("ecdfmode" %in% names(args)) {
        tdata <- ecdf_transform(args, tdata)
      }
      kw <- make_trace_kwargs(args, spec, tdata, mapping_labels, sizeref)
      trace <- deep_merge(trace, kw$patch)
      if (!is.null(kw$fit_results)) {
        trendline_rows <- c(
          trendline_rows,
          list(c(mapping_labels, list(px_fit_results = kw$fit_results)))
        )
      }
      attr(trace, "px_cell") <- c(row, col)
      attr(trace, "px_key") <- px_trace_key(args, ctor, spec, tdata)
      frames[[fkey]]$name <- frame_name
      frames[[fkey]]$data <- c(frames[[fkey]]$data, list(trace))
    }
    trace_names_by_frame[[fkey]] <- trace_names
  }
  if (length(frames) > 1L) {
    fo <- vapply(orders[[args$animation_frame]], py_str, "")
    frames <- frames[order(match(vapply(frames, `[[`, "", "name"), fo))]
  }

  if (cfg$show_colorbar) {
    colorvar <- if (constructor %in% c("histogram2d", "densitymap")) {
      "z"
    } else {
      "color"
    }
    range_color <- args$range_color %||% list(NULL, NULL)
    ca <- compact(list(
      colorscale = args$color_continuous_scale,
      cmid = args$color_continuous_midpoint,
      cmin = range_color[[1L]],
      cmax = range_color[[2L]],
      colorbar = list(
        title = list(
          text = get_decorated_label(args, args[[colorvar]], colorvar)
        )
      )
    ))
    if (user_provided_colorscale) {
      ca$autocolorscale <- FALSE
    }
    layout_patch$coloraxis <- ca
  }
  for (v in c("height", "width")) {
    if (!is.null(args[[v]])) layout_patch[[v]] <- args[[v]]
  }
  layout_patch$legend <- list(tracegroupgap = 0)
  if (length(trace_name_labels)) {
    layout_patch$legend$title <- list(
      text = paste(names(trace_name_labels), collapse = ", ")
    )
  }
  if (!is.null(args$title)) {
    layout_patch$title <- list(text = args$title)
  } else if (is.null(template_layout_value(args$template, c("margin", "t")))) {
    layout_patch$margin <- list(t = 60)
  }
  if (!is.null(args$subtitle)) {
    layout_patch$title$subtitle <- list(text = args$subtitle)
  }
  if (
    !is.null(args$size) &&
      is.null(template_layout_value(args$template, c("legend", "itemsizing")))
  ) {
    layout_patch$legend$itemsizing <- "constant"
  }
  if (wrap) {
    nrows <- as.integer(ceiling(ncols / wrap))
    ncols <- as.integer(min(ncols, wrap))
  }
  if (!is.null(args$marginal_x)) {
    nrows <- nrows + 1L
  }
  if (!is.null(args$marginal_y)) {
    ncols <- ncols + 1L
  }

  fig <- init_figure(args, subplot_type, nrows, ncols, col_labels, row_labels)
  grid_ref <- attr(fig, "grid_ref")
  frame_keys <- lapply(unname(frames), function(f) {
    lapply(f$data, attr, "px_key", exact = TRUE)
  })
  frame_list <- lapply(unname(frames), function(f) {
    f$data <- lapply(f$data, function(tr) {
      cell <- attr(tr, "px_cell")
      attr(tr, "px_cell") <- NULL
      attr(tr, "px_key") <- NULL
      tr <- validate_trace(tr)
      if (tr$type == "splom") {
        return(tr)
      }
      set_trace_grid_reference(
        tr,
        grid_ref,
        nrows - cell[[1L]] + 1L,
        cell[[2L]]
      )
    })
    f
  })
  fig <- add_traces(
    fig,
    if (length(frame_list)) frame_list[[1L]]$data else list()
  )
  fig <- update_layout(fig, compact(layout_patch))
  fig <- update_layout(fig, template = args$template, overwrite = TRUE)
  if (length(frame_list) > 1L) {
    fig$frames <- lapply(frame_list, function(f) {
      list(data = f$data, name = f$name)
    })
  }
  fig <- px_set_keys(fig, args, frame_keys)

  if (!is.null(args$trendline) && identical(args$trendline_scope, "overall")) {
    spec <- make_trendline_spec(args, constructor)
    tr <- list(
      type = spec$constructor,
      name = "Overall Trendline",
      legendgroup = "Overall Trendline",
      showlegend = FALSE
    )
    if (is.null(spec$trace_patch$line)) {
      for (m in mappings) {
        if (m$variable == "color") {
          spec$trace_patch$line <- list(
            color = m$sequence[[(length(m$val_map) %% length(m$sequence)) + 1L]]
          )
        }
      }
    }
    kw <- make_trace_kwargs(args, spec, args$data_frame, list(), sizeref)
    tr <- deep_merge(tr, kw$patch)
    main_rows <- if (!is.null(args$marginal_x)) nrows - 1L else nrows
    main_cols <- if (!is.null(args$marginal_y)) ncols - 1L else ncols
    for (r in seq_len(main_rows)) {
      for (cc in seq_len(main_cols)) {
        fig <- add_trace(
          fig,
          tr,
          row = r,
          col = cc,
          exclude_empty_subplots = TRUE
        )
      }
    }
    fig <- update_traces(fig, showlegend = TRUE, selector = length(fig$data))
    if (!is.null(kw$fit_results)) {
      trendline_rows <- c(
        trendline_rows,
        list(list(px_fit_results = kw$fit_results))
      )
    }
  }
  if (length(trendline_rows)) {
    attr(fig, "px_trendlines") <- trendline_rows
  }

  fig <- configure_axes(args, constructor, fig, orders)
  configure_animation_controls(args, constructor, fig)
}

ecdf_transform <- function(args, group) {
  base <- if (args$orientation == "v") args$x else args$y
  var <- if (args$orientation == "h") args$x else args$y
  ascending <- !identical(args$ecdfmode, "reversed")
  o <- order(
    group[[base]],
    decreasing = !ascending,
    na.last = TRUE,
    method = "radix"
  )
  group <- group[o]
  vals <- group[[var]]
  total <- sum(vals, na.rm = TRUE)
  cum <- cumsum(ifelse(is.na(vals), 0, vals))
  cum[is.na(vals)] <- NA
  group[[var]] <- cum
  if (!ascending) {
    o <- order(group[[base]], na.last = TRUE, method = "radix")
    group <- group[o]
  }
  if (identical(args$ecdfmode, "complementary")) {
    group[[var]] <- total - group[[var]]
  }
  if (identical(args$ecdfnorm, "probability")) {
    group[[var]] <- group[[var]] / total
  } else if (identical(args$ecdfnorm, "percent")) {
    group[[var]] <- group[[var]] / total * 100
  }
  group
}

# Axes ----------------------------------------------------------------------------------------------

log10_range <- function(r) {
  lapply(r, function(v) if (is.null(v) || is.na(v)) v else log10(v))
}

cartesian_axis_patch <- function(args, letter, orders) {
  patch <- list()
  log_key <- paste0("log_", letter)
  range_key <- paste0("range_", letter)
  if (isTRUE(args[[log_key]])) {
    patch$type <- "log"
    if (!is.null(args[[range_key]])) {
      patch$range <- log10_range(args[[range_key]])
    }
  } else if (!is.null(args[[range_key]])) {
    patch$range <- args[[range_key]]
  }
  col <- args[[letter]]
  if (is.character(col) && length(col) == 1L && !is.null(orders[[col]])) {
    patch$categoryorder <- "array"
    o <- orders[[col]]
    patch$categoryarray <- I(if (letter == "x") o else rev(o))
  }
  patch
}

configure_axes <- function(args, constructor, fig, orders) {
  if (constructor %in% CARTESIAN_TYPES) {
    return(configure_cartesian_axes(args, fig, orders))
  }
  switch(
    constructor,
    scatter3d = {
      patch <- list()
      for (letter in c("x", "y", "z")) {
        ax <- list(title = list(text = get_label(args, args[[letter]])))
        if (isTRUE(args[[paste0("log_", letter)]])) {
          ax$type <- "log"
          if (!is.null(args[[paste0("range_", letter)]])) {
            ax$range <- log10_range(args[[paste0("range_", letter)]])
          }
        } else if (!is.null(args[[paste0("range_", letter)]])) {
          ax$range <- args[[paste0("range_", letter)]]
        }
        if (!is.null(orders[[args[[letter]]]])) {
          ax$categoryorder <- "array"
          ax$categoryarray <- I(orders[[args[[letter]]]])
        }
        patch[[paste0(letter, "axis")]] <- ax
      }
      update_scenes(fig, patch)
    },
    scatterternary = update_ternaries(
      fig,
      aaxis = list(title = list(text = get_label(args, args$a))),
      baxis = list(title = list(text = get_label(args, args$b))),
      caxis = list(title = list(text = get_label(args, args$c)))
    ),
    scatterpolar = ,
    scatterpolargl = ,
    barpolar = {
      patch <- list(
        angularaxis = compact(list(
          direction = args$direction,
          rotation = args$start_angle
        )),
        radialaxis = list()
      )
      for (pair in list(c("r", "radialaxis"), c("theta", "angularaxis"))) {
        colname <- args[[pair[[1L]]]]
        if (!is.null(colname) && !is.null(orders[[colname]])) {
          patch[[pair[[2L]]]]$categoryorder <- "array"
          patch[[pair[[2L]]]]$categoryarray <- I(orders[[colname]])
        }
      }
      if (isTRUE(args$log_r)) {
        patch$radialaxis$type <- "log"
        if (!is.null(args$range_r)) {
          patch$radialaxis$range <- log10_range(args$range_r)
        }
      } else if (!is.null(args$range_r)) {
        patch$radialaxis$range <- args$range_r
      }
      if (!is.null(args$range_theta)) {
        patch$sector <- args$range_theta
      }
      update_polars(fig, patch)
    },
    # plotly.py sets center=None, which produces an empty object
    scattermap = ,
    choroplethmap = ,
    densitymap = update_maps(
      fig,
      compact(list(
        center = args$center %||% empty_object(),
        zoom = args$zoom,
        style = args$map_style
      ))
    ),
    scattergeo = ,
    choropleth = update_geos(
      fig,
      compact(list(
        center = args$center %||% empty_object(),
        scope = args$scope,
        fitbounds = args$fitbounds,
        visible = args$basemap_visible,
        projection = if (!is.null(args$projection)) list(type = args$projection)
      ))
    ),
    fig
  )
}

configure_cartesian_axes <- function(args, fig, orders) {
  if (!is.null(args$marginal_x) || !is.null(args$marginal_y)) {
    return(configure_cartesian_marginal_axes(args, fig, orders))
  }
  fig <- update_yaxes(
    fig,
    c(
      list(title = list(text = get_decorated_label(args, args$y, "y"))),
      cartesian_axis_patch(args, "y", orders)
    ),
    col = 1
  )
  xpatch <- cartesian_axis_patch(args, "x", orders)
  if (!isTRUE(args$is_timeline)) {
    xpatch <- c(
      list(title = list(text = get_decorated_label(args, args$x, "x"))),
      xpatch
    )
  }
  fig <- update_xaxes(fig, xpatch, row = 1)
  if (isTRUE(args$log_x)) {
    fig <- update_xaxes(fig, type = "log")
  }
  if (isTRUE(args$log_y)) {
    fig <- update_yaxes(fig, type = "log")
  }
  if (isTRUE(args$is_timeline)) {
    fig <- update_xaxes(fig, type = "date")
  }
  if ("ecdfmode" %in% names(args)) {
    fig <- if (args$orientation == "v") {
      update_yaxes(fig, rangemode = "tozero")
    } else {
      update_xaxes(fig, rangemode = "tozero")
    }
  }
  fig
}

configure_cartesian_marginal_axes <- function(args, fig, orders) {
  grid_ref <- attr(fig, "grid_ref")
  nrows <- length(grid_ref)
  ncols <- length(grid_ref[[1L]])
  fig <- update_yaxes(fig, cartesian_axis_patch(args, "y", orders), col = 1)
  fig <- update_xaxes(fig, cartesian_axis_patch(args, "x", orders), row = 1)
  tpl_grid <- function(ax) {
    template_layout_value(args$template, c(ax, "showgrid"))
  }
  if (!is.null(args$marginal_x)) {
    fig <- update_yaxes(
      fig,
      showticklabels = FALSE,
      showline = FALSE,
      ticks = "",
      range = NULL,
      row = nrows
    )
    if (is.null(tpl_grid("yaxis"))) {
      fig <- update_yaxes(
        fig,
        showgrid = identical(args$marginal_x, "histogram"),
        row = nrows
      )
    }
    if (is.null(tpl_grid("xaxis"))) {
      fig <- update_xaxes(fig, showgrid = TRUE, row = nrows)
    }
  }
  if (!is.null(args$marginal_y)) {
    fig <- update_xaxes(
      fig,
      showticklabels = FALSE,
      showline = FALSE,
      ticks = "",
      range = NULL,
      col = ncols
    )
    if (is.null(tpl_grid("xaxis"))) {
      fig <- update_xaxes(
        fig,
        showgrid = identical(args$marginal_y, "histogram"),
        col = ncols
      )
    }
    if (is.null(tpl_grid("yaxis"))) {
      fig <- update_yaxes(fig, showgrid = TRUE, col = ncols)
    }
  }
  y_title <- get_decorated_label(args, args$y, "y")
  if (!is.null(args$marginal_x)) {
    fig <- update_yaxes(fig, title = list(text = y_title), row = 1, col = 1)
  } else {
    for (r in seq_len(nrows)) {
      fig <- update_yaxes(fig, title = list(text = y_title), row = r, col = 1)
    }
  }
  x_title <- get_decorated_label(args, args$x, "x")
  if (!is.null(args$marginal_y)) {
    fig <- update_xaxes(fig, title = list(text = x_title), row = 1, col = 1)
  } else {
    for (cc in seq_len(ncols)) {
      fig <- update_xaxes(fig, title = list(text = x_title), row = 1, col = cc)
    }
  }
  if (isTRUE(args$log_x)) {
    fig <- update_xaxes(fig, type = "log")
  }
  if (isTRUE(args$log_y)) {
    fig <- update_yaxes(fig, type = "log")
  }
  if (!is.null(args$marginal_x) && nrows >= 2L) {
    for (r in seq(2L, nrows, by = 2L)) {
      fig <- update_yaxes(
        fig,
        matches = paste0("y", ncols + 1L),
        type = NULL,
        row = r
      )
    }
  }
  if (!is.null(args$marginal_y) && ncols >= 2L) {
    for (cc in seq(2L, ncols, by = 2L)) {
      fig <- update_xaxes(fig, matches = "x2", type = NULL, col = cc)
    }
  }
  fig
}

configure_animation_controls <- function(args, constructor, fig) {
  if (is.null(args$animation_frame) || length(fig$frames) <= 1L) {
    return(fig)
  }
  frame_args <- function(duration) {
    list(
      frame = list(duration = duration, redraw = constructor != "scatter"),
      mode = "immediate",
      fromcurrent = TRUE,
      transition = list(duration = duration, easing = "linear")
    )
  }
  update_layout(
    fig,
    updatemenus = list(list(
      buttons = list(
        list(
          args = list(NULL, frame_args(500)),
          label = "&#9654;",
          method = "animate"
        ),
        list(
          args = list(list(NULL), frame_args(0)),
          label = "&#9724;",
          method = "animate"
        )
      ),
      direction = "left",
      pad = list(r = 10, t = 70),
      showactive = FALSE,
      type = "buttons",
      x = 0.1,
      xanchor = "right",
      y = 0,
      yanchor = "top"
    )),
    sliders = list(list(
      active = 0,
      yanchor = "top",
      xanchor = "left",
      currentvalue = list(
        prefix = paste0(get_label(args, args$animation_frame), "=")
      ),
      pad = list(b = 10, t = 60),
      len = 0.9,
      x = 0.1,
      y = 0,
      steps = lapply(fig$frames, function(f) {
        list(
          args = list(list(f$name), frame_args(0)),
          label = f$name,
          method = "animate"
        )
      })
    ))
  )
}
