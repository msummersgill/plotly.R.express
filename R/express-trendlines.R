# Trendlines (a port of plotly/express/trendline_functions). plotly.py uses
# statsmodels (OLS, LOWESS) and pandas (rolling, expanding, ewm); these use
# stats::lm(), stats::lowess() and equivalent window functions.

# Compute a trendline for one trace's data; NULL if there are too few points.
compute_trendline <- function(args, df) {
  xcol <- args$x
  ycol <- args$y
  if (is.null(xcol) || is.null(ycol)) {
    return(NULL)
  }
  if (sum(!is.na(df[[xcol]]) & !is.na(df[[ycol]])) <= 1L) {
    return(NULL)
  }
  o <- order(df[[xcol]], na.last = TRUE, method = "radix")
  sorted <- df[o]
  x_raw <- sorted[[xcol]]
  x <- if (inherits(x_raw, c("Date", "POSIXt"))) {
    as.numeric(as.POSIXct(x_raw))
  } else {
    x_raw
  }
  if (!is.numeric(x)) {
    x <- suppressWarnings(as.numeric(as.character(x)))
    if (anyNA(x[!is.na(x_raw)])) {
      stop(
        sprintf(
          "Could not convert value of 'x' ('%s') into a numeric type. If 'x' contains stringified dates, please convert to a Date or POSIXct column.",
          xcol
        ),
        call. = FALSE
      )
    }
  }
  y <- sorted[[ycol]]
  if (!is.numeric(y)) {
    y <- suppressWarnings(as.numeric(as.character(y)))
    if (anyNA(y[!is.na(sorted[[ycol]])])) {
      stop("Could not convert value of 'y' into a numeric type.", call. = FALSE)
    }
  }
  non_missing <- !(is.na(x) | is.na(y))
  fn <- TRENDLINE_FUNCTIONS[[args$trendline]]
  res <- fn(
    args$trendline_options %||% list(),
    x_raw,
    x,
    y,
    xcol,
    ycol,
    non_missing
  )
  if (length(res$y) != sum(non_missing)) {
    stop("missing-data-handling failure in trendline code", call. = FALSE)
  }
  list(
    x = x_raw[non_missing],
    y = res$y,
    hover_header = res$hover_header,
    fit_results = res$fit_results
  )
}

check_trendline_options <- function(options, valid, what) {
  bad <- setdiff(names(options), valid)
  if (length(bad)) {
    stop(
      sprintf(
        "%s trendline_options keys must be one of [%s] but got '%s'",
        what,
        paste(valid, collapse = ", "),
        bad[[1L]]
      ),
      call. = FALSE
    )
  }
}

trendline_ols <- function(options, x_raw, x, y, x_label, y_label, non_missing) {
  check_trendline_options(options, c("add_constant", "log_x", "log_y"), "OLS")
  add_constant <- options$add_constant %||% TRUE
  if (isTRUE(options$log_y)) {
    if (any(y <= 0, na.rm = TRUE)) {
      stop(
        "Can't do OLS trendline with `log_y=True` when `y` contains non-positive values.",
        call. = FALSE
      )
    }
    y <- log10(y)
    y_label <- sprintf("log10(%s)", y_label)
  }
  if (isTRUE(options$log_x)) {
    if (any(x <= 0, na.rm = TRUE)) {
      stop(
        "Can't do OLS trendline with `log_x=True` when `x`  contains non-positive values.",
        call. = FALSE
      )
    }
    x <- log10(x)
    x_label <- sprintf("log10(%s)", x_label)
  }
  fit <- if (add_constant) {
    stats::lm(y ~ x, na.action = stats::na.omit)
  } else {
    stats::lm(y ~ x - 1, na.action = stats::na.omit)
  }
  y_out <- unname(stats::fitted(fit))
  if (isTRUE(options$log_y)) {
    y_out <- 10^y_out
  }
  co <- unname(stats::coef(fit))
  header <- "<b>OLS trendline</b><br>"
  header <- paste0(
    header,
    if (length(co) == 2L) {
      sprintf(
        "%s = %s * %s + %s<br>",
        y_label,
        fmt_g(co[[2L]]),
        x_label,
        fmt_g(co[[1L]])
      )
    } else {
      sprintf("%s = %s * %s<br>", y_label, fmt_g(co[[1L]]), x_label)
    }
  )
  # statsmodels reports uncentered R^2 for models without a constant
  r2 <- if (add_constant) {
    summary(fit)$r.squared
  } else {
    1 - sum(stats::residuals(fit)^2) / sum(y[non_missing]^2)
  }
  header <- paste0(header, sprintf("R<sup>2</sup>=%f<br><br>", r2))
  list(y = y_out, hover_header = header, fit_results = fit)
}

# Python's "%g" formatting.
fmt_g <- function(x) sprintf("%g", x)

trendline_lowess <- function(
  options,
  x_raw,
  x,
  y,
  x_label,
  y_label,
  non_missing
) {
  check_trendline_options(options, "frac", "LOWESS")
  frac <- options$frac %||% 0.6666666
  fit <- stats::lowess(
    x[non_missing],
    y[non_missing],
    f = frac,
    iter = 3L,
    delta = 0
  )
  list(
    y = fit$y,
    hover_header = "<b>LOWESS trendline</b><br><br>",
    fit_results = NULL
  )
}

WINDOW_FUNCTIONS <- list(
  mean = function(v) mean(v),
  median = function(v) stats::median(v),
  sum = function(v) sum(v),
  min = function(v) min(v),
  max = function(v) max(v),
  count = function(v) length(v),
  std = function(v) if (length(v) > 1L) stats::sd(v) else NA_real_,
  var = function(v) if (length(v) > 1L) stats::var(v) else NA_real_
)

# pandas-style rolling/expanding/ewm window functions.
trendline_window <- function(
  mode,
  options,
  x_raw,
  x,
  y,
  x_label,
  y_label,
  non_missing
) {
  function_name <- options$`function` %||% "mean"
  options$`function` <- NULL
  options$function_args <- NULL
  f <- WINDOW_FUNCTIONS[[function_name]]
  if (is.null(f)) {
    stop(
      sprintf(
        "Unsupported trendline function '%s'; use one of %s",
        function_name,
        paste(names(WINDOW_FUNCTIONS), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  n <- length(y)
  out <- rep(NA_real_, n)
  if (mode == "rolling") {
    window <- options$window
    if (!is.numeric(window) || length(window) != 1L) {
      stop(
        "rolling trendline_options must include a numeric `window` (time-based windows are not supported)",
        call. = FALSE
      )
    }
    min_periods <- options$min_periods %||% window
    center <- isTRUE(options$center)
    for (i in seq_len(n)) {
      hi <- if (center) i + window %/% 2L else i
      lo <- hi - window + 1L
      idx <- max(1L, lo):min(n, hi)
      v <- y[idx]
      v <- v[!is.na(v)]
      if (length(v) >= min_periods && hi <= n) out[[i]] <- f(v)
    }
  } else if (mode == "expanding") {
    min_periods <- options$min_periods %||% 1L
    for (i in seq_len(n)) {
      v <- y[seq_len(i)]
      v <- v[!is.na(v)]
      if (length(v) >= min_periods) out[[i]] <- f(v)
    }
  } else {
    if (function_name != "mean") {
      stop(
        "Only the 'mean' function is supported for ewm trendlines",
        call. = FALSE
      )
    }
    alpha <- if (!is.null(options$alpha)) {
      options$alpha
    } else if (!is.null(options$span)) {
      2 / (options$span + 1)
    } else if (!is.null(options$com)) {
      1 / (1 + options$com)
    } else if (!is.null(options$halflife)) {
      1 - exp(-log(2) / options$halflife)
    } else {
      stop(
        "ewm trendline_options must include one of `com`, `span`, `halflife` or `alpha`",
        call. = FALSE
      )
    }
    adjust <- options$adjust %||% TRUE
    min_periods <- max(1L, options$min_periods %||% 0L)
    num <- den <- 0
    prev <- NA_real_
    count <- 0L
    for (i in seq_len(n)) {
      num <- num * (1 - alpha)
      den <- den * (1 - alpha)
      if (!is.na(y[[i]])) {
        count <- count + 1L
        if (adjust) {
          num <- num + y[[i]]
          den <- den + 1
        } else {
          prev <- if (is.na(prev)) {
            y[[i]]
          } else {
            (1 - alpha) * prev + alpha * y[[i]]
          }
        }
      }
      val <- if (adjust) num / den else prev
      if (count >= min_periods) out[[i]] <- val
    }
  }
  modes <- c(
    rolling = "Rolling",
    ewm = "Exponentially Weighted",
    expanding = "Expanding"
  )
  list(
    y = out[non_missing],
    hover_header = sprintf(
      "<b>%s %s trendline</b><br><br>",
      modes[[mode]],
      function_name
    ),
    fit_results = NULL
  )
}

TRENDLINE_FUNCTIONS <- list(
  lowess = trendline_lowess,
  rolling = function(...) trendline_window("rolling", ...),
  ewm = function(...) trendline_window("ewm", ...),
  expanding = function(...) trendline_window("expanding", ...),
  ols = trendline_ols
)

#' Trendline fit results
#'
#' Returns the fitted models behind the OLS trendlines of a figure created by
#' a `px_*()` function with `trendline = "ols"`, one row per trendline, like
#' plotly.py's `px.get_trendline_results()`.
#'
#' @param fig A figure returned by [px_scatter()] or a similar function.
#' @return A data.table with a column per grouping variable and a list column
#'   `px_fit_results` of [stats::lm()] fits (`NULL` for other trendline types).
#' @export
#' @examples
#' fig <- px_data_tips() |>
#'   px_scatter(x = ~total_bill, y = ~tip, color = ~smoker, trendline = "ols")
#' res <- px_get_trendline_results(fig)
#' summary(res$px_fit_results[[1]])
px_get_trendline_results <- function(fig) {
  rows <- attr(fig, "px_trendlines")
  if (!length(rows)) {
    return(data.table::data.table())
  }
  cols <- unique(unlist(lapply(rows, names)))
  out <- lapply(stats_named(as.list(cols), cols), function(col) {
    vals <- lapply(rows, `[[`, col)
    if (col == "px_fit_results") {
      vals
    } else {
      vapply(
        vals,
        function(v) if (is.null(v)) NA_character_ else as.character(v),
        ""
      )
    }
  })
  data.table::as.data.table(out)
}
