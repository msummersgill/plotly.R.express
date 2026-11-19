# px_imshow() (a port of plotly/express/_imshow.py).

#' Display an image or a matrix
#'
#' Displays a matrix as a heatmap, or an RGB/RGBA array as an image, like
#' plotly.py's `px.imshow()`. Higher-dimensional arrays can be split into
#' facets and animation frames with `facet_col`, `facet_row` and
#' `animation_frame`.
#'
#' @param img The image: a numeric or logical matrix (shown as a heatmap with
#'   a continuous color scale), a data.frame (its column and row names give
#'   `x` and `y`), or an array of dimension `c(height, width, 3)` (RGB) or
#'   `c(height, width, 4)` (RGBA) shown as an image. Raw vectors and integer
#'   arrays with values 0-255 are treated as 8-bit images; numeric images are
#'   scaled according to their range (e.g. 0-1 for [png::readPNG()] output).
#'   With `facet_col`, `facet_row` or `animation_frame`, one more dimension
#'   per argument.
#' @param zmin,zmax Minimum and maximum of the color range (per channel for
#'   RGB(A) images: a single value, or 1, 3 or 4 values).
#' @param origin `"upper"` (default, row 1 at the top) or `"lower"`.
#' @param labels Named list overriding the labels of `x`, `y`, `color`,
#'   `facet_col`, `facet_row` and `animation_frame`.
#' @param x,y Coordinates of the columns and rows (numeric for RGB images).
#' @param animation_frame,facet_col,facet_row Dimension of `img` (a number,
#'   or a name in `names(dimnames(img))`) to split into animation frames,
#'   facet columns or facet rows.
#' @param facet_col_wrap Maximum number of facet columns.
#' @param facet_col_spacing,facet_row_spacing Spacing between facets, as a
#'   fraction of the figure.
#' @param color_continuous_scale,color_continuous_midpoint,range_color Color
#'   scale settings for single-channel images (see [px_args]).
#' @param title,template,width,height See [px_args].
#' @param aspect `"equal"` (default for arrays; square pixels) or `"auto"`
#'   (fill the figure).
#' @param contrast_rescaling `"minmax"` (scale to the data range; default for
#'   single-channel images) or `"infer"` (scale to the range implied by the
#'   data type).
#' @param binary_string If `TRUE`, RGB(A) images are sent as a compressed PNG
#'   (needs the png package) instead of an array of values; the default when
#'   png is installed.
#' @param binary_backend,binary_compression_level Unused; accepted for
#'   compatibility with plotly.py.
#' @param binary_format Only `"png"` is supported.
#' @param text_auto If `TRUE` or a d3-format string, show the values as text
#'   on heatmaps.
#' @return A [go_figure()].
#' @family plotly express
#' @export
#' @examples
#' px_imshow(matrix(c(1, 20, 30, 20, 1, 60, 30, 60, 1), 3), text_auto = TRUE)
#'
#' img <- array(c(255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 0), dim = c(2, 2, 3))
#' px_imshow(img, binary_string = FALSE)
#'
#' volume <- array(stats::runif(5 * 20 * 20), dim = c(5, 20, 20))
#' px_imshow(volume, animation_frame = 1)
px_imshow <- function(
  img,
  zmin = NULL,
  zmax = NULL,
  origin = NULL,
  labels = list(),
  x = NULL,
  y = NULL,
  animation_frame = NULL,
  facet_col = NULL,
  facet_row = NULL,
  facet_col_wrap = NULL,
  facet_col_spacing = NULL,
  facet_row_spacing = NULL,
  color_continuous_scale = NULL,
  color_continuous_midpoint = NULL,
  range_color = NULL,
  title = NULL,
  template = NULL,
  width = NULL,
  height = NULL,
  aspect = NULL,
  contrast_rescaling = NULL,
  binary_string = NULL,
  binary_backend = "auto",
  binary_compression_level = 4,
  binary_format = "png",
  text_auto = FALSE
) {
  args <- list2env(as.list(environment()), envir = new.env(parent = emptyenv()))
  user_provided_colorscale <- !is.null(color_continuous_scale)
  args <- apply_default_cascade(args, constructor = NULL)
  labels <- as.list(labels)

  is_dataframe <- is.data.frame(img)
  if (is_dataframe) {
    if (isTRUE(binary_string)) {
      stop("Binary strings cannot be used with data frames", call. = FALSE)
    }
    if (is.null(x)) {
      x <- names(img)
    }
    if (is.null(y)) {
      rn <- attr(img, "row.names")
      y <- if (is.integer(rn)) seq_len(nrow(img)) - 1L else rn
    }
    img <- as.matrix(img)
  }
  if (is.raw(img)) {
    img <- array(as.integer(img), dim = dim(img))
  }
  if (is.null(dim(img))) {
    stop("`img` must be a matrix, data.frame or array", call. = FALSE)
  }
  eight_bit <- is.integer(img) &&
    all(img[!is.na(img)] >= 0 & img[!is.na(img)] <= 255)
  if (is.logical(img)) {
    img <- array(255L * as.integer(img), dim = dim(img))
    eight_bit <- TRUE
  }
  if (is.null(labels$x)) {
    labels$x <- ""
  }
  if (is.null(labels$y)) {
    labels$y <- ""
  }
  if (is.null(labels$color)) {
    labels$color <- ""
  }
  if (is.null(aspect)) {
    aspect <- "equal"
  }

  dim_index <- function(d) {
    if (is.character(d)) {
      i <- match(d, names(dimnames(img)))
      if (is.na(i)) {
        stop(
          sprintf("'%s' is not a named dimension of `img`", d),
          call. = FALSE
        )
      }
      i
    } else {
      as.integer(d)
    }
  }
  # Move slice dimensions to the front: animation, facet_row, facet_col.
  slice_dims <- c(
    animation = if (!is.null(animation_frame)) dim_index(animation_frame),
    row = if (!is.null(facet_row)) dim_index(facet_row),
    col = if (!is.null(facet_col)) dim_index(facet_col)
  )
  n_slice <- length(slice_dims)
  if (n_slice) {
    img <- aperm(img, c(slice_dims, setdiff(seq_along(dim(img)), slice_dims)))
  }
  d <- dim(img)
  nanim <- if (!is.null(animation_frame)) d[[1L]] else 1L
  nfrow <- if (!is.null(facet_row)) d[[1L + !is.null(animation_frame)]] else 1L
  nfcol <- if (!is.null(facet_col)) d[[n_slice]] else 1L
  if (!is.null(facet_row)) {
    facet_col_wrap <- NULL
  }
  if (is.null(facet_col_wrap)) {
    ncols <- nfcol
    nrows <- nfrow
  } else {
    ncols <- min(as.integer(facet_col_wrap), nfcol)
    nrows <- nfcol %/% ncols + (nfcol %% ncols > 0)
  }
  if (!is.null(animation_frame)) {
    args$animation_frame <- labels$animation_frame %||% "animation_frame"
  }
  img_dims <- d[(n_slice + 1L):length(d)]
  ndim <- length(d)

  if (is.null(binary_string)) {
    binary_string <- ndim >= 3L + n_slice &&
      !is_dataframe &&
      requireNamespace("png", quietly = TRUE)
  }
  if (!is.null(range_color)) {
    zmin <- range_color[[1L]]
    zmax <- range_color[[2L]]
  }
  if (is.null(contrast_rescaling)) {
    contrast_rescaling <- if (ndim == 2L + n_slice) "minmax" else "infer"
  }
  if (contrast_rescaling == "minmax") {
    if ((!is.null(zmin) || binary_string) && is.null(zmax)) {
      zmax <- max(img, na.rm = TRUE)
    }
    if ((!is.null(zmax) || binary_string) && is.null(zmin)) {
      zmin <- min(img, na.rm = TRUE)
    }
  } else {
    if (is.null(zmax) && !eight_bit) {
      zmax <- infer_zmax(img)
    }
    if (is.null(zmin) && !is.null(zmax)) zmin <- 0
  }

  # Slices in the order of itertools.product(animation, facet_row, facet_col)
  slice_counts <- c(
    if (!is.null(animation_frame)) nanim,
    if (!is.null(facet_row)) nfrow,
    if (!is.null(facet_col)) nfcol
  )
  combos <- if (n_slice) {
    rev(expand.grid(lapply(rev(slice_counts), seq_len)))
  } else {
    data.frame(dummy = 1L)[, 0L, drop = FALSE]
  }
  n_slices <- if (n_slice) nrow(combos) else 1L
  get_slice <- function(k) {
    if (!n_slice) {
      return(img)
    }
    idx <- c(as.list(unlist(combos[k, ])), rep(list(TRUE), length(img_dims)))
    array(do.call(`[`, c(list(img), idx, list(drop = FALSE))), dim = img_dims)
  }

  layout <- list()
  is_rgb <- length(img_dims) == 3L && img_dims[[3L]] %in% c(3L, 4L)
  img_rescaled <- NULL
  if (length(img_dims) == 2L && !binary_string) {
    if (!is.null(y) && img_dims[[1L]] != length(y)) {
      stop(
        "The length of the y vector must match the length of the first dimension of the img matrix.",
        call. = FALSE
      )
    }
    if (!is.null(x) && img_dims[[2L]] != length(x)) {
      stop(
        "The length of the x vector must match the length of the second dimension of the img matrix.",
        call. = FALSE
      )
    }
    texttemplate <- if (isTRUE(text_auto)) {
      "%{z}"
    } else if (!isFALSE(text_auto)) {
      paste0("%{z:", text_auto, "}")
    }
    traces <- lapply(seq_len(n_slices), function(k) {
      compact(list(
        type = "heatmap",
        x = x,
        y = y,
        z = get_slice(k),
        coloraxis = "coloraxis1",
        name = as.character(k - 1L),
        texttemplate = texttemplate
      ))
    })
    layout$yaxis <- list(
      autorange = if (identical(origin, "lower")) TRUE else "reversed"
    )
    if (aspect == "equal") {
      layout$xaxis <- list(scaleanchor = "y", constrain = "domain")
      layout$yaxis$constrain <- "domain"
    }
    ca <- compact(list(
      colorscale = args$color_continuous_scale,
      cmid = color_continuous_midpoint,
      cmin = zmin,
      cmax = zmax
    ))
    if (user_provided_colorscale) {
      ca$autocolorscale <- FALSE
    }
    if (nzchar(labels$color)) {
      ca$colorbar <- list(title = list(text = labels$color))
    }
    layout$coloraxis <- ca
  } else if (is_rgb || (length(img_dims) == 2L && binary_string)) {
    if (!is.null(zmin) && !is.null(zmax)) {
      zmin <- vectorize_zvalue(zmin, "min")
      zmax <- vectorize_zvalue(zmax, "max")
    }
    x0 <- y0 <- dx <- dy <- NULL
    if (!is.null(x)) {
      if (!is.numeric(x)) {
        stop(
          "Only numerical values are accepted for the `x` parameter when an Image trace is used.",
          call. = FALSE
        )
      }
      x0 <- x[[1L]]
      dx <- x[[2L]] - x[[1L]]
    }
    if (!is.null(y)) {
      if (!is.numeric(y)) {
        stop(
          "Only numerical values are accepted for the `y` parameter when an Image trace is used.",
          call. = FALSE
        )
      }
      y0 <- y[[1L]]
      dy <- y[[2L]] - y[[1L]]
    }
    if (binary_string) {
      if (!requireNamespace("png", quietly = TRUE)) {
        stop("`binary_string = TRUE` needs the png package", call. = FALSE)
      }
      if (binary_format != "png") {
        stop("Only binary_format = \"png\" is supported", call. = FALSE)
      }
      if (is.null(zmin) && is.null(zmax)) {
        img_rescaled <- img
      } else if (length(img_dims) == 2L) {
        img_rescaled <- rescale_intensity(img, zmin[[1L]], zmax[[1L]])
      } else {
        img_rescaled <- img
        nch <- img_dims[[3L]]
        idx_all <- rep(list(TRUE), length(d) - 1L)
        for (ch in seq_len(nch)) {
          sel <- do.call(`[`, c(list(img), idx_all, list(ch, drop = FALSE)))
          img_rescaled <- do.call(
            `[<-`,
            c(
              list(img_rescaled),
              idx_all,
              list(ch, value = rescale_intensity(sel, zmin[[ch]], zmax[[ch]]))
            )
          )
        }
      }
      orig <- img
      img <- img_rescaled
      traces <- lapply(seq_len(n_slices), function(k) {
        compact(list(
          type = "image",
          source = image_data_uri(get_slice(k)),
          name = as.character(k - 1L),
          x0 = x0,
          y0 = y0,
          dx = dx,
          dy = dy
        ))
      })
      img <- orig
    } else {
      colormodel <- if (img_dims[[3L]] == 3L) "rgb" else "rgba256"
      traces <- lapply(seq_len(n_slices), function(k) {
        compact(list(
          type = "image",
          z = get_slice(k),
          zmin = zmin,
          zmax = zmax,
          colormodel = colormodel,
          x0 = x0,
          y0 = y0,
          dx = dx,
          dy = dy
        ))
      })
    }
    if (identical(origin, "lower") || (!is.null(dy) && dy < 0)) {
      layout$yaxis <- list(autorange = TRUE)
    }
    if (!is.null(dx) && dx < 0) layout$xaxis <- list(autorange = "reversed")
  } else {
    stop(
      sprintf(
        "px_imshow() only accepts 2D single-channel, RGB or RGBA images. An image of dimension c(%s) was provided. Alternatively, 3-, 4-, or 5-D single or multichannel datasets can be visualized using the `facet_col`, `facet_row`, and/or `animation_frame` arguments.",
        paste(d, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  col_labels <- if (!is.null(facet_col)) {
    paste0(labels$facet_col %||% "facet_col", "=", seq_len(nfcol) - 1L)
  } else {
    character()
  }
  row_labels <- if (!is.null(facet_row)) {
    paste0(labels$facet_row %||% "facet_row", "=", seq_len(nfrow) - 1L)
  } else {
    character()
  }
  args$facet_col_wrap <- facet_col_wrap %||% 0
  fig <- init_figure(args, "xy", nrows, ncols, col_labels, row_labels)
  for (v in c("height", "width")) {
    if (!is.null(args[[v]])) layout[[v]] <- args[[v]]
  }
  if (!is.null(title)) {
    layout$title <- list(text = title)
  } else if (is.null(template_layout_value(args$template, c("margin", "t")))) {
    layout$margin <- list(t = 60)
  }
  nfacets <- nfrow * nfcol
  for (k in seq_along(traces)) {
    index <- k - 1L
    if (
      ((!is.null(facet_col) || !is.null(facet_row)) && index < nrows * ncols) ||
        index == 0L
    ) {
      fig <- add_trace(
        fig,
        traces[[k]],
        row = nrows - index %/% ncols,
        col = index %% ncols + 1L
      )
    }
  }
  if (!is.null(animation_frame)) {
    fig$frames <- lapply(seq_len(nanim), function(i) {
      list(
        data = traces[(nfacets * (i - 1L) + 1L):(nfacets * i)],
        layout = layout,
        name = as.character(i - 1L)
      )
    })
  }
  fig <- update_layout(fig, layout)
  rescaled_differs <- binary_string &&
    !is.null(img_rescaled) &&
    !isTRUE(all(img == img_rescaled))
  hovertemplate <- if (rescaled_differs) {
    sprintf(
      "%s: %%{x}<br>%s: %%{y}<extra></extra>",
      if (nzchar(labels$x)) labels$x else "x",
      if (nzchar(labels$y)) labels$y else "y"
    )
  } else {
    hover_name <- if (traces[[1L]]$type == "heatmap") {
      "%{z}"
    } else if (ndim == 2L) {
      "%{z[0]}"
    } else if (ndim == 3L && d[[ndim]] == 3L) {
      "[%{z[0]}, %{z[1]}, %{z[2]}]"
    } else {
      "%{z}"
    }
    sprintf(
      "%s: %%{x}<br>%s: %%{y}<br>%s: %s<extra></extra>",
      if (nzchar(labels$x)) labels$x else "x",
      if (nzchar(labels$y)) labels$y else "y",
      if (nzchar(labels$color)) labels$color else "color",
      hover_name
    )
  }
  fig <- update_traces(fig, hovertemplate = hovertemplate)
  if (nzchar(labels$x)) {
    fig <- update_xaxes(fig, title = list(text = labels$x), row = 1)
  }
  if (nzchar(labels$y)) {
    fig <- update_yaxes(fig, title = list(text = labels$y), col = 1)
  }
  fig <- configure_animation_controls(args, "image", fig)
  update_layout(fig, template = args$template, overwrite = TRUE)
}

vectorize_zvalue <- function(z, mode = "max") {
  alpha <- if (mode == "max") 255 else 0
  switch(
    as.character(length(z)),
    "1" = c(rep(z, 3L), alpha),
    "3" = c(z, alpha),
    "4" = z,
    stop(
      sprintf(
        "zmax can be a scalar, or a vector of length 1, 3 or 4. A value of %s was passed for zmax.",
        paste(z, collapse = ", ")
      ),
      call. = FALSE
    )
  )
}

infer_zmax <- function(img) {
  m <- max(img[is.finite(img)])
  if (m <= 1.05) {
    1
  } else if (m <= 255 * 1.05) {
    255
  } else if (m <= 65535 * 1.05) {
    65535
  } else {
    2^32
  }
}

# Rescale to 0-255 integers (as skimage's rescale_intensity(..., out_range=uint8)).
rescale_intensity <- function(img, imin, imax) {
  out <- pmin(pmax(img, imin), imax)
  if (imin != imax) {
    out <- (out - imin) / (imax - imin)
  }
  array(as.integer(trunc(out * 255)), dim = dim(img))
}

# PNG data URI for an image array of 0-255 values (height x width [x channels]).
image_data_uri <- function(a) {
  raw <- png::writePNG(a / 255)
  paste0("data:image/png;base64,", base64enc::base64encode(raw))
}
