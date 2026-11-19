library(plotly.core)
library(plotly.express)

if (requireNamespace("crosstalk", quietly = TRUE)) {
  iris <- px_data_iris()
  sd <- crosstalk::SharedData$new(iris, group = "iris")

  # px_*() with a SharedData: per-point keys, the group as each trace's set,
  # trendlines standing for all their keys
  fig <- px_scatter(
    sd,
    x = ~sepal_width,
    y = ~sepal_length,
    color = ~species,
    trendline = "ols"
  )
  keys <- attr(fig, "plotly_keys")
  stopifnot(
    length(fig$data) == 6L,
    identical(unlist(attr(fig, "plotly_sets")), rep("iris", 6L)),
    identical(keys[[1]], as.character(1:50)),
    identical(keys[[2]], list(simple = as.character(1:50))),
    identical(keys[[3]], as.character(51:100)),
    # the key column isn't plotted or shown
    !any(grepl("crosstalk", unlist(fig$data[[1]]$hovertemplate)))
  )

  # The widget sends the keys and the crosstalk libraries
  w <- as_widget(fig)
  stopifnot("crosstalk" %in% vapply(w$dependencies, `[[`, "", "name"))

  # Keys follow the rows: histograms, wide-form data (a key per row and
  # variable), animation frames
  h <- px_histogram(sd, x = ~petal_length, color = ~species)
  stopifnot(identical(attr(h, "plotly_keys")[[2]], as.character(51:100)))
  wide <- px_line(crosstalk::SharedData$new(data.frame(a = 1:3, b = 4:6)))
  stopifnot(identical(
    attr(wide, "plotly_keys"),
    list(as.character(1:3), as.character(1:3))
  ))
  gm <- px_data_gapminder()
  a <- px_scatter(
    crosstalk::SharedData$new(gm, key = ~country),
    x = ~gdpPercap,
    y = ~lifeExp,
    animation_frame = ~year
  )
  fk <- attr(a, "plotly_frameKeys")
  stopifnot(
    length(fk) == length(unique(gm$year)),
    identical(
      fk[[2]]$keys[[1]],
      as.character(gm$country[gm$year == sort(unique(gm$year))[2]])
    )
  )

  # Aggregating figures can't be linked
  pie <- withCallingHandlers(
    px_pie(
      crosstalk::SharedData$new(data.frame(n = c("a", "b"), v = 1:2)),
      names = ~n,
      values = ~v
    ),
    warning = function(w) {
      stopifnot(grepl("can't be linked", conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
  stopifnot(is.null(attr(pie, "plotly_keys")))

  # Unlinked figures carry no crosstalk settings
  w0 <- as_widget(px_scatter(iris, x = ~sepal_width, y = ~sepal_length))
  stopifnot(is.null(w0$x$highlight), is.null(w0$x$sets))
}

# go_figure() / add_<trace>() with a SharedData: traces whose points are its
# rows are linked; others (and traces of plain data) aren't
if (requireNamespace("crosstalk", quietly = TRUE)) {
  sd <- crosstalk::SharedData$new(mtcars, group = "cars")
  f <- sd |>
    go_figure() |>
    add_scatter(x = ~mpg, y = ~hp, mode = "markers") |>
    add_scatter(x = c(10, 30), y = c(50, 300), mode = "lines") |>
    add_bar(x = ~cyl, y = ~mpg, data = mtcars) |>
    add_histogram(
      x = ~wt,
      data = crosstalk::SharedData$new(mtcars, group = "other")
    ) |>
    highlight(on = "plotly_selected", color = "red")
  keys <- attr(f, "plotly_keys")
  stopifnot(
    identical(attr(f, "plotly_sets"), list("cars", NULL, NULL, "other")),
    identical(keys[[1]], rownames(mtcars)),
    is.null(keys[[2]]),
    identical(as_widget(f)$x$layout$dragmode, "select")
  )
  # with subplot cells: each added trace is linked
  g <- make_subplots(rows = 1, cols = 2) |>
    add_scatter(
      x = ~mpg,
      y = ~hp,
      data = sd,
      mode = "markers",
      row = 1,
      col = "all"
    )
  stopifnot(identical(attr(g, "plotly_sets"), list("cars", "cars")))
}
