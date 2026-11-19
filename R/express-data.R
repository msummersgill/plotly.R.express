# Built-in datasets (the R counterpart of plotly.express.data). The data
# files are plotly.py's, bundled in inst/extdata.

read_dataset <- function(name) {
  path <- system.file(
    "extdata",
    paste0(name, ".csv.gz"),
    package = "plotly.express"
  )
  con <- gzfile(path)
  on.exit(close(con))
  data.table::fread(text = readLines(con, warn = FALSE), encoding = "UTF-8")
}

#' Built-in datasets
#'
#' Datasets used in the plotly.express examples, returned as data.tables.
#'
#' * `px_data_gapminder()`: life expectancy, population and GDP per capita
#'   by country and year (1704 rows). `datetimes = TRUE` makes `year` a Date;
#'   `centroids = TRUE` keeps the `centroid_lat` and `centroid_lon` columns;
#'   `year` filters to one year; `pretty_names = TRUE` uses readable column
#'   names.
#' * `px_data_tips()`: restaurant bills and tips (244 rows).
#' * `px_data_iris()`: Fisher's iris measurements, with a numeric
#'   `species_id` (150 rows).
#' * `px_data_wind()`: wind direction and strength frequencies (128 rows).
#' * `px_data_election()`: results of the 2013 Montreal mayoral election by
#'   district (58 rows); `px_data_election_geojson()` returns the district
#'   boundaries as a GeoJSON list.
#' * `px_data_carshare()`: car-sharing availability in Montreal (249 rows).
#' * `px_data_stocks()`: weekly closing prices of six technology stocks,
#'   normalised to 1 on 2018-01-01 (105 rows). `datetimes = TRUE` makes
#'   `date` a Date.
#' * `px_data_experiment()`: results of a fictional experiment (100 rows).
#' * `px_data_medals_wide()`, `px_data_medals_long()`: medal counts by
#'   nation in wide and long form.
#'
#' @param datetimes Convert the date column to `Date`.
#' @param centroids Keep country centroid columns.
#' @param year Keep only this year.
#' @param pretty_names Use readable column names.
#' @return A data.table (a GeoJSON list for `px_data_election_geojson()`).
#' @name px_data
#' @export
#' @examples
#' head(px_data_gapminder(year = 2007))
#' head(px_data_tips())
px_data_gapminder <- function(
  datetimes = FALSE,
  centroids = FALSE,
  year = NULL,
  pretty_names = FALSE
) {
  df <- read_dataset("gapminder")
  # (index computed outside `[`: inside it, `year` would mean the column)
  if (!is.null(year)) {
    keep <- df[["year"]] == year
    df <- df[keep]
  }
  if (datetimes) {
    df[["year"]] <- as.Date(paste0(df[["year"]], "-01-01"))
  }
  if (!centroids) {
    df[, c("centroid_lat", "centroid_lon") := NULL]
  }
  if (pretty_names) {
    data.table::setnames(
      df,
      c(
        "country",
        "continent",
        "year",
        "lifeExp",
        "gdpPercap",
        "pop",
        "iso_alpha",
        "iso_num",
        "centroid_lat",
        "centroid_lon"
      ),
      c(
        "Country",
        "Continent",
        "Year",
        "Life Expectancy",
        "GDP per Capita",
        "Population",
        "ISO Alpha Country Code",
        "ISO Numeric Country Code",
        "Centroid Latitude",
        "Centroid Longitude"
      ),
      skip_absent = TRUE
    )
  }
  df[]
}

#' @rdname px_data
#' @export
px_data_tips <- function(pretty_names = FALSE) {
  df <- read_dataset("tips")
  if (pretty_names) {
    data.table::setnames(
      df,
      c("total_bill", "tip", "sex", "smoker", "day", "time", "size"),
      c(
        "Total Bill",
        "Tip",
        "Payer Gender",
        "Smokers at Table",
        "Day of Week",
        "Meal",
        "Party Size"
      )
    )
  }
  df[]
}

#' @rdname px_data
#' @export
px_data_iris <- function() read_dataset("iris")

#' @rdname px_data
#' @export
px_data_wind <- function() read_dataset("wind")

#' @rdname px_data
#' @export
px_data_election <- function() read_dataset("election")

#' @rdname px_data
#' @export
px_data_election_geojson <- function() {
  path <- system.file(
    "extdata",
    "election.geojson.gz",
    package = "plotly.express"
  )
  con <- gzfile(path)
  on.exit(close(con))
  jsonlite::parse_json(
    paste(readLines(con, encoding = "UTF-8"), collapse = "\n"),
    simplifyVector = FALSE
  )
}

#' @rdname px_data
#' @export
px_data_carshare <- function() read_dataset("carshare")

#' @rdname px_data
#' @export
px_data_stocks <- function(datetimes = FALSE) {
  df <- read_dataset("stocks")
  df[["date"]] <- if (datetimes) {
    as.Date(df[["date"]])
  } else {
    as.character(df[["date"]])
  }
  df[]
}

#' @rdname px_data
#' @export
px_data_experiment <- function() read_dataset("experiment")

#' @rdname px_data
#' @export
px_data_medals_wide <- function() read_dataset("medals")

#' @rdname px_data
#' @export
px_data_medals_long <- function() {
  wide <- read_dataset("medals")
  data.table::melt(
    wide,
    id.vars = "nation",
    variable.name = "medal",
    value.name = "count",
    variable.factor = FALSE
  )
}
