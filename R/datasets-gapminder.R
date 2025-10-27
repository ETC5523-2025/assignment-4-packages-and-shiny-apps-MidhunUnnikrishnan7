#' Gapminder country–year panel (1952–2007, 5-year cadence)
#'
#' Country-level development indicators every 5 years from 1952 to 2007.
#' Used by the package's Shiny app to explore trends by continent and year.
#'
#' @details
#' Each row is a country–year observation (5-year intervals).
#' Typical uses in this package include:
#' \itemize{
#'   \item Filtering by continent(s) and year range in the app,
#'   \item Aggregating to continent-level means (life expectancy, GDP per capita),
#'   \item Aggregating to continent-level totals (population).
#' }
#'
#' \strong{Units:}
#' \itemize{
#'   \item \code{lifeExp}: years (life expectancy at birth),
#'   \item \code{gdpPercap}: GDP per capita (inflation-adjusted international \$),
#'   \item \code{pop}: persons.
#' }
#'
#' \strong{Coverage:}
#' \itemize{
#'   \item Years: 1952–2007 (inclusive), every 5 years,
#'   \item Continents: Africa, Americas, Asia, Europe, Oceania.
#' }
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{country}{Country name (character).}
#'   \item{continent}{Continent (character). One of: "Africa", "Americas", "Asia", "Europe", "Oceania".}
#'   \item{year}{Calendar year (integer), in \{1952, 1957, ..., 2007\}.}
#'   \item{lifeExp}{Life expectancy at birth (numeric, years).}
#'   \item{gdpPercap}{GDP per capita (numeric, international \$).}
#'   \item{pop}{Population (integer, persons).}
#' }
#'
#' @source Gapminder Foundation (CC BY 3.0), via the CRAN \pkg{gapminder} package.
#' @keywords datasets
#' @examples
#' # Basic glimpse
#' head(gm_panel)
#'
#' # Continent mean life expectancy in 2007
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   library(dplyr)
#'   gm_panel |>
#'     dplyr::filter(year == 2007) |>
#'     dplyr::group_by(continent) |>
#'     dplyr::summarise(mean_lifeExp = mean(lifeExp, na.rm = TRUE))
#' }
#'
#' # Total population by continent in the 1990s
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   library(dplyr)
#'   gm_panel |>
#'     dplyr::filter(year >= 1990, year <= 1999) |>
#'     dplyr::group_by(continent, year) |>
#'     dplyr::summarise(pop_total = sum(pop, na.rm = TRUE), .groups = "drop")
#' }
#'
#' @usage data(gm_panel)
"gm_panel"

#' Gapminder latest-year snapshot by country
#'
#' One row per country for the most recent year in \code{gm_panel} (typically 2007).
#' Useful for bubble plots and ranked tables.
#'
#' @details
#' Derived from \code{gm_panel} by filtering to \code{max(year)} and
#' optionally transforming population to millions for display.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{country}{Country (character).}
#'   \item{continent}{Continent (character).}
#'   \item{year}{Latest year present (integer, usually 2007).}
#'   \item{lifeExp}{Life expectancy at birth (numeric, years).}
#'   \item{gdpPercap}{GDP per capita (numeric, international \$).}
#'   \item{pop_mill}{Population in millions (numeric).}
#' }
#' @source Derived from \code{gm_panel} (Gapminder; CC BY 3.0).
#' @keywords datasets
#' @examples
#' # Top 10 countries by population (latest year)
#' if (exists("gm_summary")) {
#'   head(gm_summary[order(-gm_summary$pop_mill), ], 10)
#' }
#' @usage data(gm_summary)
"gm_summary"

#' Gapminder continent–year aggregates
#'
#' Aggregated indicators by continent and year for trend plots and comparisons.
#'
#' @details
#' Created from \code{gm_panel} by grouping \code{continent, year} and computing:
#' \itemize{
#'   \item \code{mean_lifeExp}: mean life expectancy,
#'   \item \code{mean_gdpPercap}: mean GDP per capita,
#'   \item \code{pop_total}: total population.
#' }
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{continent}{Continent (character).}
#'   \item{year}{Calendar year (integer).}
#'   \item{mean_lifeExp}{Mean life expectancy (numeric, years).}
#'   \item{mean_gdpPercap}{Mean GDP per capita (numeric, international \$).}
#'   \item{pop_total}{Total population (integer, persons).}
#' }
#' @source Derived from \code{gm_panel} (Gapminder; CC BY 3.0).
#' @keywords datasets
#' @examples
#' # Simple line plot with ggplot2 (if available)
#' if (exists("gm_trends") && requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(gm_trends, ggplot2::aes(year, mean_lifeExp, colour = continent)) +
#'     ggplot2::geom_line()
#' }
#' @usage data(gm_trends)
"gm_trends"

