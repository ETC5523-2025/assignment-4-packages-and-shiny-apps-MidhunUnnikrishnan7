if (!requireNamespace("gapminder", quietly = TRUE)) install.packages("gapminder")
library(dplyr)

gm_panel <- gapminder::gapminder |>
  select(country, continent, year, lifeExp, gdpPercap, pop)

latest_year <- max(gm_panel$year, na.rm = TRUE)

gm_summary <- gm_panel |>
  filter(year == latest_year) |>
  mutate(pop_mill = round(pop / 1e6, 2)) |>
  select(country, continent, year, lifeExp, gdpPercap, pop_mill)

gm_trends <- gm_panel |>
  group_by(continent, year) |>
  summarise(
    mean_lifeExp   = mean(lifeExp, na.rm = TRUE),
    mean_gdpPercap = mean(gdpPercap, na.rm = TRUE),
    pop_total      = sum(pop, na.rm = TRUE),
    .groups = "drop"
  )

usethis::use_data(gm_panel, gm_summary, gm_trends, overwrite = TRUE)