# gapxplore

**gapxplore** is an R package for *ETC5523: Communicating with Data* that turns the Gapminder dataset into a polished, interactive exploration.

- A Shiny app for trends, country bubbles, and continent comparisons  
- Tidy, documented datasets bundled in the package  
- A pkgdown website with function docs and a getting-started vignette

## Install

```r
# install.packages("remotes")
remotes::install_github("ETC5523-2025/assignment-4-packages-and-shiny-apps-MidhunUnnikrishnan7")
```

## Launch the app

```r
gapxplore::run_app()
```

## Data shipped with the package

- `gm_panel` — country–year panel (1952–2007, every 5 years)  
- `gm_trends` — continent–year aggregates (means/totals)  
- `gm_summary` — latest-year snapshot by country

## Learn more

See **Articles → Getting started** for a quick tour and tips on interpreting the app's views.

## Source & license

Data derived from the CRAN `gapminder` package (Gapminder Foundation; CC BY 3.0).