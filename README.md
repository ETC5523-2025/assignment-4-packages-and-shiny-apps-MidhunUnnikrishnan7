# gapxplore — Interactive Gapminder Explorer (ETC5523 Assignment 4)

<!-- badges: start -->
<!-- Add pkgdown badge after first deploy:
[![pkgdown](https://github.com/ETC5523-2025/assignment-4-packages-and-shiny-apps-MidhunUnnikrishnan7/actions/workflows/pkgdown.yaml/badge.svg)](https://ETC5523-2025.github.io/assignment-4-packages-and-shiny-apps-MidhunUnnikrishnan7/)
-->
<!-- badges: end -->

## Overview

**gapxplore** is an R package for **ETC5523: Communicating with Data**.  
It includes a documented Gapminder dataset, an **interactive Shiny app**, and a **pkgdown website**.

## Installation

```r
# {pak}
pak::pak("ETC5523-2025/assignment-4-packages-and-shiny-apps-MidhunUnnikrishnan7")

# or {remotes}
remotes::install_github("ETC5523-2025/assignment-4-packages-and-shiny-apps-MidhunUnnikrishnan7")
```

Load:

```r
library(gapxplore)
```

## Quick start (Shiny app)

```r
gapxplore::run_app()
```

## Data

This package ships a tidy Gapminder dataset (`gm_panel`) with:

- `country`, `continent`, `year`
- `lifeExp` (years), `gdpPercap` (US$), `pop` (persons)

**Attribution:** Gapminder (CC BY 3.0), via the CRAN **gapminder** package.

## Vignette

- Local: `browseVignettes("gapxplore")`  
- Web: https://ETC5523-2025.github.io/assignment-4-packages-and-shiny-apps-MidhunUnnikrishnan7/

## Documentation website (pkgdown)

- **Site:** https://ETC5523-2025.github.io/assignment-4-packages-and-shiny-apps-MidhunUnnikrishnan7/  
- Contains function reference, dataset help pages, and the vignette.

## Repository layout

```
R/           # functions & data docs
inst/app/    # Shiny app
data/        # packaged dataset (.rda)
data-raw/    # script to create data
vignettes/   # vignette(s)
man/         # auto-generated Rd files
```

## Development

```r
devtools::document()
devtools::check()
pkgdown::build_site()
```

## License

MIT © Midhun Unnikrishnan  
Data © Gapminder (CC BY 3.0)