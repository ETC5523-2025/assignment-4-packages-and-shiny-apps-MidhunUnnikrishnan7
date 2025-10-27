# inst/app/app.R
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)
library(bslib)
library(scales)

# ---- Ensure packaged datasets are available even if package isn't attached
ns <- asNamespace("gapxplore")
if (!exists("gm_panel", inherits = TRUE))   gm_panel   <- get("gm_panel",   envir = ns)
if (!exists("gm_trends", inherits = TRUE))  gm_trends  <- get("gm_trends",  envir = ns)
if (!exists("gm_summary", inherits = TRUE)) gm_summary <- get("gm_summary", envir = ns)

# Derive if missing (belt & braces)
if (!exists("gm_trends", inherits = TRUE) || is.null(gm_trends)) {
  gm_trends <- gm_panel %>%
    group_by(continent, year) %>%
    summarise(
      mean_lifeExp   = mean(lifeExp, na.rm = TRUE),
      mean_gdpPercap = mean(gdpPercap, na.rm = TRUE),
      pop_total      = sum(pop, na.rm = TRUE),
      .groups = "drop"
    )
}
if (!exists("gm_summary", inherits = TRUE) || is.null(gm_summary)) {
  latest_year <- max(gm_panel$year, na.rm = TRUE)
  gm_summary <- gm_panel %>%
    filter(year == latest_year) %>%
    mutate(pop_mill = round(pop / 1e6, 2)) %>%
    select(country, continent, year, lifeExp, gdpPercap, pop_mill)
}

# ---- Constants
CONTINENTS <- sort(unique(gm_panel$continent))
YR_MIN <- min(gm_panel$year); YR_MAX <- max(gm_panel$year)

METRICS <- c("Life expectancy" = "lifeExp",
             "GDP per capita"  = "gdpPercap",
             "Population"      = "pop")

AXIS_Y <- c(
  lifeExp   = "Life expectancy (years)",
  gdpPercap = "GDP per capita (US$)",
  pop       = "Population"
)

CONT_COLORS <- c(
  Africa   = "#a3a380",
  Americas = "#bb8588",
  Asia     = "#d8a48f",
  Europe   = "#6b9080",
  Oceania  = "#cddafd"
)

ui <- navbarPage(
  title = "Gapminder Explorer",
  theme = bslib::bs_theme(
    version    = 5,
    bootswatch = "lux",            
    primary    = "#7B2CBF",
    base_font  = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Poppins"),
    code_font  = bslib::font_google("Fira Mono"),
    font_scale = 1.05
  ),
  header = tags$head( # keep CSS here (no navbar warnings)
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
  
  tabPanel(
    "Explore",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(
          "view", "View:",
          choices = c("Trends (by continent)" = "trends",
                      "Bubble (countries)"    = "bubble",
                      "Compare (bar, by continent @ year)" = "compare"),
          selected = "trends"
        ),
        checkboxGroupInput("continents", "Continents:",
                           choices = CONTINENTS, selected = CONTINENTS),
        
        conditionalPanel(
          condition = "input.view == 'trends'",
          radioButtons("metric_tr", "Metric:",
                       choices = names(METRICS), selected = "Life expectancy"),
          sliderInput("year_rng", "Year range:",
                      min = YR_MIN, max = YR_MAX, value = c(YR_MIN, YR_MAX), step = 5)
        ),
        
        conditionalPanel(
          condition = "input.view == 'bubble'",
          sliderInput("year_bubble", "Year:",
                      min = YR_MIN, max = YR_MAX, value = YR_MAX, step = 5),
          checkboxInput("logx", "Log10 GDP axis", value = TRUE)
        ),
        
        conditionalPanel(
          condition = "input.view == 'compare'",
          radioButtons("metric_cmp", "Metric:",
                       choices = names(METRICS), selected = "Life expectancy"),
          sliderInput("year_cmp", "Year:",
                      min = YR_MIN, max = YR_MAX, value = YR_MAX, step = 5)
        )
      ),
      mainPanel(
        tags$h4("Interactive views"),
        div(
          class = "app-card",  # styled in app.css
          plotlyOutput("plot", height = 460)
        ),
        conditionalPanel(
          condition = "input.view == 'compare'",
          tags$hr(),
          tags$h5(tags$b("Text summary")),
          htmlOutput("cmp_text")
        ),
        tags$hr(),
        tags$h5(tags$b("How to interpret")),
        tags$ul(
          tags$li(tags$b("Trends:"), " continent-level lines across the selected years."),
          tags$li(tags$b("Bubble:"), " countries in a single year; x = GDP per capita, y = life expectancy, size = population (millions), colour = continent."),
          tags$li(tags$b("Compare:"), " bar chart of continents for a selected year and metric.")
        )
      )
    )
  ),
  
  tabPanel(
    "About",
    tagList(
      tags$h3("About this app"),
      tags$p("This app ships with the ", tags$b("gapxplore"), " package for ",
             tags$i("ETC5523: Communicating with Data"), ". It explores Gapminder’s country-level indicators with clean aggregations and helpful context."),
      
      tags$hr(),
      tags$h4("What’s inside"),
      tags$ul(
        tags$li(tags$code("gm_panel"), " — country–year panel (1952–2007, every 5 years)."),
        tags$li(tags$code("gm_trends"), " — continent–year aggregates used for trend lines."),
        tags$li(tags$code("gm_summary"), " — one row per country for the latest year (usually 2007).")
      ),
      
      tags$h5("How aggregates are computed"),
      tags$ul(
        tags$li(tags$b("Life expectancy:"), " mean by continent & year (from country values)."),
        tags$li(tags$b("GDP per capita:"), " mean by continent & year (from country values)."),
        tags$li(tags$b("Population:"), " total by continent & year (sum of countries).")
      ),
      tags$p(
        tags$em("Why means vs totals?"),
        " Life expectancy and GDP per capita are interpreted at the country level, so",
        " continent-level comparisons use the (unweighted) mean. Population is additive, so totals are more meaningful."
      ),
      
      tags$hr(),
      tags$h4("Field meanings"),
      tags$h5(tags$code("gm_panel")),
      tags$ul(
        tags$li(tags$code("country"), ": country name"),
        tags$li(tags$code("continent"), ": Africa, Americas, Asia, Europe, Oceania"),
        tags$li(tags$code("year"), ": 1952–2007 (5-year cadence)"),
        tags$li(tags$code("lifeExp"), ": life expectancy at birth (years)"),
        tags$li(tags$code("gdpPercap"), ": GDP per capita (international $, inflation-adjusted)"),
        tags$li(tags$code("pop"), ": population (persons)")
      ),
      tags$h5(tags$code("gm_trends")),
      tags$ul(
        tags$li(tags$code("continent"), ", ", tags$code("year")),
        tags$li(tags$code("mean_lifeExp"), ": mean life expectancy (years)"),
        tags$li(tags$code("mean_gdpPercap"), ": mean GDP per capita (US$)"),
        tags$li(tags$code("pop_total"), ": total population (persons)")
      ),
      tags$h5(tags$code("gm_summary")),
      tags$ul(
        tags$li(tags$code("country"), ", ", tags$code("continent"), ", ", tags$code("year"), " (latest)"),
        tags$li(tags$code("lifeExp"), ", ", tags$code("gdpPercap")),
        tags$li(tags$code("pop_mill"), ": population in millions (for display)")
      ),
      
      tags$hr(),
      tags$h4("Sources & licensing"),
      tags$p(
        "Data derived from the CRAN ",
        tags$a(href = "https://CRAN.R-project.org/package=gapminder",
               "gapminder", target = "_blank", rel = "noopener noreferrer"),
        " package (Gapminder Foundation, CC BY 3.0)."
      ),
      tags$p(
        "The reproducible build script lives in ",
        tags$code("data-raw/build_gapminder_data.R"),
        " and runs at package build time."
      )
    )
  )
)

server <- function(input, output, session) {
  # Modern formatters
  number_short <- scales::label_number(scale_cut = scales::cut_short_scale())
  comma_lab    <- scales::label_comma()
  comma_fmt    <- scales::comma
  
  # ---- Trends view
  trends_data <- reactive({
    req(input$view == "trends", length(input$continents) > 0)
    metric_col <- METRICS[[input$metric_tr]]
    
    gm_trends %>%
      mutate(year = as.integer(year)) %>%
      arrange(continent, year) %>%
      filter(
        year >= input$year_rng[1], year <= input$year_rng[2],
        continent %in% input$continents
      ) %>%
      mutate(
        value = dplyr::case_when(
          metric_col == "lifeExp"   ~ mean_lifeExp,
          metric_col == "gdpPercap" ~ mean_gdpPercap,
          metric_col == "pop"       ~ as.numeric(pop_total),
          TRUE ~ NA_real_
        ),
        text = paste0(
          "Continent: ", continent,
          "<br>Year: ", year,
          "<br>", AXIS_Y[[metric_col]], ": ", number_short(value)
        )
      ) %>%
      filter(!is.na(value))
  })
  
  # ---- Bubble view
  bubble_data <- reactive({
    req(input$view == "bubble", length(input$continents) > 0)
    gm_panel %>%
      filter(year == input$year_bubble,
             continent %in% input$continents,
             gdpPercap > 0) %>%           # guard for log axis
      mutate(
        pop_mill = pop / 1e6,
        text = paste0(
          "Country: ", country,
          "<br>Continent: ", continent,
          "<br>Life expectancy: ", round(lifeExp, 1),
          "<br>GDP per capita: ", comma_fmt(gdpPercap),
          "<br>Population (millions): ", round(pop_mill, 2)
        )
      )
  })
  
  # ---- Compare view
  compare_data <- reactive({
    req(input$view == "compare", length(input$continents) > 0)
    metric_col <- METRICS[[input$metric_cmp]]
    gm_trends %>%
      filter(year == input$year_cmp, continent %in% input$continents) %>%
      transmute(
        continent, year,
        value = dplyr::case_when(
          metric_col == "lifeExp"   ~ mean_lifeExp,
          metric_col == "gdpPercap" ~ mean_gdpPercap,
          metric_col == "pop"       ~ as.numeric(pop_total),
          TRUE ~ NA_real_
        ),
        text = paste0(
          "Continent: ", continent,
          "<br>Year: ", year,
          "<br>", AXIS_Y[[metric_col]], ": ", number_short(value)
        )
      )
  })
  
  output$plot <- renderPlotly({
    view <- req(input$view)
    
    if (view == "trends") {
      dat <- trends_data()
      validate(need(nrow(dat) > 0, "No data for this selection."))
      
      present <- unique(dat$continent)
      pal <- CONT_COLORS[present]
      
      g <- ggplot(
        dat,
        aes(x = year, y = value, colour = continent, group = continent, text = text)
      ) +
        geom_line(linewidth = 1, na.rm = TRUE) +
        geom_point(size = 1.5, alpha = 0.9) +
        scale_colour_manual(values = pal, drop = TRUE) +
        labs(x = NULL, y = AXIS_Y[[METRICS[[input$metric_tr]]]], colour = NULL) +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      return(ggplotly(g, tooltip = "text"))
    }
    
    if (view == "bubble") {
      dat <- bubble_data()
      validate(need(nrow(dat) > 0, "No countries match the filters."))
      
      present <- unique(dat$continent)
      pal <- CONT_COLORS[present]
      
      g <- ggplot(
        dat,
        aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop_mill, text = text)
      ) +
        geom_point(alpha = 0.8) +
        scale_size_area(name = "Population (millions)", max_size = 16) +
        scale_colour_manual(values = pal, drop = TRUE) +
        { if (isTRUE(input$logx)) scale_x_log10(labels = comma_lab)
          else scale_x_continuous(labels = comma_lab) } +
        labs(x = "GDP per capita", y = "Life expectancy (years)", colour = NULL) +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      return(ggplotly(g, tooltip = "text"))
    }
    
    # compare
    dat <- compare_data()
    validate(need(nrow(dat) > 0, "No data to compare for this selection."))
    
    dat <- dat %>% arrange(desc(value)) %>%
      mutate(continent = factor(continent, levels = continent))
    pal <- CONT_COLORS[levels(dat$continent)]
    
    g <- ggplot(dat, aes(x = continent, y = value, fill = continent, text = text)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = pal, drop = TRUE) +
      labs(x = NULL, y = AXIS_Y[[METRICS[[input$metric_cmp]]]]) +
      theme_minimal()
    
    ggplotly(g, tooltip = "text") %>% layout(showlegend = FALSE)
  })
  
  output$cmp_text <- renderUI({
    req(input$view == "compare")
    dat <- compare_data() %>% arrange(desc(value))
    items <- lapply(seq_len(nrow(dat)), function(i) {
      tags$li(tags$b(as.character(dat$continent[i])), ": ", number_short(dat$value[i]))
    })
    tags$div(
      tags$p("Year: ", tags$b(input$year_cmp),
             " · Metric: ", tags$b(names(METRICS)[METRICS == METRICS[[input$metric_cmp]]])
      ),
      tags$ul(items)
    )
  })
}

shinyApp(ui, server)