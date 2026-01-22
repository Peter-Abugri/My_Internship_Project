
# Setting up working directory

setwd("/home/peter/Downloads/exercise/")
getwd()

# Name: PETER ABUGRI

# MALARIA SIMULATION DASHBOARD

# Project: Swiss TPH Malaria Intervention Analysis

# PERFORMANCE STRATEGIES IMPLEMENTED:
# 1. Database connection pooling (prevents connection exhaustion)
# 2. Memoization of expensive operations (shapefile loads, queries)
# 3. Selective column retrieval (only fetch what's needed)
# 4. Geometry simplification (faster map rendering)
# 5. data.table operations (efficient data manipulation)
# 
# AI ASSISTANCE: Claude AI assisted with errors corrections and structure

# I spent alot of time getting some packages installed example the sf package because of the dependencies.
#I changed laptop recently so I lost all my previous packages. 

# Load required packages
library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(pool)
library(data.table)
library(dplyr)
library(sf)
library(leaflet)
library(plotly)
library(DT)
library(glue)
library(memoise)
library(htmltools)

# Configuration
DB_PATH <- "input_data.sqlite"
SHAPE_PATH <- "ch_shapefiles/"
LOGO_PATH <- "www/logo.png"  # Place your logo here
LOGO_WIDTH <- "180px"

# Brand colors - Customize to match your organization
COLORS <- list(
  primary = "#2C3E50", secondary = "#E74C3C", success = "#27AE60",
  info = "#3498DB", warning = "#F39C12", background = "#F8F9FA"
)

# Organization branding
ORG_NAME <- "Swiss Tropical and Public Health Institute"
ORG_SUBTITLE <- "Swiss Tropical and Public Health Institute"
DASHBOARD_TITLE <- "Malaria Simulation Dashboard (Fictional Scenario)"
FOOTER_TEXT <- "© 2024 Swiss TPH | Confidential"

# Database connection pool (1-5 connections, auto-managed) 
pool_db <- dbPool(RSQLite::SQLite(), dbname = DB_PATH, minSize = 1, maxSize = 5)
onStop(function() tryCatch(poolClose(pool_db), silent = TRUE))

# Memoized data functions (cached for performance)

# Load shapefile once, cache result
load_admin_sf <- memoise(function() {
  if (!dir.exists(SHAPE_PATH)) return(NULL)
  shp_files <- list.files(SHAPE_PATH, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
  if (length(shp_files) == 0) return(NULL)
  
  # Load, transform to WGS84, simplify geometry (1% tolerance for speed)
  st_read(shp_files[1], quiet = TRUE) %>% 
    st_transform(4326) %>% 
    mutate(geometry = st_simplify(geometry, dTolerance = 0.01))
})

# Detect primary table in database
get_table_name <- function() {
  tabs <- dbListTables(pool_db)
  if ("simulations" %in% tabs) return("simulations")
  if (length(tabs) > 0) return(tabs[1])
  stop("No tables found in database: ", DB_PATH)
}

TABLE_NAME <- tryCatch(get_table_name(), error = function(e) stop("DB detection failed: ", e$message))

# Get year range for slider initialization
get_year_range_db <- function() {
  res <- tryCatch(
    dbGetQuery(pool_db, glue("SELECT MIN(year) AS miny, MAX(year) AS maxy FROM {TABLE_NAME}")),
    error = function(e) data.frame(miny = NA, maxy = NA)
  )
  if (nrow(res) == 0 || is.na(res$miny)) return(c(2020, 2030))
  c(as.integer(res$miny), as.integer(res$maxy))
}

# Fetch filtered data (memoized - same inputs return cached results)
get_sim_data <- memoise(function(age_group = NULL, year_min = NULL, year_max = NULL) {
  # Get available columns
  col_names <- dbGetQuery(pool_db, glue("PRAGMA table_info('{TABLE_NAME}')"))$name
  
  # Core columns + age_group + intervention columns
  core_cols <- c("country", "admin_1", "scenario_name", "seed", "EIR", "year",
                 "nHost", "prevalenceRate", "incidenceRate", "nUncomp", "tUncomp",
                 "nSevere", "tSevere", "expectedDirectDeaths")
  select_cols <- c(intersect(core_cols, col_names),
                   if ("age_group" %in% col_names) "age_group",
                   grep("^deployed_int_", col_names, value = TRUE))
  
  # Build filtered SQL query
  sql <- glue("SELECT {paste(unique(select_cols), collapse = ', ')} FROM {TABLE_NAME}")
  where_clauses <- c(
    if (!is.null(age_group) && "age_group" %in% col_names) glue("age_group = '{age_group}'"),
    if (!is.null(year_min)) glue("year >= {as.integer(year_min)}"),
    if (!is.null(year_max)) glue("year <= {as.integer(year_max)}")
  )
  if (length(where_clauses) > 0) {
    sql <- paste(sql, "WHERE", paste(where_clauses, collapse = " AND "))
  }
  
  # Execute and return as data.table
  df <- tryCatch(dbGetQuery(pool_db, sql), error = function(e) data.frame())
  as.data.table(df)
})

# User interface (UI)
yr_range <- get_year_range_db()

ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(
    version = 5, preset = "cosmo",
    primary = COLORS$primary, secondary = COLORS$secondary,
    success = COLORS$success, info = COLORS$info, warning = COLORS$warning,
    base_font = font_google("Lato"), heading_font = font_google("Montserrat")
  ),
  
  # Header with logo (Wanted to add logo)
  div(style = "background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%); color: white; padding: 20px; margin: -15px -15px 20px -15px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
      div(style = "display: flex; align-items: center; gap: 20px; margin-bottom: 15px;",
          if (file.exists(LOGO_PATH)) tags$img(src = basename(LOGO_PATH), style = "background: white; padding: 10px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.15);", width = LOGO_WIDTH),
          div(h1(style = "font-size: 1.8em; font-weight: 600; margin: 0; text-shadow: 1px 1px 2px rgba(0,0,0,0.2);", DASHBOARD_TITLE),
              p(style = "font-size: 1.1em; opacity: 0.95; margin: 5px 0 0 0; font-weight: 300;", ORG_SUBTITLE)))),
  
  # Sidebar controls
  sidebar = sidebar(
    width = 300,
    h4("Analysis Controls", style = "margin-top: 0; color: #2C3E50;"),
    selectInput("age_group", tags$span(icon("users"), " Age Group:"), 
                choices = c("All ages" = "all"), selected = "all"),
    radioButtons("baseline_scenario", tags$span(icon("chart-line"), " Baseline:"),
                 choices = c("Business as Usual (BAU)" = "BAU", "National Strategic Plan (NSP)" = "NSP"),
                 selected = "BAU"),
    checkboxInput("show_reduction", tags$span(icon("calculator"), " Calculate reduction vs baseline"), TRUE),
    sliderInput("year_range", tags$span(icon("calendar"), " Time Period:"),
                min = yr_range[1], max = yr_range[2], value = yr_range, step = 1, sep = ""),
    hr(),
    div(class = "alert alert-info", style = "font-size: 0.9em;",
        icon("info-circle"), " Map displays NSP intervention combinations."),
    accordion(accordion_panel("System Information", icon = icon("database"),
                              verbatimTextOutput("system_info", placeholder = TRUE)))
  ),
  
  # Main content tabs
  navset_card_tab(
    title = "Analysis Dashboard(Fictional)",
    
    # Temporal Trends - Split into sub-tabs
    nav_panel("Temporal Trends", icon = icon("chart-line"),
              navset_card_pill(
                nav_panel("Incidence Rate",
                          card(full_screen = TRUE,
                               card_header("Incidence Rate Over Time"),
                               plotlyOutput("plot_incidence", height = "600px"))),
                nav_panel("Prevalence Rate",
                          card(full_screen = TRUE,
                               card_header("Prevalence Rate Over Time"),
                               plotlyOutput("plot_prevalence", height = "600px")))
              )),
    
    # Impact Assessment - Split into sub-tabs
    nav_panel("Impact Assessment", icon = icon("chart-bar"),
              conditionalPanel("input.show_reduction == true",
                               navset_card_pill(
                                 nav_panel("Prevalence Reduction",
                                           card(full_screen = TRUE,
                                                card_header("Prevalence Reduction vs Baseline (%)"),
                                                plotlyOutput("plot_prev_reduction", height = "600px"))),
                                 nav_panel("Incidence Reduction",
                                           card(full_screen = TRUE,
                                                card_header("Incidence Reduction vs Baseline (%)"),
                                                plotlyOutput("plot_inc_reduction", height = "600px")))
                               )),
              conditionalPanel("input.show_reduction == false",
                               div(class = "alert alert-warning text-center", style = "margin: 50px;",
                                   icon("exclamation-triangle", style = "font-size: 3em;"),
                                   h4("Reduction Analysis Disabled"),
                                   p("Enable 'Calculate reduction vs baseline' in the sidebar to view this analysis.")))),
    
    # Geographic Distribution
    nav_panel("Geographic Distribution", icon = icon("map"),
              card(full_screen = TRUE, card_header("NSP Intervention Deployment by Admin Unit"),
                   leafletOutput("map_interventions", height = "650px"))),
    
    # Data Explorer
    nav_panel("Data Explorer", icon = icon("table"),
              card(card_header("Simulation Data Sample", class = "d-flex justify-content-between",
                               div(downloadButton("download_data", "Download CSV", class = "btn-sm"))),
                   DTOutput("data_table")))
  )
)

# Server 
server <- function(input, output, session) {
  
  # Populate age group choices dynamically
  observe({
    cols <- tryCatch(dbGetQuery(pool_db, glue("PRAGMA table_info('{TABLE_NAME}')")), error = function(e) NULL)
    if (!is.null(cols) && "age_group" %in% cols$name) {
      age_groups <- tryCatch(dbGetQuery(pool_db, glue("SELECT DISTINCT age_group FROM {TABLE_NAME} ORDER BY age_group")),
                             error = function(e) data.frame(age_group = character(0)))
      if (nrow(age_groups) > 0) {
        updateSelectInput(session, "age_group", 
                          choices = setNames(c("all", age_groups$age_group), c("All ages", age_groups$age_group)))
      }
    }
  })
  
  # Reactive data
  admin_boundaries <- reactive({ load_admin_sf() })
  
  simulation_data <- reactive({
    age_sel <- if (input$age_group == "all") NULL else input$age_group
    dt <- get_sim_data(age_sel, input$year_range[1], input$year_range[2])
    
    # Ensure required columns exist
    if (nrow(dt) > 0) {
      if (!"prevalenceRate" %in% names(dt)) dt[, prevalenceRate := NA_real_]
      if (!"incidenceRate" %in% names(dt)) dt[, incidenceRate := NA_real_]
      if (!"nHost" %in% names(dt)) dt[, nHost := 1]
    }
    dt
  })
  
  # Population-weighted aggregation by scenario and year
  aggregated_timeseries <- reactive({
    dt <- simulation_data()
    if (!is.data.table(dt) || nrow(dt) == 0) return(data.table())
    
    dt[, .(total_population = sum(nHost, na.rm = TRUE),
           prevalence = sum(prevalenceRate * nHost, na.rm = TRUE) / sum(nHost, na.rm = TRUE),
           incidence = sum(incidenceRate * nHost, na.rm = TRUE) / sum(nHost, na.rm = TRUE),
           deaths = sum(expectedDirectDeaths, na.rm = TRUE)),
       by = .(scenario_name, year)]
  })
  
  # System info output
  output$system_info <- renderPrint({
    cat(sprintf("Database Table: %s\n", TABLE_NAME),
        sprintf("Tables: %s\n", paste(tryCatch(dbListTables(pool_db), error = function(e) character(0)), collapse = ", ")),
        sprintf("Shapefile: %s\n", if (!is.null(admin_boundaries())) "✓ Loaded" else "✗ Not found"),
        sprintf("Filtered Rows: %s\n", format(nrow(simulation_data()), big.mark = ",")))
  })
  
  # Incidence plot
  output$plot_incidence <- renderPlotly({
    dt <- aggregated_timeseries()
    if (nrow(dt) == 0) return(NULL)
    
    plot_ly(as.data.frame(dt), x = ~year, y = ~incidence, color = ~scenario_name,
            type = "scatter", mode = "lines+markers", line = list(width = 3), marker = list(size = 8)) %>%
      layout(xaxis = list(title = "Year", gridcolor = "#E5E5E5"),
             yaxis = list(title = "Incidence Rate", gridcolor = "#E5E5E5"),
             hovermode = "x unified", legend = list(orientation = "h", y = -0.2),
             plot_bgcolor = "#FAFAFA", paper_bgcolor = "#FFFFFF") %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  # Prevalence plot
  output$plot_prevalence <- renderPlotly({
    dt <- aggregated_timeseries()
    if (nrow(dt) == 0) return(NULL)
    
    plot_ly(as.data.frame(dt), x = ~year, y = ~prevalence, color = ~scenario_name,
            type = "scatter", mode = "lines+markers", line = list(width = 3), marker = list(size = 8)) %>%
      layout(xaxis = list(title = "Year", gridcolor = "#E5E5E5"),
             yaxis = list(title = "Prevalence Rate", gridcolor = "#E5E5E5"),
             hovermode = "x unified", legend = list(orientation = "h", y = -0.2),
             plot_bgcolor = "#FAFAFA", paper_bgcolor = "#FFFFFF") %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  # Reduction plots - Split into separate prevalence and incidence
  
  # Prevalence reduction plot
  output$plot_prev_reduction <- renderPlotly({
    dt <- simulation_data()
    if (!is.data.table(dt) || nrow(dt) == 0 || !"admin_1" %in% names(dt)) return(NULL)
    
    # Aggregate by admin unit, scenario, year
    agg <- dt[, .(prevalence = mean(prevalenceRate, na.rm = TRUE)),
              by = .(admin_1, scenario_name, year)]
    
    # Reshape to wide format - column names will be like "BAU", "NSP", etc.
    wide <- tryCatch(dcast(agg, admin_1 + year ~ scenario_name, value.var = "prevalence"),
                     error = function(e) data.table())
    if (nrow(wide) == 0) return(NULL)
    
    # Find baseline and comparison columns (look for columns that start with baseline name)
    baseline <- input$baseline_scenario
    scenario_cols <- setdiff(names(wide), c("admin_1", "year"))
    
    # Find baseline column - match columns that start with baseline prefix
    base_col <- scenario_cols[grep(paste0("^", baseline), scenario_cols)][1]
    compare_cols <- setdiff(scenario_cols, base_col)
    
    if (is.na(base_col) || length(compare_cols) == 0) return(NULL)
    
    # Calculate % reduction using first comparison scenario
    compare_col <- compare_cols[1]
    wide[, prev_reduction := 100 * (get(base_col) - get(compare_col)) / pmax(0.0001, get(base_col))]
    
    if (!"prev_reduction" %in% names(wide)) return(NULL)
    
    plot_data <- as.data.frame(wide)
    
    # Create prevalence reduction plot
    plot_ly(plot_data, x = ~year, y = ~prev_reduction, color = ~admin_1,
            type = "scatter", mode = "lines+markers", 
            line = list(width = 2.5), marker = list(size = 6)) %>%
      layout(
        title = list(text = paste("Prevalence Reduction:", compare_col, "vs", baseline, "Baseline"), 
                     font = list(size = 18, color = "#2C3E50")),
        xaxis = list(title = "Year", gridcolor = "#E5E5E5", showgrid = TRUE),
        yaxis = list(title = "Prevalence Reduction (%)", gridcolor = "#E5E5E5", showgrid = TRUE,
                     zeroline = TRUE, zerolinecolor = "#999", zerolinewidth = 2),
        hovermode = "x unified",
        legend = list(title = list(text = "Administrative Unit")),
        plot_bgcolor = "#FAFAFA",
        paper_bgcolor = "#FFFFFF"
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  # Incidence reduction plot
  output$plot_inc_reduction <- renderPlotly({
    dt <- simulation_data()
    if (!is.data.table(dt) || nrow(dt) == 0 || !"admin_1" %in% names(dt)) return(NULL)
    
    # Aggregate by admin unit, scenario, year
    agg <- dt[, .(incidence = mean(incidenceRate, na.rm = TRUE)),
              by = .(admin_1, scenario_name, year)]
    
    # Reshape to wide format - column names will be like "BAU", "NSP", etc.
    wide <- tryCatch(dcast(agg, admin_1 + year ~ scenario_name, value.var = "incidence"),
                     error = function(e) data.table())
    if (nrow(wide) == 0) return(NULL)
    
    # Find baseline and comparison columns
    baseline <- input$baseline_scenario
    scenario_cols <- setdiff(names(wide), c("admin_1", "year"))
    
    # Find baseline column - match columns that start with baseline prefix
    base_col <- scenario_cols[grep(paste0("^", baseline), scenario_cols)][1]
    compare_cols <- setdiff(scenario_cols, base_col)
    
    if (is.na(base_col) || length(compare_cols) == 0) return(NULL)
    
    # Calculate % reduction using first comparison scenario
    compare_col <- compare_cols[1]
    wide[, inc_reduction := 100 * (get(base_col) - get(compare_col)) / pmax(0.0001, get(base_col))]
    
    if (!"inc_reduction" %in% names(wide)) return(NULL)
    
    plot_data <- as.data.frame(wide)
    
    # Create incidence reduction plot
    plot_ly(plot_data, x = ~year, y = ~inc_reduction, color = ~admin_1,
            type = "scatter", mode = "lines+markers",
            line = list(width = 2.5), marker = list(size = 6)) %>%
      layout(
        title = list(text = paste("Incidence Reduction:", compare_col, "vs", baseline, "Baseline"),
                     font = list(size = 18, color = "#2C3E50")),
        xaxis = list(title = "Year", gridcolor = "#E5E5E5", showgrid = TRUE),
        yaxis = list(title = "Incidence Reduction (%)", gridcolor = "#E5E5E5", showgrid = TRUE,
                     zeroline = TRUE, zerolinecolor = "#999", zerolinewidth = 2),
        hovermode = "x unified",
        legend = list(title = list(text = "Administrative Unit")),
        plot_bgcolor = "#FAFAFA",
        paper_bgcolor = "#FFFFFF"
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  # Map of NSP interventions
  output$map_interventions <- renderLeaflet({
    dt <- simulation_data()
    sf_obj <- admin_boundaries()
    
    # Default map view (Switzerland coordinates)
    default_map <- function(msg) {
      leaflet() %>% addTiles() %>% setView(lng = 8.2275, lat = 46.8182, zoom = 8) %>%
        addPopups(8.2275, 46.8182, msg)
    }
    
    if (is.null(sf_obj)) return(default_map("Shapefile not available"))
    if (!is.data.table(dt) || nrow(dt) == 0) return(default_map("No simulation data"))
    
    # Filter NSP scenario, latest year only
    nsp_data <- dt[grepl("^NSP", scenario_name) & year == max(year, na.rm = TRUE)]
    if (nrow(nsp_data) == 0) return(default_map("No NSP data for selected filters"))
    
    # Detect intervention columns
    int_cols <- grep("^deployed_int_", names(nsp_data), value = TRUE)
    
    if (length(int_cols) == 0) {
      admin_summary <- unique(nsp_data[, .(admin_1, scenario_name)])
    } else {
      # Build intervention combination string
      admin_summary <- unique(nsp_data[, c("admin_1", int_cols), with = FALSE])
      admin_summary[, intervention_combo := apply(.SD, 1, function(row) {
        deployed <- names(.SD)[which(as.logical(row))]
        if (length(deployed) == 0) return("No interventions")
        paste(gsub("^deployed_int_", "", deployed), collapse = " + ")
      }), .SDcols = int_cols]
    }
    
    # Merge with shapefile
    merged <- merge(sf_obj, admin_summary, by.x = "NAME", by.y = "admin_1", all.x = FALSE)
    if (nrow(merged) == 0) return(default_map("No matching administrative units"))
    
    # Color by intervention combo or scenario
    color_var <- if ("intervention_combo" %in% names(merged)) "intervention_combo" else "scenario_name"
    pal <- colorFactor("Set2", domain = merged[[color_var]])
    
    # Create labels
    labels <- sprintf("<strong>%s</strong><br/>%s: %s", merged$NAME,
                      ifelse(color_var == "intervention_combo", "Interventions", "Scenario"),
                      merged[[color_var]]) %>% lapply(HTML)
    
    # Render map
    leaflet(merged) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(get(color_var)), weight = 2, opacity = 1, color = "white",
                  dashArray = "3", fillOpacity = 0.7,
                  highlightOptions = highlightOptions(weight = 3, color = "#666", dashArray = "",
                                                      fillOpacity = 0.9, bringToFront = TRUE),
                  label = labels, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                              textsize = "15px", direction = "auto")) %>%
      addLegend(pal = pal, values = ~get(color_var), title = "NSP Interventions",
                position = "bottomright", opacity = 0.8)
  })
  
  # Data table
  output$data_table <- renderDT({
    dt <- simulation_data()
    if (!is.data.table(dt) || nrow(dt) == 0) return(datatable(data.frame(Message = "No data available")))
    
    datatable(head(dt, 500),
              options = list(pageLength = 25, scrollX = TRUE, scrollY = "500px",
                             dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'),
                             columnDefs = list(list(className = 'dt-center', targets = "_all"))),
              class = 'cell-border stripe hover', rownames = FALSE) %>%
      formatRound(columns = c("prevalenceRate", "incidenceRate", "EIR"), digits = 4)
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() paste0("malaria_simulation_", Sys.Date(), ".csv"),
    content = function(file) fwrite(simulation_data(), file)
  )
}

# Running the app
shinyApp(ui = ui, server = server)