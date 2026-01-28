library(tidyverse)
library(sf)
library(leaflet)
library(shiny)
library(shinythemes)
library(DT)
library(bslib)
library(shinyjs)
library(shinyWidgets)

# loading seperately because of GitHub, which requires data in smaller batches
load("StateData.RData")
County2020 <- readRDS("County2020.rds")
County2021 <- readRDS("County2021.rds")
County2022 <- readRDS("County2022.rds")
County2023 <- readRDS("County2023.rds")

# DON'T load tract data at startup anymore
# Tract data will be loaded dynamically based on user selection
# This helps with speed

ui <- navbarPage(
  title = "Mike Weaver App",
  theme = shinytheme("sandstone"),
  
  # Link to external CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  tabsetPanel(
    tabPanel(
      "Data Mapping",
      shinyjs::useShinyjs(),
      
      titlePanel(
        tags$h2("Census Data Mapping", style = "margin: 10px 5px 20px 15px;")
      ),
      
      tags$h4(
        "Select Inputs Below!",
        style = "color: DarkCyan; margin: 10px 5px 15px 15px;"
      ),
      
      sidebarLayout(
        sidebarPanel(
          p("", style = "margin-bottom: -10px;"),
          h4("Choose Variable & Geography to Analyze"),
          p("", style = "margin-bottom: 25px;"),
          
          shinyWidgets::radioGroupButtons(
            inputId = "Analysismode",
            label = "Select Type of Analysis",
            choices = c("Point-in-Time" = "point", "Change-over-Time" = "change"),
            selected = "point",
            individual = TRUE,
            size = "sm",
            status = "darkcyan",
            checkIcon = list(yes = icon("check"))
          ),
          
          p("", style = "margin-bottom: 8px;"),
          
          selectInput(
            "Geography",
            "Choose a Geography",
            choices = c("State", "County", "Tract"),
            selected = "State"
          ),
          
          conditionalPanel(
            condition = "input.Geography == 'Tract' || input.Geography == 'County'",
            selectInput(
              "State",
              "Choose a State to Analyze",
              choices = Statenames,
              selected = "Alabama"
            )
          ),
          
          conditionalPanel(
            condition = "input.Analysismode == 'point'",
            selectInput(
              "Year2",
              "Choose a Year to Analyze",
              choices = Yearsavailable,
              selected = "2023"
            )
          ),
          
          conditionalPanel(
            condition = "input.Analysismode == 'change'",
            selectInput(
              "Year1",
              "Choose Start Year for Analysis",
              choices = Yearsavailable,
              selected = "2020"
            ),
            selectInput(
              "Year2",
              "Choose End Year for Analysis",
              choices = Yearsavailable,
              selected = "2023"
            )
          ),
          
          conditionalPanel(
            condition = "input.Year1 == input.Year2 && input.Analysismode == 'change'",
            div(
              style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 5px; margin-bottom: 15px;",
              tags$strong(style = "color: #856404;", "Note: "),
              tags$span(style = "color: #856404;", "Please select two different years below")
            )
          ),
          
          selectInput(
            "Variable",
            "Choose a Variable to Analyze",
            choices = Variablename,
            selected = "Population"
          ),
          
          p("", style = "margin-bottom: 35px;"),
          
          actionButton("Button", "Generate Map"),
          
          p("", style = "margin-bottom: 5px;"),
          shinyjs::hidden(
            div(id = "loading_note", class = "text-warning", "Loading…")
          )
        ),
        
        mainPanel(
          div(
            class = "panel panel-default",
            div(class = "panel-heading", "Interactive Map"),
            div(
              class = "panel-body",
              leafletOutput("Map", height = 400),
              verbatimTextOutput("clicked")
            )
          ),
          
          div(
            class = "panel panel-primary",
            div(class = "panel-heading", "Data Table"),
            div(
              class = "panel-body",
              fluidPage(DTOutput('Tbl'))
            )
          )
        )
      )
    ),
    
    tabPanel(
      "Other Apps",
      
      titlePanel(
        tags$h2(
          "Portfolio of Web Applications",
          style = "color: DarkCyan; margin: 10px 5px 20px 15px;"
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          class = "col-md-4",
          div(
            class = "panel panel-primary",
            div(
              class = "panel-heading",
              tags$a(href = "https://Mikeweaver.dev", "Personal Portfolio")
            ),
            div(
              class = "panel-body",
              "This link takes you directly to my portfolio - a website for all my coding projects and qualifications. From there you can access my web apps, source code, resume, and more."
            )
          )
        ),
        
        column(
          width = 6,
          class = "col-md-4",
          div(
            class = "panel panel-info",
            div(
              class = "panel-heading",
              tags$a(href = "https://voyage.Mikeweaver.dev/", "Voyage")
            ),
            div(
              class = "panel-body",
              "This web app is a working and scalable social media platform where users can post about their travels and experiences."
            )
          )
        ),
        
        div(class = "clearfix visible-sm-block"),
        
        column(
          width = 6,
          class = "col-md-4",
          div(
            class = "panel panel-info",
            div(
              class = "panel-heading",
              tags$a(href = "https://aichef.mikeweaver.dev/", "AI Chef")
            ),
            div(
              class = "panel-body",
              "AI Chef is for web and mobile (including iOS via Expo). The app integrates AI within a compelling UI/UX to inspire meals based on food users have on-hand."
            )
          )
        ),
        
        column(
          width = 6,
          class = "col-md-4",
          div(
            class = "panel panel-info",
            div(
              class = "panel-heading",
              tags$a(href = "https://mikeweaver.dev/spotifylab", "SpotifyLab")
            ),
            div(
              class = "panel-body",
              "Optimized for desktop and mobile and available on the ios and Android app store, this app leverages artificial intelligence to generate bespoke playlists based on user inputs and Spotify listening history."
            )
          )
        )
      )
    ),
    
    tabPanel(
      "About",
      
      titlePanel(
        tags$h2(
          "Note from Developer",
          style = "color: DarkCyan; margin: 10px 5px 20px 15px;"
        )
      ),
      
      div(
        class = "panel panel-default",
        div(class = "panel-heading", "About this Web App"),
        div(
          class = "panel-body",
          h5(
            "This is the first app I built for my software development portfolio. I like it because it utilizes the Census API and millions of data points, but appears very simple from the user perspective.",
            style = "margin: 10px 5px 20px 0px;"
          ),
          h5(
            "This app reminds me a lot of the coding work I did at my last job. We routinely worked with demographic data and GIS systems to synthesize complex analysis into clear and readable maps. I often used R for my workflow so I felt it a fitting starting point, though in the future I hope to publish projects leverage Python, Javascript, and AI.",
            style = "margin: 10px 5px 20px 0px;"
          ),
          h5(
            "Thanks for visiting my webpage! See more of my apps on the 'Other Apps' Tab",
            style = "margin: 10px 5px 20px 0px;"
          ),
          h5(
            "Mike Weaver",
            style = "margin: 0px 5px 20px 0px;"
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$Button, show("loading_note"))
  
  output$Map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          map.on('layeradd', function(e){
            if (e.layer && (e.layer instanceof L.Path)) {
              Shiny.setInputValue('map_drawn', Date.now());
            }
          });
        }
      ")
  })
  
  # Helper function to load tract data dynamically
  load_tract_data <- function(state, year) {
    
    # Replace spaces with underscores in state name
    state_formatted <- gsub(" ", "_", state)
    
    # Construct file name
    filename <- paste0("Tract", year, "_", state_formatted, ".rds")
    
    # Check if file exists. Should always exist
    if (!file.exists(filename)) {
      stop(paste("File not found:", filename))
    }
    
    # Load and return the data
    readRDS(filename)
  }
  
  selected_map_data <- eventReactive(input$Button, {
    req(input$Year2)
    
    # For Tract geography, load dynamically
    if (input$Geography == "Tract") {
      req(input$State)
      geo_data <- load_tract_data(input$State, input$Year2)
    } else {
      # For State and County, use existing logic
      dataset_name <- paste0(input$Geography, input$Year2)
      geo_data <- get(dataset_name, inherits = TRUE)
      
      # for county we are going to filter for just the state selected
      if (input$Geography == "County" && "Statenames" %in% names(geo_data)) {
        geo_data <- geo_data[geo_data$Statenames == input$State, , drop = FALSE]
      }
    }
    
    geo_data
  })
  
  observeEvent(selected_map_data(), {
    geo_data <- selected_map_data()
    req(geo_data, nrow(geo_data) > 0)
    
    geoid_key <- "GEOID"
    location_names <- as.character(geo_data[["NAME"]])
    current_year_values <- suppressWarnings(as.numeric(geo_data[[input$Variable]]))
    display_values <- current_year_values
    
    # when we show the data we may need to show a prefix of $ or suffix of % depending on the variable
    value_prefix <- if (input$Variable %in% c("Median Household Income", "Median Home Value",
                                              "Median Gross Rent", "Income Reached by Top 5% of Earners")) "$" else ""
    value_suffix <- if (input$Variable %in% c("Percent with Bachelor Degrees")) "%" else ""
    legend_title <- input$Variable
    
    if (input$Analysismode == "change") {
      validate(
        need(input$Year1 != input$Year2,
             "Please select two different years for Change-over-Time analysis.")
      )
      
      # For Tract geography, load Year1 data dynamically
      # we load tract data after the button is pushed to avoid loading in a tremendous amount of data (every tract every year) upon first load
      if (input$Geography == "Tract") {
        req(input$State)
        start_year_data <- load_tract_data(input$State, input$Year1)
      } else {
        start_dataset_name <- paste0(input$Geography, input$Year1)
        validate(need(exists(start_dataset_name), paste("Data for year", input$Year1, "not available.")))
        
        start_year_data <- get(start_dataset_name, inherits = TRUE)
        
        if (input$Geography == "County" && "Statenames" %in% names(start_year_data)) {
          start_year_data <- start_year_data[start_year_data$Statenames == input$State, , drop = FALSE]
        }
      }
      
      start_year_values <- suppressWarnings(as.numeric(start_year_data[[input$Variable]][match(geo_data[[geoid_key]], start_year_data[[geoid_key]])]))
      
      display_values <- ifelse(is.na(start_year_values) | is.na(current_year_values) | start_year_values == 0,
                               NA_real_,
                               100 * (current_year_values - start_year_values) / abs(start_year_values))
      
      value_prefix <- ""
      value_suffix <- "%"
      legend_title <- paste0(input$Variable, " (", input$Year1, "→", input$Year2, ", % change)")
    }
    
    valid_data_mask <- is.finite(display_values)
    valid_values_domain <- display_values[valid_data_mask]
    validate(need(length(valid_values_domain) > 0, "No valid data to display for the selected variable."))
    
    if (input$Geography %in% c("State")) {
      leafletProxy("Map") %>%
        flyToBounds(lng1 = -125, lat1 = 24, lng2 = -66, lat2 = 50)
    } else {
      zoom_data <- geo_data[valid_data_mask & !st_is_empty(geo_data), , drop = FALSE]
      if (nrow(zoom_data) > 0) {
        if (is.na(st_crs(zoom_data)) || st_crs(zoom_data)$epsg != 4326) {
          zoom_data <- st_transform(zoom_data, 4326)
        }
        bounding_box <- st_bbox(zoom_data)
        leafletProxy("Map") %>%
          fitBounds(bounding_box[["xmin"]], bounding_box[["ymin"]], bounding_box[["xmax"]], bounding_box[["ymax"]])
      }
    }
    
    color_palette <- colorQuantile(c("#f1f1f1", "#02b3b3"), domain = valid_values_domain, n = 5, na.color = "transparent")
    
    legend_label_formatter <- if (input$Analysismode == "change") {
      function(type, cuts, p) {
        paste0(
          ifelse(cuts[-length(cuts)] >= 0, "+", ""),
          formatC(cuts[-length(cuts)], format = "f", digits = 1), "% to ",
          ifelse(cuts[-1] >= 0, "+", ""),
          formatC(cuts[-1], format = "f", digits = 1), "%"
        )
      }
    } else {
      function(type, cuts, p) {
        c("Bottom 20th Percentile", "20–40th Percentile", "40–60th Percentile",
          "60–80th Percentile", "80th+ Percentile")
      }
    }
    
    # This is the actual mapping piece. the stuff before just cleaned the data and defined our bins and stiff
    leafletProxy("Map") %>%
      clearGroup("Mapgroup") %>%
      removeControl("legend") %>%
      addPolygons(
        data = geo_data,
        group = "Mapgroup",
        fillColor = color_palette(display_values),
        color = "#999999",
        weight = 0.25,
        opacity = 0.4,
        fillOpacity = 0.5,
        label = paste0(location_names, " — ", legend_title, ": ", value_prefix,
                       formatC(display_values, format = "f", digits = if (input$Analysismode == "change") 1 else 0, big.mark = ","),
                       value_suffix),
        highlightOptions = highlightOptions(weight = 2, color = "#444", bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = color_palette, values = valid_values_domain,
                title = legend_title, opacity = 0.6,
                layerId = "legend", labFormat = legend_label_formatter)
  })
  
  observeEvent(input$map_drawn, {
    shinyjs::hide("loading_note")
  })
  
  # now on to the data table
  selected_table_data <- eventReactive(input$Button, {
    req(input$Year2)
    
    if (input$Analysismode == "point") {
      # For Tract geography, load dynamically
      if (input$Geography == "Tract") {
        req(input$State)
        geo_data <- load_tract_data(input$State, input$Year2)
      } else {
        dataset_name <- paste0(input$Geography, input$Year2)
        geo_data <- get(dataset_name, inherits = TRUE)
        
        if (input$Geography == "County" && "Statenames" %in% names(geo_data)) {
          geo_data <- geo_data[geo_data$Statenames == input$State, , drop = FALSE]
        }
      }
      
      table_df <- as.data.frame(geo_data) %>% select(-any_of("geometry"))
      # Show these variables and the one they chose. If they chose one of these drop it with unique function
      desired_columns <- unique(c("NAME", "Population", "Median Household Income", input$Variable))
      table_df <- select(table_df, any_of(desired_columns))
      return(table_df)
    }
    
    validate(
      need(input$Year1 != input$Year2, "Start and End years must be different.")
    )
    
    # For Tract geography, load both years dynamically
    if (input$Geography == "Tract") {
      req(input$State)
      start_year_data <- load_tract_data(input$State, input$Year1)
      end_year_data <- load_tract_data(input$State, input$Year2)
    } else {
      start_dataset_name <- paste0(input$Geography, input$Year1)
      end_dataset_name <- paste0(input$Geography, input$Year2)
      start_year_data <- get(start_dataset_name, inherits = TRUE)
      end_year_data <- get(end_dataset_name, inherits = TRUE)
      
      if (input$Geography == "County") {
        if ("Statenames" %in% names(start_year_data)) start_year_data <- start_year_data[start_year_data$Statenames == input$State, , drop = FALSE]
        if ("Statenames" %in% names(end_year_data)) end_year_data <- end_year_data[end_year_data$Statenames == input$State, , drop = FALSE]
      }
    }
    
    start_year_df <- as.data.frame(start_year_data) %>% select(-any_of("geometry"))
    end_year_df <- as.data.frame(end_year_data) %>% select(-any_of("geometry"))
    
    variable_symbol <- sym(input$Variable)
    start_year_column_name <- paste0(input$Year1, " ", input$Variable)
    end_year_column_name <- paste0(input$Year2, " ", input$Variable)
    change_column_name <- paste0("Change over Time (", input$Year1, "→", input$Year2, ")")
    
    start_year_subset <- start_year_df %>% select(GEOID, NAME, !!variable_symbol) %>% rename(start_year_raw = !!variable_symbol)
    end_year_subset <- end_year_df %>% select(GEOID, !!variable_symbol) %>% rename(end_year_raw = !!variable_symbol)
    
    comparison_df <- start_year_subset %>%
      left_join(end_year_subset, by = "GEOID") %>%
      mutate(
        !!start_year_column_name := start_year_raw,
        !!end_year_column_name := end_year_raw,
        !!change_column_name := if_else(!is.na(start_year_raw) & start_year_raw != 0,
                                        (end_year_raw - start_year_raw) / abs(start_year_raw),
                                        NA_real_)
      ) %>%
      select(NAME, !!sym(start_year_column_name), !!sym(end_year_column_name), !!sym(change_column_name))
    
    comparison_df
  })
  
  # Here is where we load the table. The stuff before was just identifying what needed to load and be shown
  output$Tbl <- DT::renderDT({
    table_df <- selected_table_data()
    req(table_df, nrow(table_df) > 0)
    
    is_change_analysis <- isolate(input$Analysismode == "change")
    selected_variable_name <- isolate(input$Variable)
    
    dollar_variables <- c("Median Household Income", "Median Home Value", "Median Gross Rent", "Income Reached by Top 5% of Earners")
    percent_variables <- c("Percent with Bachelor Degrees")
    
    data_widget <- datatable(table_df, filter = "top", rownames = FALSE,
                             options = list(
                               pageLength = 10,
                               scrollX = TRUE,
                               autoWidth = FALSE
                             ))
    
    all_column_names <- names(table_df)
    
    if (is_change_analysis) {
      start_year <- isolate(input$Year1)
      end_year <- isolate(input$Year2)
      year_columns <- c(paste0(start_year, " ", selected_variable_name), paste0(end_year, " ", selected_variable_name))
      change_column <- paste0("Change over Time (", start_year, "→", end_year, ")")
      
      for (column_name in all_column_names) {
        if (column_name == "NAME") next
        
        if (grepl("Population", column_name, ignore.case = TRUE)) {
          data_widget <- formatRound(data_widget, column_name, digits = 0, mark = ",")
        }
        else if (grepl("Median Household Income", column_name, ignore.case = TRUE)) {
          data_widget <- formatCurrency(data_widget, column_name, currency = "$", digits = 0, mark = ",")
        }
        else if (any(sapply(dollar_variables, function(dollar_var) grepl(dollar_var, column_name, fixed = TRUE)))) {
          data_widget <- formatCurrency(data_widget, column_name, currency = "$", digits = 0, mark = ",")
        }
        else if (any(sapply(percent_variables, function(percent_var) grepl(percent_var, column_name, fixed = TRUE))) && !grepl("Change over Time", column_name)) {
          data_widget <- formatString(data_widget, column_name, suffix = "%") %>%
            formatRound(column_name, digits = 1, mark = ",")
        }
        else if (column_name == change_column) {
          data_widget <- formatPercentage(data_widget, column_name, digits = 1)
        }
        else {
          data_widget <- formatRound(data_widget, column_name, digits = 0, mark = ",")
        }
      }
    } else {
      for (column_name in all_column_names) {
        if (column_name == "NAME") next
        
        if (column_name == "Population") {
          data_widget <- formatRound(data_widget, column_name, digits = 0, mark = ",")
        }
        else if (column_name == "Median Household Income") {
          data_widget <- formatCurrency(data_widget, column_name, currency = "$", digits = 0, mark = ",")
        }
        else if (column_name %in% dollar_variables) {
          data_widget <- formatCurrency(data_widget, column_name, currency = "$", digits = 0, mark = ",")
        }
        else if (column_name %in% percent_variables) {
          data_widget <- formatString(data_widget, column_name, suffix = "%") %>%
            formatRound(column_name, digits = 1, mark = ",")
        }
        else {
          data_widget <- formatRound(data_widget, column_name, digits = 0, mark = ",")
        }
      }
    }
    
    data_widget
  })
}

shinyApp(ui = ui, server = server)