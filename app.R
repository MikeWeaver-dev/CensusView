library(tidyverse)
library(sf)
library(leaflet)
library(shiny)
library(shinythemes)
library(DT)
library(bslib)
library(shinyjs)
library(shinyWidgets)  # Added this — needed for radioGroupButtons

ui <- navbarPage(title = "Mike Weaver App", theme = shinytheme("sandstone"),
                 
                 tags$head(tags$style(HTML("
                   .btn-darkcyan, .btn-darkcyan:focus {
                     background-color:#6bc2c2!important; border-color:#6bc2c2!important; color:#fff!important;
                   }
                   .btn-darkcyan:hover, .btn-darkcyan.active, .open>.dropdown-toggle.btn-darkcyan {
                     background-color:#0bbaba!important; border-color:#0bbaba!important; color:#fff!important;
                   }
                 "))),
                 
                 tabsetPanel(
                   tabPanel("Data Mapping",
                            shinyjs::useShinyjs(),
                            
                            titlePanel(tags$h2("Census Data Mapping",
                                               style = "margin: 10px 5px 20px 15px;")),
                            
                            tags$h4("Select Inputs Below!",
                                    style = "color: DarkCyan; margin: 10px 5px 15px 15px;"),
                            
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
                                
                                tags$head(tags$style(HTML("
                                  .radio-group-buttons .btn.radiobtn {
                                    border-radius: 10px !important;
                                    margin-right: 8px;
                                    margin-bottom: 6px;
                                  }
                                  .radio-group-buttons .btn.radiobtn:last-child { margin-right: 0; }
                                "))),
                                
                                p("", style = "margin-bottom: 8px;"),
                                
                                selectInput("Geography", "Choose a Geography",
                                            choices = c("State", "County", "Tract"),
                                            selected = "State"),
                                
                                conditionalPanel(
                                  condition = "input.Geography == 'Tract'",
                                  selectInput("State", "Choose a State to Analyze",
                                              choices = Statenames, selected = "Alabama")
                                ),
                                
                                conditionalPanel(
                                  condition = "input.Geography == 'County'",
                                  div(style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 5px; margin-bottom: 15px;",
                                      tags$strong(style = "color: #856404;", "Note: "),
                                      tags$span(style = "color: #856404;", "County dataset is large and loads slowly. On the server it may break due to low RAM. please be patient.")
                                  )
                                ),
                                
                                conditionalPanel(
                                  condition = "input.Analysismode == 'point'",
                                  selectInput("Year2", "Choose a Year to Analyze",
                                              choices = Yearsavailable, selected = "2023")
                                ),
                                
                                conditionalPanel(
                                  condition = "input.Analysismode == 'change'",
                                  
                                  selectInput("Year1", "Choose Start Year for Analysis",
                                              choices = Yearsavailable, selected = "2020"),
                                  selectInput("Year2", "Choose End Year for Analysis",
                                              choices = Yearsavailable, selected = "2023")
                                ),
                                
                                
                                conditionalPanel(
                                  condition = "input.Year1 == input.Year2",
                                  div(style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 5px; margin-bottom: 15px;",
                                      tags$strong(style = "color: #856404;", "Note: "),
                                      tags$span(style = "color: #856404;", "Please select two different years below")
                                  )
                                ),
                  
                                  
                                selectInput("Variable", "Choose a Variable to Analyze",
                                            choices = Variablename, selected = "Population"),
                                
                                p("", style = "margin-bottom: 35px;"),
                                
                                actionButton("Button", "Generate Map"),
                                
                                p("", style = "margin-bottom: 5px;"),
                                shinyjs::hidden(div(id = "loading_note", class = "text-warning", "Loading…"))
                              ),
                              
                              mainPanel(
                                div(class = "panel panel-default",
                                    div(class = "panel-heading", "Interactive Map"),
                                    div(class = "panel-body",
                                        leafletOutput("Map", height = 400),
                                        verbatimTextOutput("clicked")
                                    )
                                ),
                                
                                div(class = "panel panel-primary",
                                    div(class = "panel-heading", "Data Table"),
                                    div(class = "panel-body",
                                        fluidPage(DTOutput('Tbl'))
                                    )
                                )
                              )
                            )
                   ),
                   
                   tabPanel(
                     "Other Apps",
                     
                     tags$head(tags$style(HTML("
                       .panel { border-radius:8px; box-shadow:0 2px 6px rgba(0,0,0,.06); border-width:1px; margin-bottom:20px; }
                       .panel .panel-body { min-height: 120px; }
                       .panel-default { border-color:#a39c96; }
                       .panel-default > .panel-heading { background:#a39c96; border-color:#a39c96; color:#FFFFFF; font-weight:600; }
                       .panel-default > .panel-body { background:#F5F1EC; color:#3F3A37; }
                       .panel-default > .panel-heading a,
                       .panel-default > .panel-heading a:hover,
                       .panel-default > .panel-heading a:focus { color:#FFFFFF; text-decoration:none; }
                       .panel-primary { border-color:#008B8B; }
                       .panel-primary > .panel-heading { background:#008B8B; border-color:#008B8B; color:#FFFFFF; font-weight:600; }
                       .panel-primary > .panel-body { background:#E6F7F7; color:#0A3F3F; }
                       .panel-primary > .panel-heading a,
                       .panel-primary > .panel-heading a:hover,
                       .panel-primary > .panel-heading a:focus { color:#FFFFFF; text-decoration:none; }
                       .panel-info { border-color:#20C997; }
                       .panel-info > .panel-heading { background:#20C997; border-color:#20C997; color:#FFFFFF; font-weight:600; }
                       .panel-info > .panel-body { background:#E9FBF6; color:#114C45; }
                       .panel-info > .panel-heading a,
                       .panel-info > .panel-heading a:hover,
                       .panel-info > .panel-heading a:focus { color:#FFFFFF; text-decoration:none; }
                     "))),
                     
                     titlePanel(
                       tags$h2("Portfolio of Web Applications",
                               style = "color: DarkCyan; margin: 10px 5px 20px 15px;")
                     ),
                     
                     fluidRow(
                       column(width = 6, class = "col-md-4",
                              div(class = "panel panel-primary",
                                  div(class = "panel-heading",
                                      tags$a(href = "https://Mikeweaver.dev", "Personal Portfolio")),
                                  div(class = "panel-body",
                                      "This link takes you directly to my portfolio - a website for all my coding projects and qualifications. From there you can access my web apps, source code, resume, and more.")
                              )
                       ),
                       
                       column(width = 6, class = "col-md-4",
                              div(class = "panel panel-info",
                                  div(class = "panel-heading",
                                      tags$a(href = "https://voyage.Mikeweaver.dev/", "Voyage")),
                                  div(class = "panel-body",
                                      "This web app is a working and scalable social media platform where users can post about their travels and experiences.")
                              )
                       ),
                       
                       div(class = "clearfix visible-sm-block"),
                       
                       column(width = 6, class = "col-md-4",
                              div(class = "panel panel-info",
                                  div(class = "panel-heading",
                                      tags$a(href = "https://aichef.mikeweaver.dev/", "AI Chef")),
                                  div(class = "panel-body",
                                      "AI Chef is for web and mobile (including iOS via Expo). The app integrates AI within a compelling UI/UX to inspire meals based on food users have on-hand.")
                              )
                       ),
                       
                       column(width = 6, class = "col-md-4",
                              div(class = "panel panel-info",
                                  div(class = "panel-heading",
                                      tags$a(href = "https://spotifylab.mikeweaver.dev/", "SpotifyLab")),
                                  div(class = "panel-body",
                                      "Optimized for desktop and mobile and available on the ios and Android app store, this app leverages artificial intelligence to generate bespoke playlists based on user inputs and Spotify listening history.")
                              )
                       )
                     )
                   ),
                   
                   tabPanel("About",
                            titlePanel(
                              tags$h2("Note from Developer",
                                      style = "color: DarkCyan; margin: 10px 5px 20px 15px;"
                              )),
                            
                            div(class = "panel panel-default",
                                div(class = "panel-heading", "About this Web App"),
                                div(class = "panel-body",
                                    h5("This is the first app I built for my software development portfolio. I like it because it utilizes the Census API and millions of data points, but appears very simple from the user perspective.", style = "margin: 10px 5px 20px 0px;"),
                                    h5("This app reminds me a lot of the coding work I did at my last job. We routinely worked with demographic data and GIS systems to synthesize complex analysis into clear and readable maps. I often used R for my workflow so I felt it a fitting starting point, though in the future I hope to publish projects leverage Python, Javascript, and AI.", style = "margin: 10px 5px 20px 0px;"),
                                    h5("Thanks for visiting my webpage! See more of my apps on the 'Other Apps' Tab", style = "margin: 10px 5px 20px 0px;"),
                                    h5("Mike Weaver", style = "margin: 0px 5px 20px 0px;")
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
  
  Mapdata <- eventReactive(input$Button, {
    req(input$Year2)
    nm <- paste0(input$Geography, input$Year2)
    d <- get(nm, inherits = TRUE)
    
    if (input$Geography == "Tract" && "Statenames" %in% names(d)) {
      d <- d[d$Statenames == input$State, , drop = FALSE]
    }
    d
  })
  
  observeEvent(Mapdata(), {
    d <- Mapdata()
    req(d, nrow(d) > 0)
    
    key <- "GEOID"
    nm_col <- as.character(d[["NAME"]])
    y2 <- suppressWarnings(as.numeric(d[[input$Variable]]))
    v <- y2
    
    pre <- if (input$Variable %in% c("Median Household Income","Median Home Value",
                                     "Median Gross Rent","Income Reached by Top 5% of Earners")) "$" else ""
    post <- if (input$Variable %in% c("Percent with Bachelor Degrees")) "%" else ""
    legend_title <- input$Variable
    
    if (input$Analysismode == "change") {
      validate(
        need(input$Year1 != input$Year2,
             "Please select two different years for Change-over-Time analysis.")
      )
      
      nm1 <- paste0(input$Geography, input$Year1)
      validate(need(exists(nm1), paste("Data for year", input$Year1, "not available.")))
      
      d1 <- get(nm1, inherits = TRUE)
      if (input$Geography == "Tract" && "Statenames" %in% names(d1)) {
        d1 <- d1[d1$Statenames == input$State, , drop = FALSE]
      }
      
      y1 <- suppressWarnings(as.numeric(d1[[input$Variable]][match(d[[key]], d1[[key]])]))
      
      v <- ifelse(is.na(y1) | is.na(y2) | y1 == 0,
                  NA_real_,
                  100 * (y2 - y1) / abs(y1))
      
      pre <- ""
      post <- "%"
      legend_title <- paste0(input$Variable, " (", input$Year1, "→", input$Year2, ", % change)")
    }
    
    mask <- is.finite(v)
    dom <- v[mask]
    validate(need(length(dom) > 0, "No valid data to display for the selected variable."))
    
    if (input$Geography %in% c("State", "County")) {
      leafletProxy("Map") %>%
        flyToBounds(lng1 = -125, lat1 = 24, lng2 = -66, lat2 = 50)
    } else {
      d_zoom <- d[mask & !st_is_empty(d), , drop = FALSE]
      if (nrow(d_zoom) > 0) {
        if (is.na(st_crs(d_zoom)) || st_crs(d_zoom)$epsg != 4326) {
          d_zoom <- st_transform(d_zoom, 4326)
        }
        bb <- st_bbox(d_zoom)
        leafletProxy("Map") %>%
          fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      }
    }
    
    pal <- colorQuantile(c("#f1f1f1", "#02b3b3"), domain = dom, n = 5, na.color = "transparent")
    
    lab_fmt <- if (input$Analysismode == "change") {
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
    
    leafletProxy("Map") %>%
      clearGroup("Mapgroup") %>%
      removeControl("legend") %>%
      addPolygons(
        data = d,
        group = "Mapgroup",
        fillColor = pal(v),
        color = "#999999",
        weight = 0.25,
        opacity = 0.4,
        fillOpacity = 0.5,
        label = paste0(nm_col, " — ", legend_title, ": ", pre,
                       formatC(v, format = "f", digits = if (input$Analysismode == "change") 1 else 0, big.mark = ","),
                       post),
        highlightOptions = highlightOptions(weight = 2, color = "#444", bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = dom,
                title = legend_title, opacity = 0.6,
                layerId = "legend", labFormat = lab_fmt)
  })
  
  observeEvent(input$map_drawn, {
    shinyjs::hide("loading_note")
  })
  
  # FIXED: Changed to eventReactive tied to Button click instead of inputs
  Tabledata <- eventReactive(input$Button, {
    req(input$Year2)
    
    if (input$Analysismode == "point") {
      nm <- paste0(input$Geography, input$Year2)
      d <- get(nm, inherits = TRUE)
      if (input$Geography == "Tract" && "Statenames" %in% names(d)) {
        d <- d[d$Statenames == input$State, , drop = FALSE]
      }
      df <- as.data.frame(d) %>% select(-any_of("geometry"))
      wanted <- unique(c("NAME", "Population", "Median Household Income", input$Variable))
      df <- select(df, any_of(wanted))
      return(df)
    }
    
    validate(
      need(input$Year1 != input$Year2, "Start and End years must be different."),
      need(exists(paste0(input$Geography, input$Year1)), paste("Data for", input$Year1, "not found."))
    )
    
    nm1 <- paste0(input$Geography, input$Year1)
    nm2 <- paste0(input$Geography, input$Year2)
    d1 <- get(nm1, inherits = TRUE)
    d2 <- get(nm2, inherits = TRUE)
    
    if (input$Geography == "Tract") {
      if ("Statenames" %in% names(d1)) d1 <- d1[d1$Statenames == input$State, , drop = FALSE]
      if ("Statenames" %in% names(d2)) d2 <- d2[d2$Statenames == input$State, , drop = FALSE]
    }
    
    df1 <- as.data.frame(d1) %>% select(-any_of("geometry"))
    df2 <- as.data.frame(d2) %>% select(-any_of("geometry"))
    
    var_sym <- sym(input$Variable)
    col_y1 <- paste0(input$Year1, " ", input$Variable)
    col_y2 <- paste0(input$Year2, " ", input$Variable)
    change_col <- paste0("Change over Time (", input$Year1, "→", input$Year2, ")")
    
    y1 <- df1 %>% select(GEOID, NAME, !!var_sym) %>% rename(y1_raw = !!var_sym)
    y2 <- df2 %>% select(GEOID, !!var_sym) %>% rename(y2_raw = !!var_sym)
    
    df <- y1 %>%
      left_join(y2, by = "GEOID") %>%
      mutate(
        !!col_y1 := y1_raw,
        !!col_y2 := y2_raw,
        !!change_col := if_else(!is.na(y1_raw) & y1_raw != 0,
                                (y2_raw - y1_raw) / abs(y1_raw),
                                NA_real_)
      ) %>%
      select(NAME, !!sym(col_y1), !!sym(col_y2), !!sym(change_col))
    
    df
  })
  
  output$Tbl <- DT::renderDT({
    df <- Tabledata()
    req(df, nrow(df) > 0)
    
    is_change <- isolate(input$Analysismode == "change")
    var_name <- isolate(input$Variable)
    
    dollars <- c("Median Household Income","Median Home Value","Median Gross Rent","Income Reached by Top 5% of Earners")
    percents <- c("Percent with Bachelor Degrees")
    
    w <- datatable(df, filter = "top", rownames = FALSE,
                   options = list(pageLength = 10))
    
    # FIXED: Identify all columns in the dataframe to format
    all_cols <- names(df)
    
    if (is_change) {
      year1 <- isolate(input$Year1)
      year2 <- isolate(input$Year2)
      ycols <- c(paste0(year1, " ", var_name), paste0(year2, " ", var_name))
      change_col <- paste0("Change over Time (", year1, "→", year2, ")")
      
      # Format each column based on what it contains
      for (col in all_cols) {
        if (col == "NAME") next  # Skip name column
        
        # Check if this column contains population data
        if (grepl("Population", col, ignore.case = TRUE)) {
          w <- formatRound(w, col, digits = 0, mark = ",")
        }
        # Check if this column contains median household income
        else if (grepl("Median Household Income", col, ignore.case = TRUE)) {
          w <- formatCurrency(w, col, currency = "$", digits = 0, mark = ",")
        }
        # Check if this column contains other dollar values
        else if (any(sapply(dollars, function(d) grepl(d, col, fixed = TRUE)))) {
          w <- formatCurrency(w, col, currency = "$", digits = 0, mark = ",")
        }
        # Check if this column contains percent data (but not change over time)
        else if (any(sapply(percents, function(p) grepl(p, col, fixed = TRUE))) && !grepl("Change over Time", col)) {
          w <- formatString(w, col, suffix = "%") %>%
            formatRound(col, digits = 1, mark = ",")
        }
        # Check if this is the change over time column
        else if (col == change_col) {
          w <- formatPercentage(w, col, digits = 1)
        }
        # Default: regular number formatting
        else {
          w <- formatRound(w, col, digits = 0, mark = ",")
        }
      }
    } else {
      # Point-in-time formatting
      for (col in all_cols) {
        if (col == "NAME") next  # Skip name column
        
        # Check if this column is Population
        if (col == "Population") {
          w <- formatRound(w, col, digits = 0, mark = ",")
        }
        # Check if this column is Median Household Income
        else if (col == "Median Household Income") {
          w <- formatCurrency(w, col, currency = "$", digits = 0, mark = ",")
        }
        # Check if this column is a dollar variable
        else if (col %in% dollars) {
          w <- formatCurrency(w, col, currency = "$", digits = 0, mark = ",")
        }
        # Check if this column is a percent variable
        else if (col %in% percents) {
          w <- formatString(w, col, suffix = "%") %>%
            formatRound(col, digits = 1, mark = ",")
        }
        # Default: regular number formatting
        else {
          w <- formatRound(w, col, digits = 0, mark = ",")
        }
      }
    }
    
    w
  })
}

shinyApp(ui = ui, server = server)