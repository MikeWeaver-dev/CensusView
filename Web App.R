library (tidyverse)
library (sf)
library (leaflet)
library (shiny)
library (shinythemes)
library (DT)
library (bslib)
library(shinyjs)

#shinyapp UI
ui <- navbarPage(title = "Mike Weaver App",  theme = shinytheme("sandstone"),
            
            #This is HTML style code written by ChatGPT       
            tags$head(tags$style(HTML("
             .btn-darkcyan, .btn-darkcyan:focus {
             background-color:#6bc2c2!important; border-color:#6bc2c2!important; color:#fff!important;
             }
             .btn-darkcyan:hover, .btn-darkcyan.active, .open>.dropdown-toggle.btn-darkcyan {
             background-color:#0bbaba!important; border-color:#0bbaba!important; color:#fff!important;
             }
            "))),     
                 
             #make several tabs, the first one is the main one    
             tabsetPanel( 
              tabPanel("Data Mapping",
                       
                       #this line is for the 'loading' thing when the button clicks
                       shinyjs::useShinyjs(),
                
                       titlePanel(
                         tags$h2("Census Data Mapping",
                           style = "margin: 10px 5px 20px 15px;"
                         )),
                         
                         tags$h4("Select Inputs Below!",
                                 style = "color: DarkCyan; margin: 10px 5px 15px 15px;"
                         ),
                         
                sidebarLayout(
                  sidebarPanel(
                   
                    p("", style = "margin-bottom: -10px;"),
                    
                    h4("Choose Variable & Geography to Analyze"),
                    
                    p("", style = "margin-bottom: 25px;"),
                    
                    #these buttons switch between Point in time and Change over Time.
                    shinyWidgets::radioGroupButtons(
                      inputId  = "Analysismode",
                      label    = "Select Type of Analysis",
                      choices  = c("Point-in-Time" = "point", "Change-over-Time" = "change"),
                      selected = "point",
                      individual = TRUE,        # <-- separate buttons (not touching)
                      size = "sm",
                      status = "darkcyan",   # color
                      checkIcon = list(yes = icon("check"))
                    ),
                    
                     #HTML code from GPT helps with the formatting of the button
                     tags$head(tags$style(HTML("
                        /* spacing + rounded corners for shinyWidgets radioGroupButtons */
                        .radio-group-buttons .btn.radiobtn {
                        border-radius: 10px !important;   /* slightly rounded */
                        margin-right: 8px;                /* slight gap between buttons */
                        margin-bottom: 6px;                /* slight gap between buttons */
                        }
                        .radio-group-buttons .btn.radiobtn:last-child { margin-right: 0; }
                        "))),
                    
                     p("", style = "margin-bottom: 8px;"),
      
                     selectInput(inputId ="Geography",
                                label = "Choose a Geography",
                                choices = c("State", "County", "Tract"),
                                selected = "State"
                    ),
                    
                    # Only show this panel if the plot type is a histogram
                    conditionalPanel(
                      condition = "input.Geography == 'Tract'",
                      selectInput(inputId ="State",
                                  label = "Choose a State to Analyze",
                                  choices = Statenames,
                                  selected = "Alabama"
                      )),
                    
                    conditionalPanel(
                      condition = "input.Analysismode == 'point'",
                      selectInput(inputId ="Year2",
                                  label = "Choose a Year to Analyze",
                                  choices = Yearsavailable,
                                  selected = "2023"
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "input.Analysismode == 'change'",
                      selectInput(inputId ="Year1",
                                  label = "Choose Start Year for Analysis",
                                  choices = Yearsavailable,
                                  selected = "2020"
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "input.Analysismode == 'change'",
                      selectInput(inputId ="Year2",
                                  label = "Choose End Year for Analysis",
                                  choices = Yearsavailable,
                                  selected = "2023"
                      )
                    ),
                    
                    selectInput(inputId ="Variable",
                                label = "Choose a Variable to Analyze",
                                choices = Variablename,
                                selected = "Population"
                    ),
                    
                    p("", style = "margin-bottom: 35px;"),
                    
                    actionButton(inputId = "Button",
                                 label = "Generate Map", 
                                 styleclass = "", 
                                 size = "", 
                                 block = F,
                                 icon = NULL, 
                                 icon.library = c("bootstrap", "font awesome")
                    ),
                    
                    p("", style = "margin-bottom: 5px;"),
                    
                    #if button is pressed this pulls up a loading icon
                    shinyjs::hidden(
                      div(id = "loading_note", class = "text-warning", "Loading…")
                    )
                    
                  ),
                  
                  #div panel panel just makes the cards that the stuff sits in
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
              
              # This tab showcases your other apps with three themed panels in a row
              tabPanel(
                "Other Apps",
                
                # HTML from GPT to style the cards
                tags$head(tags$style(HTML("
                   .panel { border-radius:8px; box-shadow:0 2px 6px rgba(0,0,0,.06); border-width:1px; margin-bottom:20px; }
                   .panel .panel-body { min-height: 120px; } /* tweak to taste */

                   /* DEFAULT (warm neutral) */
                   .panel-default { border-color:#a39c96; }
                   .panel-default > .panel-heading { background:#a39c96; border-color:#a39c96; color:#FFFFFF; font-weight:600; }
                   .panel-default > .panel-body { background:#F5F1EC; color:#3F3A37; }
                   .panel-default > .panel-heading a,
                   .panel-default > .panel-heading a:hover,
                   .panel-default > .panel-heading a:focus { color:#FFFFFF; text-decoration:none; }

                   /* PRIMARY (dark cyan) */
                   .panel-primary { border-color:#008B8B; }
                   .panel-primary > .panel-heading { background:#008B8B; border-color:#008B8B; color:#FFFFFF; font-weight:600; }
                   .panel-primary > .panel-body { background:#E6F7F7; color:#0A3F3F; }
                   .panel-primary > .panel-heading a,
                   .panel-primary > .panel-heading a:hover,
                   .panel-primary > .panel-heading a:focus { color:#FFFFFF; text-decoration:none; }

                   /* INFO (minty) */
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

                #These are the three cards and portfolio sites
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
                                 tags$a(href = "https://Mikeweaver.dev/Voyage", "Voyage")),
                             div(class = "panel-body",
                                 "This web app is a working and scalable social media platform where users can post about their travels and experiences.")
                         )
                  ),
                  
                  div(class = "clearfix visible-sm-block"),
                  
                  column(width = 6, class = "col-md-4",
                         div(class = "panel panel-info",
                             div(class = "panel-heading",
                                 tags$a(href = "https://mikeweaver.dev/AIChef", "AI Chef")),
                             div(class = "panel-body",
                                 "AI Chef is for web and mobile (including iOS via Expo). The app integrates AI within a compelling UI/UX to inspire meals based on food users have on-hand.")
                         ),
                  ),
                  
                  column(width = 6, class = "col-md-4",
                         div(class = "panel panel-info",
                             div(class = "panel-heading",
                                 tags$a(href = "https://mikeweaver.dev/SpotifyLab", "SpotifyLab")),
                             div(class = "panel-body",
                                 "Optimized for desktop and mobile and available on the ios and Android app store, this app leverages artificial intelligence to generate bespoke playlists based on user inputs and Spotify listening history.")
                         ),
                  )
                )
              ),
              
             tabPanel( "About",
                       
                       titlePanel(
                         tags$h2("Note from Developer",
                                 style = "color: DarkCyan; margin: 10px 5px 20px 15px;"
                         )),
                       
                       div(class = "panel panel-default",
                           div(class = "panel-heading", "About this Web App"),
                           div(class = "panel-body",
                       
                            h5("This is the first app I built for my software development portfolio. I like it because it utilizes the Census API and millions of data points, but appears very simple from the user perspective.",  style = "margin: 10px 5px 20px 0px;"),  
                            h5("This app reminds me a lot of the coding work I did at my last job. We routinely worked with demographic data and GIS systems to synthesize complex analysis into clear and readable maps. I often used R for my workflow so I felt it a fitting starting point, though in the future I hope to publish projects leverage Python, Javascript, and AI.",  style = "margin: 10px 5px 20px 0px;"),
                            h5("Thanks for visiting my webpage! See more of my apps on the 'Other Apps' Tab",  style = "margin: 10px 5px 20px 0px;"),
                            h5("Mike Weaver",  style = "margin: 0px 5px 20px 0px;")
                           )
                       )
             )
             )
)


server <- function(input, output, session) {
  
  # show the loading note
  observeEvent(input$Button, show("loading_note"))
  
  # base map
  output$Map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      # keep users inside the U.S. envelope (approx CONUS; adjust to taste)
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
  
  # This returns a reactive dataframe with the geography columns and it's based on our existing dataframes and the user inputs
  Mapdata <- eventReactive(input$Button, {
    nm <- paste0(input$Geography, input$Year2)
    d  <- get(nm, inherits = TRUE)  # sf (EPSG:4326 expected)
    if (identical(input$Geography, "Tract") && "Statenames" %in% names(d)) {
      d <- d[d$Statenames == input$State, , drop = FALSE]
    }
    d
  })
  
  #Ok so now we are matching for change over time and creating a column for change
  observeEvent(Mapdata(), {
    d   <- Mapdata()                                 # end-year sf (Year2)
    key <- "GEOID"
    nm  <- as.character(d[["NAME"]])
    
    # Point-in-time values from Year2
    y2 <- suppressWarnings(as.numeric(d[[input$Variable]]))
    v  <- y2
    
    #For legend. in the legend sometimes you need a $ before or % after so this adds that 
    pre  <- if (input$Variable %in% c("Median Household Income","Median Home Value",
                                      "Median Gross Rent","Income Reached by Top 5% of Earners")) " $" else " "
    post <- if (input$Variable %in% c("Percent with Bachelor Degrees")) "%" else " "
    
    legend_title <- input$Variable
    
    # Change-over-time: % change = 100 * (y2 - y1) / y1
    if (identical(input$Analysismode, "change")) {
      nm1 <- paste0(input$Geography, input$Year1)
      d1  <- get(nm1, inherits = TRUE)
      if (identical(input$Geography, "Tract") && "Statenames" %in% names(d1)) {
        d1 <- d1[d1$Statenames == input$State, , drop = FALSE]
      }
      y1 <- suppressWarnings(as.numeric(d1[[input$Variable]][ match(d[[key]], d1[[key]]) ]))
      v  <- ifelse(is.na(y1) | y1 == 0, NA_real_, 100 * (y2 - y1) / y1)
      
      pre  <- ""
      post <- "%"
      legend_title <- paste0(input$Variable, " (", input$Year1, "\u2192", input$Year2, ", % change)")
    }
    
    # This is for the zoom feature. some redundancies included
    mask <- is.finite(v)
    dom  <- v[mask]
    if (!length(dom)) return(invisible(NULL))
    
    # This does a fancy zoom to each thing to make it look cool. For state local its the whole US
    if (input$Geography %in% c("State", "County")) {
      leafletProxy("Map") %>%
        flyToBounds(lng1 = -125, lat1 = 24, lng2 = -66, lat2 = 50)
    } else {
      # For Tract it zooms to the actual tract area
      d_zoom <- d[mask & !sf::st_is_empty(d), , drop = FALSE]
      if (nrow(d_zoom)) {
        if (is.na(sf::st_crs(d_zoom)) || sf::st_crs(d_zoom)$epsg != 4326) {
          d_zoom <- sf::st_transform(d_zoom, 4326)
        }
        bb <- sf::st_bbox(d_zoom)
        leafletProxy("Map") %>%
          fitBounds(lng1 = unname(bb[["xmin"]]), lat1 = unname(bb[["ymin"]]),
                    lng2 = unname(bb[["xmax"]]), lat2 = unname(bb[["ymax"]]))
      }
    }
    # --------------------
    
    # palette is for legend colors
    pal <- colorQuantile(c("#f1f1f1", "#02b3b3"), domain = dom, n = 5, na.color = "transparent")
    
    # legend labels (show cut ranges for change mode)
    lab_fmt <- if (identical(input$Analysismode, "change")) {
      # existing change-over-time logic
      function(type, cuts, p) {
        paste0(
          ifelse(cuts[-length(cuts)] >= 0, "+", ""),
          formatC(cuts[-length(cuts)], format = "f", digits = 1), "% to ",
          ifelse(cuts[-1] >= 0, "+", ""),
          formatC(cuts[-1], format = "f", digits = 1), "%"
        )
      }
    } else {
      # point mode: use fixed labels you define
      function(type, cuts, p) {
        my_labels <- c(
          "Bottom 20th Percentile",
          "20 – 40th Percentile",
          "40 – 60th Percentile",
          "60 – 80th Percentile",
          "80th+ Percentile"
        )
        my_labels
      }
    }
    
    leafletProxy("Map") %>%
      clearGroup("Mapgroup") %>%
      removeControl("legend") %>%
      addPolygons(
        data        = d,
        group       = "Mapgroup",
        fillColor   = pal(v),
        color       = "#999999",
        weight      = 0.25,
        opacity     = 0.4,
        fillOpacity = 0.5,
        label       = paste0(
          nm, " — ", legend_title, ": ", pre,
          formatC(v, format = "f", digits = if (identical(input$Analysismode, "change")) 1 else 0, big.mark = ","),
          post
        ),
        highlightOptions = highlightOptions(weight = 2, color = "#444", bringToFront = TRUE)
      ) %>%
      addLegend("bottomright",
                pal       = pal,
                values    = dom,
                title     = legend_title,
                opacity   = 0.6,
                layerId   = "legend",
                labFormat = lab_fmt)
  })
  
  # hide the note only after the browser added the new vector layer
  observeEvent(input$map_drawn, {
    shinyjs::hide("loading_note")
  })
  
  # Builds the DT data
    Tabledata <- eventReactive(input$Button, {
    if (identical(input$Analysismode, "point")) {
      DTnm <- paste0(input$Geography, input$Year2)
      DTd  <- get(DTnm, inherits = TRUE)
      if (identical(input$Geography, "Tract") && "Statenames" %in% names(DTd)) {
        DTd <- DTd[DTd$Statenames == input$State, , drop = FALSE]
      }
      DTdDF <- dplyr::select(as.data.frame(DTd), -geometry)
      
      # Avoid duplicate columns if input$Variable is "Population" or "Median Household Income"
      wanted <- unique(c("NAME", "Population", "Median Household Income", input$Variable))
      DTdDF  <- dplyr::select(DTdDF, dplyr::any_of(wanted))
      
      #we need a new column for % change if it's change over time that's what this does
    } else {
      DTnm1 <- paste0(input$Geography, input$Year1)
      DTnm2 <- paste0(input$Geography, input$Year2)
      DTd1  <- get(DTnm1, inherits = TRUE)
      DTd2  <- get(DTnm2, inherits = TRUE)
      if (identical(input$Geography, "Tract")) {
        if ("Statenames" %in% names(DTd1)) DTd1 <- DTd1[DTd1$Statenames == input$State, , drop = FALSE]
        if ("Statenames" %in% names(DTd2)) DTd2 <- DTd2[DTd2$Statenames == input$State, , drop = FALSE]
      }
      
      DTdDF1 <- dplyr::select(as.data.frame(DTd1), -geometry)
      DTdDF2 <- dplyr::select(as.data.frame(DTd2), -geometry)
      
      var_sym    <- rlang::sym(input$Variable)
      col_y1     <- paste0(input$Year1, " ", input$Variable)
      col_y2     <- paste0(input$Year2, " ", input$Variable)
      change_col <- paste0("Change over Time (", input$Year1, "\u2192", input$Year2, ")")
      
      y1 <- DTdDF1 %>% dplyr::select(GEOID, NAME, !!var_sym) %>% dplyr::rename(y1_raw = !!var_sym)
      y2 <- DTdDF2 %>% dplyr::select(GEOID,        !!var_sym) %>% dplyr::rename(y2_raw = !!var_sym)
      
      DTdDF <- y1 %>%
        dplyr::left_join(y2, by = "GEOID") %>%
        dplyr::mutate(
          !!col_y1    := y1_raw,
          !!col_y2    := y2_raw,
          !!change_col := dplyr::if_else(!is.na(y1_raw) & y1_raw != 0,
                                         (y2_raw - y1_raw) / abs(y1_raw),  # proportion
                                         NA_real_)
        ) %>%
        dplyr::select(NAME, !!rlang::sym(col_y1), !!rlang::sym(col_y2), !!rlang::sym(change_col))
    }
    
    DTdDF
  })
  
  #had to repeat this line bc it was in a different observe event before and is needed here
  Variable <- eventReactive(input$Button, input$Variable)
    
  # IMPORTANT: match UI id 'Tbl'
  output$Tbl <- DT::renderDT({
    df <- Tabledata()
    req(!is.null(df), is.data.frame(df), nrow(df) > 0)
    
    is_change  <- isolate(input$Analysismode == "change")
    var_name   <- isolate(input$Variable)
    y1_label   <- isolate(paste0(input$Year1, " ", var_name))
    y2_label   <- isolate(paste0(input$Year2, " ", var_name))
    change_lab <- isolate(paste0("Change over Time (", input$Year1, "\u2192", input$Year2, ")"))
    
    dollars  <- c("Median Household Income","Median Home Value","Median Gross Rent",
                  "Income Reached by Top 5% of Earners")
    percents <- c("Percent with Bachelor Degrees")
    ints     <- c("Population","Households","Housing Units",
                  "Number of Employed People","Employees Working from Home")
    one_dec  <- c("Median Age")
    two_dec  <- c("Gini Index of Income Inequality")
    
    df_disp <- df  # copy for display tweaks
    
    # This is annoying but the % variable is actually more than 1 so i
    if (var_name %in% percents) {
      cols_to_scale <- if (is_change) intersect(c(y1_label, y2_label), names(df_disp))
      else intersect(var_name, names(df_disp))
      if (length(cols_to_scale)) {
        df_disp[cols_to_scale] <- lapply(df_disp[cols_to_scale], function(x) x / 100)
      }
    }
    
    w <- DT::datatable(df_disp, filter = "top", rownames = FALSE,
                       options = list(pageLength = 10,     columnDefs = list(
                         list(className = 'dt-left', targets = 0),
                         list(className = 'dt-center', targets = 1),
                         list(className = 'dt-center', targets = 2),
                         if (!(Variable() %in% c("Population","Median Household Income"))) {list(className = 'dt-center', targets = 3)} else {list(className = 'dt-center', targets = 2)}
                       )))
    
    if (is_change) {
      ycols <- intersect(c(y1_label, y2_label), names(df_disp))
      if (length(ycols)) {
        if (var_name %in% dollars) {
          w <- DT::formatCurrency(w, columns = ycols, currency = "$", digits = 0)
        } else if (var_name %in% percents) {
          w <- DT::formatPercentage(w, columns = ycols, digits = 1)  # now correct
        } else if (var_name %in% ints) {
          w <- DT::formatRound(w, columns = ycols, digits = 0)
        } else if (var_name %in% one_dec) {
          w <- DT::formatRound(w, columns = ycols, digits = 1)
        } else if (var_name %in% two_dec) {
          w <- DT::formatRound(w, columns = ycols, digits = 2)
        }
      }
      if (change_lab %in% names(df_disp)) {
        w <- DT::formatPercentage(w, columns = change_lab, digits = 1)
      }
    } else {
      money_cols   <- intersect(names(df_disp), unique(c("Median Household Income", if (var_name %in% dollars) var_name)))
      percent_cols <- intersect(names(df_disp), unique(c(if (var_name %in% percents) var_name)))
      int_cols     <- intersect(names(df_disp), unique(c("Population","Households","Housing Units",
                                                         "Number of Employed People","Employees Working from Home",
                                                         if (var_name %in% ints) var_name)))
      one_dec_cols <- intersect(names(df_disp), unique(c(if (var_name %in% one_dec) var_name)))
      two_dec_cols <- intersect(names(df_disp), unique(c(if (var_name %in% two_dec) var_name)))
      
      if (length(money_cols))   w <- DT::formatCurrency(w, columns = money_cols, currency = "$", digits = 0)
      if (length(percent_cols)) w <- DT::formatPercentage(w, columns = percent_cols, digits = 1)
      if (length(int_cols))     w <- DT::formatRound(w, columns = int_cols, digits = 0)
      if (length(one_dec_cols)) w <- DT::formatRound(w, columns = one_dec_cols, digits = 1)
      if (length(two_dec_cols)) w <- DT::formatRound(w, columns = two_dec_cols, digits = 2)
    }
    
    w
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)