# Live Earthquakes Shiny App

library("DT")
library("dplyr")
library("shiny")
library("shinydashboard")
library("leaflet")

server <- function(input, output) {
  
  # Create a function to subset dataset to only take data from yesterday if
  # the user selects the radio button option "since_yesterday".
  switch_df <- function(df) {
    df$split <- substring(df$time, 1,10)
    df$yesterday <- as.Date(df$split)
    df <- filter(df, df$yesterday >= Sys.Date()-1)
    return(df)
    
  }
  
  # Get quakes data from USGS website (I am extracting past 7 days data)
  quake_data <- reactive({
    input$updateButton # Refresh if button clicked
    
    url <- ("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_week.csv")
    all_day_quakes <- read.csv(url)
    
    # Subset if the user chooses "since_yesterday" option
    all_day_quakes2 <- switch(input$time_window,
                            seven_days = all_day_quakes,
                            since_yesterday = switch_df(all_day_quakes) #apply switch_df() function
                            )
  })
  
  
  # Create interactive map
  
  output$quake_map <- renderLeaflet({
    
    # Call quake dataset
    df <- quake_data()
    
    # Categorize magnitude in terms of size
    df$size <- cut(df$mag,breaks = c(-1, 3.9, 4.9, 5.9, 6.9, 7.9, 12),
                   labels=c("minor", "light", "moderate", "strong", "major", "great 8+"))
    
    # Create colour pallet
    col_rainbow<- c("#66ffff","#1aff1a","#f07900","#ff0000","#b30000","#b30059")
    pallet <- colorFactor(col_rainbow, df$size)
    
    # Create popup in HTML
    pop <- paste(
                  "<b>Place:</b>", df$place, "<br>",
                  "<b>Time:</b>", df$time, "<br>",   # should make it nicer format
                  "<b>Mag:</b>", as.character(df$mag), "<br>",
                  "<b>Depth:</b>", as.character(df$depth), "km<br>"
                )
    
    
    ## Leaflet map
    my_map <- leaflet(df, options = leafletOptions(minZoom = 2)) %>% 
      
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      
      setView(100.65, 120.0285, zoom = 1) %>%
      
      setView(24, 10, zoom=1) %>%
      
      addCircles( ~longitude, ~latitude,  

                  weight= ~ifelse(mag < 4, 1, 6),
                  color= ~pallet(size),
                  radius = ~ifelse(mag < 4, 2, 5), # add ifs
                  popup = pop
              
                  )
    
    # Add legend
    my_map <- my_map %>%
      addLegend( "bottomright", pal = pallet,
                 values = sort(df$size),
                 title = "Magnitude")
    
    # Return map
    my_map
    
  })
  

  # Table with 5 latest quakes reported
  output$table <- DT::renderDataTable({
   
    last5 <- quake_data()
    last5 <- select(last5, time, place, magnitude= mag, depth,
                    type)  #miss size
    last5 <- last5[1:5,]
    DT::datatable(last5,options = list(dom = 't'),class = "compact",
                  caption = 'Most recent earthquakes')
  })
  
  
  # Some simple statistics about quakes
  output$stats_quake <- renderUI({

    p(
      strong("Total quakes reported"), br(),
      nrow(quake_data()),
      br(),
      br(),
      
      strong("Strongest quake "), br(),
      max(quake_data()$mag)," magnitude", br(),
      select(filter(quake_data(),mag==max(mag)),place), br(),
      select(filter(quake_data(),mag==max(mag)),time)
    )
    
  })

}



## USER INTERFACE
ui <- dashboardPage( skin="red",
  
  dashboardHeader(
    title = "Live Earthquakes"
    
  
  ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
            
    fluidRow(
      column(width = 10,
             solidHeader = TRUE,
             leafletOutput("quake_map", height = 450)
      
             
             ),
      
      column(width = 2,
             radioButtons("time_window", "Time Window",
                          c("Past 7 days" = "seven_days",
                            "Since Yesterday" = "since_yesterday")),
             br(),
             
             uiOutput("stats_quake"),
             
             br(),
            
             
             helpText(p(("This App shows the latest earthquakes reported by 
                USGS. Click on each circle to see details."))), 
                         
                         
             helpText("You can get more updated data (if available) by 
                clicking on the below button."), 
              
                     
             actionButton("updateButton", "Update Data")
             
            )
  ),
    
  fluidRow(
      box(width = 10,
             DT::dataTableOutput("table")
         
           )
             
      
  )
  
  )

)
  
  
# Put together server and ui and run the app
shinyApp(ui = ui, server = server)

# To deploy it on shinyapps.io:
# from your working directory
# rsconnect::deployApp(appName = "app", appTitle = "Live Earthquakes")
