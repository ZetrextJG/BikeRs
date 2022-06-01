# PDU Projekt nr.3
# 30 maj 2022
# Aplikacja Shiny

library(shiny)
library(shinydashboard)

library(pander)
library(stringi)
library(data.table)
library(tidyverse)
library(ggmap)
library(ggplot2)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "purple", 
  dashboardHeader(title = "CitiBike Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Station to station",
               tabName = "stations",
               icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "stations",
        box(uiOutput("select_station")),
        box(uiOutput("select_month")),
        box(sliderInput(
          inputId = "selected_hours",
          label = h3("Select range of hours"),
          min = 0, max = 24,
          value = c(10, 12),
        ), width = 12),
        box(plotOutput("top_map"), width = 12, height = 610),
        box(title = h3("Stations Out"), dataTableOutput("top_stations_out")),
        box(title = h3("Stations In"), dataTableOutput("top_stations_in"))
        # box(plotOutput("top_stations_out")),
        # box(tableOutput("top_out_table")),
        # box(tableOutput("top_in_table")),
        # box(uiOutput("month_selector")),
        # box(uiOutput("station_selector"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Create in maps
  get_in_map <- function(word){
    ggmap(focused_map()) +
        geom_segment(
            data = stations_in(),
            aes(
                x = long, y = lat,
                xend = picked_station()$long[1], yend = picked_station()$lat[1],
                color = trips_from_selected 
            ),
            size = 1,
            alpha = 0.75
        ) +
        geom_point(
            data = stations_in(),
            aes(x = long, y = lat, color = trips_from_selected),
            size = 3,
            alpha = 0.75
        ) +
        geom_point(
            data = picked_station(),
            aes(x = long, y = lat),
            size = 4,
            alpha = 0.5
        ) +
        scale_colour_gradient(high = "red", low = "green", name = "Trips from selected") +
        theme(
            axis.ticks = element_blank(),
            axis.text = element_blank()
        ) +
        xlab("") +
        ylab("") +
        ggtitle(paste0("Top 10 Trips ending at ",
                       picked_station()$name))
  } 
  
  # Create out maps
  get_out_map <- function(word){
    ggmap(focused_map()) +
        geom_segment(
            data = stations_out(),
            aes(
                x = long, y = lat,
                xend = picked_station()$long[1], yend = picked_station()$lat[1],
                color = trips_to_selected 
            ),
            size = 1,
            alpha = 0.75
        ) +
        geom_point(
            data = stations_out(),
            aes(x = long, y = lat, color = trips_to_selected),
            size = 3,
            alpha = 0.75
        ) +
        geom_point(
            data = picked_station(),
            aes(x = long, y = lat),
            size = 4,
            alpha = 0.5
        ) +
        scale_colour_gradient(high = "red", low = "green", name = "Trips to selected") +
        theme(
            axis.ticks = element_blank(),
            axis.text = element_blank()
        ) +
        xlab("") +
        ylab("") +
        ggtitle(paste0("Top 10 Trips starting at ",
                       picked_station()$name))
  } 
  
  # Create nice cented maps
  get_focused_map <- function(){
    ps <- picked_station()
    get_map(
        location = c(
            lon = ps$long[1],
            lat = ps$lat[1]
        ),
        color = "bw",
        source = "google",
        zoom = 14
    )
  }
  
  # Create default New York city map
  focused_map <- reactive(get_focused_map())
  
  # Get stations out:
  get_stations_out <- function(trips_ds, hour_lower, hour_upper){
    trips_ds %>%
      filter(start_station_id == picked_station()$id[1]) %>%
      mutate(hour = as.numeric(stri_sub(ended_at, 12, 13))) %>%
      filter(hour >= hour_lower) %>%
      filter(hour <= hour_upper) %>%
      group_by(end_station_id) %>%
      summarise(
          trips_to_selected = n(),
          name = end_station_name[1],
          lat = as.numeric(end_lat[1]),
          long = as.numeric(end_lng[1]),
          avg_trip_dur = mean(as.numeric(difftime(ended_at, started_at, units = "mins")))
      ) %>%
      filter(name != picked_station()$name[1]) %>%
      arrange(desc(trips_to_selected)) %>%
      head(10)
  }
  
  # Get stations in
  get_stations_in <- function(trips_ds, hour_lower, hour_upper){
    trips_ds %>%
      filter(end_station_id == picked_station()$id[1]) %>%
      mutate(hour = as.numeric(stri_sub(ended_at, 12, 13))) %>%
      filter(hour >= hour_lower) %>%
      filter(hour <= hour_upper) %>%
      group_by(start_station_id) %>%
      summarise(
          trips_from_selected = n(),
          name = start_station_name[1],
          lat = as.numeric(start_lat[1]),
          long = as.numeric(start_lng[1]),
          avg_trip_dur = mean(as.numeric(difftime(ended_at, started_at, units = "mins")))
      ) %>%
      filter(name != picked_station()$name[1]) %>%
      arrange(desc(trips_from_selected)) %>%
      head(10)
  }
  
  
  # List of months
  month_list <- c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )
  
  # Get all trips from a selected month
  get_trips_by_month <- function(month_id){
    fread(sprintf("../data/NC/2021%02d-citibike-tripdata.csv", month_id)) %>%
      as.data.frame() %>%
      ungroup()
  }
  
  trips <- reactive(get_trips_by_month(which(month_list == input$selected_month)))
  
  # Get station function
  get_stations <- function(){
    trips() %>% group_by(start_station_id) %>%
      summarise(
          n_trips = n(),
          name = start_station_name[1],
          id = as.numeric(start_station_id[1]),
          lat = as.numeric(start_lat[1]),
          long = as.numeric(start_lng[1])
      ) %>%
      arrange(-n_trips)
  }
  
  # Reactive station list
  stations <- reactive(get_stations())
  
  picked_station <- reactive({
    stations() %>% 
      filter(name == input$selected_station) %>% 
      head(1)
  }) 
  
  # Reactive stations in/out
  stations_in <- reactive(get_stations_in(
    trips(),
    as.numeric(input$selected_hours)[1],
    as.numeric(input$selected_hours)[2]
    ))
  stations_out <- reactive(get_stations_out(
    trips(), 
    as.numeric(input$selected_hours)[1],
    as.numeric(input$selected_hours)[2]
  ))
  
  # Station selector logic
  output$select_station <- renderUI({
    selectInput(
      inputId = "selected_station",
      label = h3("Select station to examine"),
      choices = stations()$name
    )
  })
  
  # Month selector logic
  output$select_month <- renderUI({
    selectInput(
      selected = "April",
      inputId = "selected_month",
      label = h3("Select a month of 2021"),
      choices = month_list
    )
  })
  
  # Top stations out table
  output$top_stations_out <- renderDataTable({
    stations_out()
    })
  
  # Top stations int table
  output$top_stations_in <- renderDataTable({
    stations_in()
  })
  
  output$top_map <- renderPlot({
    p1 <- get_in_map()
    p2 <- get_out_map()
    grid.arrange(p2, p1, nrow = 1)
  }, height = 600)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
