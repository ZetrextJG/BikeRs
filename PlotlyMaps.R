# PDU Projekt nr.3
# 30 maj 2022
# Plotly Interactive Maps

library(pander)
library(data.table)
library(tidyverse)
library(plotly)
library(leaflet)
library(rgdal)

april21 <- fread("data/NC/202104-citibike-tripdata.csv")

stations <- april21 %>%
  as.data.frame() %>%
  ungroup() %>%
  group_by(start_station_name) %>%
  summarise(
      start_id = as.numeric(start_station_id[1]),
      lat = as.numeric(start_lat[1]),
      lon = as.numeric(start_lng[1]),
      n_trips = n()
  ) %>%
  filter(start_station_name != "")

busiest_station <- stations %>%
    arrange(desc(n_trips)) %>%
    head(1)

busy_station_out <- april21 %>%
    filter(end_station_id == busiest_station$start_id[1]) %>%
    group_by(start_station_id) %>%
    summarise(
        n_trips = n(),
        name = start_station_name[1],
        lat = as.numeric(start_lat[1]),
        lon = as.numeric(start_lng[1]),
    ) %>%
    arrange(desc(n_trips)) %>%
    head(10)

leaflet(data = busy_station_out) %>%
  setView(lat = busiest_station$lat[1], lng = busiest_station$lon[1], zoom = 13) %>%
  addTiles() %>%
  addMarkers(data = busy_station_out, 
             label = busy_station_out$name)
