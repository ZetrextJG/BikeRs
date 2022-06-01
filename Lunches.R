# PDU Projekt nr.3
# 30 maj 2022
# Lunch hours

library(pander)
library(data.table)
library(tidyverse)
library(ggmap)
library(ggplot2)
library(stringi)

# Loading data
april20 <- fread("data/NC/202004-citibike-tripdata.csv")
april21 <- fread("data/NC/202104-citibike-tripdata.csv")
may21 <- fread("data/NC/202105-citibike-tripdata.csv")
june21 <- fread("data/NC/202106-citibike-tripdata.csv")

# Parsing data
april20_data <- april20 %>%
  group_by(`end station id`) %>%
  summarise(
    end_name = `end station name`[1],
    avg_hour = mean(as.numeric(stri_sub(`stoptime`, 12, 13))),
    lat = as.numeric(`end station latitude`[1]),
    long = as.numeric(`end station longitude`[1]),
    lunch_trips = n() / 31
  ) %>%
  ungroup() %>%
  filter(avg_hour < 14) %>%
  filter(lunch_trips  > 10) %>%
  arrange(-lunch_trips) %>%
  head(5)

april21_data <- april21 %>%
  group_by(end_station_id) %>%
  summarise(
    end_name = end_station_name[1],
    avg_hour = mean(as.numeric(stri_sub(ended_at, 12, 13))),
    lat = as.numeric(end_lat[1]),
    long = as.numeric(end_lng[1]),
    lunch_trips = n() / 31
  ) %>%
  ungroup() %>%
  filter(avg_hour < 14) %>%
  filter(lunch_trips  > 10) %>%
  arrange(-lunch_trips) %>%
  head(5)

april21_data2 <- april21 %>%
  ungroup() %>%
  select(end_station_id, ended_at, end_station_name, end_lat, end_lng) %>%
  filter(end_station_id != "") %>%
  mutate(hour = as.numeric(stri_sub(ended_at, 12, 13)), .keep="unused") %>%
  group_by(end_station_id, hour) %>%
  summarise(
    end_name = end_station_name[1],
    lat = as.numeric(end_lat[1]),
    long = as.numeric(end_lng[1]),
    n_trips = n()
  ) %>%
  ungroup() %>%
  filter(hour %in% c(11,12,13)) %>%
  group_by(end_station_id) %>%
  summarise(
    end_name = end_name[1],
    lat = as.numeric(lat[1]),
    long = as.numeric(long[1]),
    lunch_trips = sum(n_trips) / (31)
  ) %>%
  arrange(-lunch_trips) %>%
  head(5)
  
# New York map
nyc_map <- get_map(
    location = "MidTown Ease, New York",
    color = "bw",
    maptype = "terrain",
    source = "google",
    zoom = 13
)

ggmap(nyc_map)

create_nice_map  <- function(dataset, title){
  ggmap(nyc_map) +
      geom_point(
          data = dataset,
          aes(
              x = long,
              y = lat,
              color = lunch_trips
          ),
          size = 10,
          alpha = 0.75
      ) +
      scale_color_gradient(high = "red", low = "green", name = "Number of trips \n per day") +
      geom_text(
        color = "blue",
        size = 5,
        aes(x=long, y=lat, label=end_name),
        data=dataset, 
        hjust=-0.1,
        vjust=2) +
      theme(axis.ticks = element_blank(), axis.text = element_blank()) +
      ggtitle(title)+
      xlab("") +
      ylab("")
}

create_nice_map(april21_data2, title = "Most active stations during lunch break - April 2021 \n trips from hours 11,12,13")

ggsave("maps/LunchMap.png")

create_nice_map(april21_data, title = "Stations mostly active during lunch break - April 2021 \n trips from all hours")

ggsave("maps/LunchMap2.png")

create_nice_map(april20_data, title = "Stations mostly active during lunch break - April 2020 \n trips from all hours")

ggsave("maps/LunchMap2020.png")

