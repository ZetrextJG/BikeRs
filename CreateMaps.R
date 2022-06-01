# PDU Projekt nr.3
# 24 maj 2022
# Tworzenie podstawowych map

library(pander)
library(data.table)
library(tidyverse)
library(ggmap)
library(ggplot2)

d1 <- fread("data/NC/202004-citibike-tripdata.csv")
difftime(d1$ended_at, d1$started_at, units = "mins")
pander(head(d1))

stations <- d1 %>%
    as.data.frame() %>%
    ungroup() %>%
    group_by(`start station name`) %>%
    summarise(
        start_id = as.numeric(`start station id`[1]),
        lat = as.numeric(`start station latitude`[1]),
        long = as.numeric(`start station longitude`[1]),
        n_trips = n()
    )

## Map of all the station
new_york_map <- get_map(
    location = "Noho, New York",
    maptype = "roadmap",
    color = "bw",
    zoom = 12
)

ggmap(new_york_map) +
    theme(axis.ticks = element_blank(), axis.text = element_blank()) +
    xlab("") +
    ylab("")

ggsave("maps/NewYorkMap.png")

ggmap(new_york_map) +
    geom_point(data = stations, aes(
        x = long,
        y = lat
    ), color = "red", size = 1) +
    theme(axis.ticks = element_blank(), axis.text = element_blank()) +
    xlab("") +
    ylab("")

ggsave("maps/NewYorkStations.png")


## Restrict view to Manhattan
manhattan_map <- get_map(
    location = "Manhattan, New York",
    maptype = "roadmap",
    color = "bw",
    zoom = 12
)

ggmap(manhattan_map) +
    theme(axis.ticks = element_blank(), axis.text = element_blank()) +
    xlab("") +
    ylab("")

ggsave("maps/ManhattanMap.png")

ggmap(manhattan_map) +
    geom_point(data = stations, aes(
        x = long,
        y = lat
    ), color = "red", size = 1) +
    theme(axis.ticks = element_blank(), axis.text = element_blank()) +
    xlab("") +
    ylab("")

ggsave("maps/ManhattanStations.png")

## Gradient Map Manhattan

ggmap(manhattan_map) +
    geom_point(
        data = stations,
        aes(
            x = long,
            y = lat,
            color = n_trips
        ),
        size = 2,
        alpha = 0.75
    ) +
    scale_colour_gradient(high = "red", low = "green") +
    theme(axis.ticks = element_blank(), axis.text = element_blank()) +
    xlab("") +
    ylab("")

ggsave("maps/ManhattanStationTraffic.png")

## Gradient Map New York
ggmap(new_york_map) +
    geom_point(
        data = stations,
        aes(
            x = long,
            y = lat,
            color = n_trips
        ),
        size = 2,
        alpha = 0.75
    ) +
    scale_colour_gradient(high = "red", low = "green", name = "Number of trips") +
    theme(axis.ticks = element_blank(), axis.text = element_blank()) +
    xlab("") +
    ylab("")

ggsave("maps/NewYorkStationTraffic.png")

## Busiest Stations

busiest_station <- stations %>%
    arrange(desc(n_trips)) %>%
    head(1)

busy_station_out <- d1 %>%
    filter(start_station_id == busiest_station$start_id[1]) %>%
    group_by(end_station_id) %>%
    summarise(
        n_trips = n(),
        name = end_station_name[1],
        start_lat = as.numeric(start_lat[1]),
        start_lon = as.numeric(start_lng[1]),
        end_lat = as.numeric(end_lat[1]),
        end_lon = as.numeric(end_lng[1])
    ) %>%
    arrange(desc(n_trips)) %>%
    head(20)

busy_station_map <- get_map(
    location = c(
        lon = busiest_station$long[1],
        lat = busiest_station$lat[1]
    ),
    color = "bw",
    source = "google",
    zoom = 14
)

ggmap(busy_station_map)

ggmap(busy_station_map) +
    geom_segment(
        data = busy_station_out,
        aes(
            x = start_lon, y = start_lat,
            xend = end_lon, yend = end_lat,
            color = n_trips
        ),
        size = 1,
        alpha = 0.75
    ) +
    geom_point(
        data = busy_station_out,
        aes(x = end_lon, y = end_lat, color = n_trips),
        size = 3,
        alpha = 0.75
    ) +
    geom_point(
        data = busiest_station,
        aes(x = long, y = lat),
        size = 4,
        alpha = 0.5
    ) +
    scale_colour_gradient(high = "red", low = "green") +
    theme(
        axis.ticks = element_blank(),
        axis.text = element_blank()
    ) +
    xlab("") +
    ylab("") +
    ggtitle(paste0("Top 20 Trips starting at ",
                   busiest_station$start_station_name))

# Density of stations

newyork_terrain_map <- get_map(
    location = "Noho, New York",
    color = "color",
    maptype = "terrain",
    source = "google",
    zoom = 12
)

rides <- d1[
    ,
    c("ride_id", "start_lat", "start_lng")
][
    ,
    lat := as.numeric(start_lat)
][
    ,
    lng := as.numeric(start_lng),
][
    ,
    c("ride_id", "lat", "lng")
]

ggmap(newyork_terrain_map)

ggsave("maps/NewYorkTerrainMap.png")

ggmap(newyork_terrain_map) + stat_density2d(
    aes(x = lng, y = lat, fill = ..level.., alpha = 0.25),
    size = 0.1, bins = 40, data = rides,
    geom = "polygon"
)

ggsave("maps/StationInDensity.png")
