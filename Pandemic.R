# PDU Projekt nr.3
# 30 maj 2022
# Pandemic Data

library(pander)
library(data.table)
library(tidyverse)
library(ggmap)
library(ggplot2)

april20 <- fread("data/NC/202004-citibike-tripdata.csv")
may20 <- fread("data/NC/202005-citibike-tripdata.csv")
june20 <- fread("data/NC/202006-citibike-tripdata.csv")

april19 <- fread("data/NC/201904-citibike-tripdata.csv")
may19 <- fread("data/NC/201905-citibike-tripdata.csv")

summary(june20)

# Data from 2019
april19_data <- april19 %>%
  group_by(`start station id`) %>%
  summarise(
    start_name = `start station name`[1],
    lat = as.numeric(`start station latitude`[1]),
    long = as.numeric(`start station longitude`[1]),
    n_trips = n() / 31
  ) %>%
  rename(start_id = `start station id`)

may19_data <- may19 %>%
  group_by(`start station id`) %>%
  summarise(
    start_name = `start station name`[1],
    lat = as.numeric(`start station latitude`[1]),
    long = as.numeric(`start station longitude`[1]),
    n_trips = n() / 31
  ) %>%
  rename(start_id = `start station id`)

# Data from 2020
april20_data <- april20 %>%
  group_by(`start station id`) %>%
  summarise(
    start_name = `start station name`[1],
    lat = as.numeric(`start station latitude`[1]),
    long = as.numeric(`start station longitude`[1]),
    n_trips = n() / 31
  ) %>%
  rename(start_id = `start station id`)

may20_data <- may20 %>%
  group_by(`start station id`) %>%
  summarise(
    start_name = `start station name`[1],
    lat = as.numeric(`start station latitude`[1]),
    long = as.numeric(`start station longitude`[1]),
    n_trips = n() / 31
  ) %>%
  rename(start_id = `start station id`)

june20_data <- june20 %>%
  group_by(`start station id`) %>%
  summarise(
    start_name = `start station name`[1],
    lat = as.numeric(`start station latitude`[1]),
    long = as.numeric(`start station longitude`[1]),
    n_trips = n() / 31
  ) %>%
  rename(start_id = `start station id`)

# New York map
nyc_map <- get_map(
    location = "Noho, New York",
    color = "color",
    maptype = "terrain",
    source = "google",
    zoom = 12
)

# Map for April 2019
ggmap(nyc_map) +
  stat_summary_2d(
    data = april19_data,
    aes(x = long, y = lat, z = n_trips),
    fun = mean,
    alpha = 0.6,
    bins = 30) +
  scale_fill_gradient(name = "Number of trips \n per day", low = "green", high = "red", limits = c(0, 300)) +
  ggtitle("Average number of trip per day in April 2019")

ggsave("maps/April2019.png")

# Map for May 2019
ggmap(nyc_map) +
  stat_summary_2d(
    data = may19_data,
    aes(x = long, y = lat, z = n_trips),
    fun = mean,
    alpha = 0.6,
    bins = 30) +
  scale_fill_gradient(name = "Number of trips \n per day", low = "green", high = "red", limits = c(0, 300)) +
  ggtitle("Average number of trip per day in May 2019")

ggsave("maps/Map2019.png")

# Map for April 2020
ggmap(nyc_map) +
  stat_summary_2d(
    data = april20_data,
    aes(x = long, y = lat, z = n_trips),
    fun = mean,
    alpha = 0.6,
    bins = 30) +
  scale_fill_gradient(name = "Number of trips \n per day", low = "green", high = "red", limits = c(0, 300)) +
  ggtitle("Average number of trip per day in April 2020")

ggsave("maps/April2020.png")

# Map for May 2020
ggmap(nyc_map) +
  stat_summary_2d(
    data = may20_data,
    aes(x = long, y = lat, z = n_trips),
    fun = mean,
    alpha = 0.6,
    bins = 30) +
  scale_fill_gradient(name = "Number of trips \n per day", low = "green", high = "red", limits = c(0, 300)) +
  ggtitle("Average number of trip per day in May 2020")

ggsave("maps/May2020.png")

# Map for June 2020
ggmap(nyc_map) +
  stat_summary_2d(
    data = june20_data,
    aes(x = long, y = lat, z = n_trips),
    fun = mean,
    alpha = 0.6,
    bins = 30) +
  scale_fill_gradient(name = "Number of trips \n per day", low = "green", high = "red", limits = c(0, 300)) +
  ggtitle("Average number of trip per day in June 2020")

ggsave("maps/June2020.png")
