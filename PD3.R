#Praca Domowa 3
#Lukasz Grabarski
#Co sie dzieje w Nowym Jorku w noc sylwestrowa?


library(dplyr)

rowery_2018_12 <- read.csv("201812-citibike-tripdata.csv", sep = ",", header = T)
rowery_2019_01 <- read.csv("201901-citibike-tripdata.csv", sep = ",", header = T)
rowery_2019_12 <- read.csv("201912-citibike-tripdata.csv", sep = ",", header = T)
rowery_2020_01 <- read.csv("202001-citibike-tripdata.csv", sep = ",", header = T)
rowery_2020_12 <- read.csv("202012-citibike-tripdata.csv", sep = ",", header = T)
rowery_2021_01 <- read.csv("202101-citibike-tripdata.csv", sep = ",", header = T)

rowery_2018_12 <- tibble(rowery_2018_12)
rowery_2019_01 <- tibble(rowery_2019_01)
rowery_2019_12 <- tibble(rowery_2019_12)
rowery_2020_01 <- tibble(rowery_2020_01)
rowery_2020_12 <- tibble(rowery_2020_12)
rowery_2021_01 <- tibble(rowery_2021_01)


#interesuje nas tylko sylwester
filter(rowery_2018_12,
       substr(starttime, 9, 10) == "31" &
         as.numeric(substr(starttime, 12, 13)) > 19) -> rowery_2018_12
rowery_2018_12 <- rowery_2018_12 %>% 
   mutate(sylwester = as.numeric(substr(.$starttime, 1, 4)))

filter(rowery_2019_01,
       substr(starttime, 9, 10) == "01" &
         as.numeric(substr(starttime, 12, 13)) < 7) -> rowery_2019_01
rowery_2019_01 <- rowery_2019_01 %>% 
  mutate(sylwester = as.numeric(substr(.$starttime, 1, 4)) - 1)

filter(rowery_2019_12,
       substr(starttime, 9, 10) == "31" &
         as.numeric(substr(starttime, 12, 13)) > 19) -> rowery_2019_12
rowery_2019_12 <- rowery_2019_12 %>% 
  mutate(sylwester = as.numeric(substr(.$starttime, 1, 4)))

filter(rowery_2020_01,
       substr(starttime, 9, 10) == "01" &
         as.numeric(substr(starttime, 12, 13)) < 7) -> rowery_2020_01
rowery_2020_01 <- rowery_2020_01 %>% 
  mutate(sylwester = as.numeric(substr(.$starttime, 1, 4)) - 1)

filter(rowery_2020_12,
       substr(starttime, 9, 10) == "31" &
         as.numeric(substr(starttime, 12, 13)) > 19) -> rowery_2020_12
rowery_2020_12 <- rowery_2020_12 %>% 
  mutate(sylwester = as.numeric(substr(.$starttime, 1, 4)))

filter(rowery_2021_01,
       substr(starttime, 9, 10) == "01" &
         as.numeric(substr(starttime, 12, 13)) < 7) -> rowery_2021_01
rowery_2021_01 <- rowery_2021_01 %>% 
  mutate(sylwester = as.numeric(substr(.$starttime, 1, 4)) - 1)

#zgodnosc typow start.station.id: character vs intiger
rowery_2019_01$start.station.id <- as.integer(rowery_2019_01$start.station.id)
rowery_2019_01$end.station.id <- as.integer(rowery_2019_01$end.station.id)
filter(rowery_2019_01,
       !is.na(start.station.id),
       !is.na(end.station.id)) -> rowery_2019_01

rowery_2018_12$start.station.id <- as.integer(rowery_2018_12$start.station.id)
rowery_2018_12$end.station.id <- as.integer(rowery_2018_12$end.station.id)
filter(rowery_2018_12,
       !is.na(start.station.id),
       !is.na(end.station.id)) -> rowery_2018_12

#poniewaz naglowki sa te same, laczymy ramki w jedna
bind_rows(rowery_2018_12, rowery_2019_01, rowery_2019_12,
          rowery_2020_01, rowery_2020_12, rowery_2021_01) -> rowery

filter(rowery,
       substr(stoptime, 9, 10) %in% c("31","01"),
       as.numeric(substr(stoptime, 12, 13)) %in% c(20:24, 0:6)
       ) -> rowery

rowery <- rowery %>% mutate(rowery, wiek = .$sylwester - .$birth.year)
mutate(rowery, grupa_wiekowa = 15 + 5*findInterval(rowery$wiek, seq(from = 20, to = 100, by = 5))) -> rowery

#tu sie bawie lokalizacja
count(rowery, start.station.name, sort = T) -> zliczone_staccje_pocz
count(rowery, end.station.name, sort = T) -> zliczone_staccje_konc
hist(zliczone_staccje_pocz$n)
hist(zliczone_staccje_konc$n)

#tu jest topka najdluzszych przejazdow z uwzglednieniem sylwestra
select(rowery, tripduration, start.station.name, end.station.name, birth.year, gender, sylwester) -> rowery_czas
filter(rowery_czas, gender != 0) -> rowery_czas
arrange(rowery_czas, -tripduration)

#tu patrze, jakie gurpy wiekowe jezdza najdluzej
arrange(summarise(group_by(rowery, grupa_wiekowa), sredni_czas = mean(tripduration))) -> srednie_czasy
inner_join(srednie_czasy,count(rowery, grupa_wiekowa)) -> srednie_czasy
filter(srednie_czasy, n > 50) -> srednie_czasy

#wykres 1
barplot(srednie_czasy$n,
        main = "Suma przejazdow grup wiekowych w noc sylwestrowa",
        col = topo.colors(length(srednie_czasy$grupa_wiekowa)),
        ylim = c(0, max(srednie_czasy$n)+500),
        las = 1,
        names.arg = srednie_czasy$grupa_wiekowa
        ) -> wykres_suma
text(wykres_suma, srednie_czasy$n + 100, srednie_czasy$n)

#wykres 2
barplot(srednie_czasy$sredni_czas,
        main = "Srednia dlugosc przejazdow w sekundach",
        col = topo.colors(length(srednie_czasy$grupa_wiekowa)),
        ylim = c(0, max(srednie_czasy$sredni_czas + 300)),
        las = 1,
        names.arg = srednie_czasy$grupa_wiekowa) -> wykres_srednia
text(wykres_srednia, srednie_czasy$sredni_czas + 100, round(srednie_czasy$sredni_czas))

#wykres 3
table(as.numeric(substr(rowery$starttime, 12, 13))) -> godziny
godziny[c(8:11,1:7)] -> godziny
godziny
barplot(godziny,
        main = "Suma startow przejazdow z podzialem na godziny",
        col = topo.colors(length(godziny)),
        ylim = c(0, max(godziny) + 200),
        las = 1) -> wykres_godziny
text(wykres_godziny, godziny + 100, godziny)

#bieda wykres 4
count(rowery, gender) -> plec
pie(plec$n, c("NA","Panowie","Panie")) -> glowne_proporcje

