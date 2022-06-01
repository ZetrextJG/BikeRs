library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)

files = list.files(pattern = ".csv")
ok <- files[1:27]
DT = as.data.frame(rbindlist(lapply(ok, fread, fill = TRUE)))

dane_young <- DT[c("tripduration","usertype","birth year")]
dane_young <- dane_young[dane_young$`birth year` > 1997, ]
dane_young_idk <- dane_young[dane_young$tripduration < 28800   ,c("tripduration","birth year")]
dane_young_idk <- dane_young_idk[dane_young_idk$tripduration > 299,]
zagregowani_young_ilosc <- aggregate(dane_young_idk, dane_young_idk["birth year"], length)
zagregowani_young_ilosc <- zagregowani_young_ilosc[1:2]
colnames(zagregowani_young_ilosc) <- c("birth year", "amount (in thousands)")
zagregowani_young_ilosc$`amount (in thousands)` <- zagregowani_young_ilosc$`amount (in thousands)`/1000
zagregowani_young <- aggregate(dane_young_idk, dane_young_idk["birth year"], mean)
zagregowani_young$tripduration <- zagregowani_young$tripduration/60
zagregowani_young <- zagregowani_young[1:2]

colnames(zagregowani_young) <- c("Rok urodzenia", "Długość jazdy")
ggplot(zagregowani_young, aes(x=`Rok urodzenia`, y=`Długość jazdy`)) + 
  geom_bar(stat = "identity", width=0.7,color="green", fill=rgb(0.1,0.2,0.3,0.4) ) +
  ggtitle("Młodzież, średnia długość jazdy (1998-2005)") + theme_dark()

colnames(zagregowani_young_ilosc) <- c("Rok urodzenia", "ilość (w tys.)")
ggplot(zagregowani_young_ilosc, aes(x=`Rok urodzenia`, y=`ilość (w tys.)`)) + 
  geom_bar(stat = "identity", width=0.7,color="green", fill=rgb(0.1,0.2,0.3,0.4) )+
  ggtitle("Młodzież, ilość jeżdżących (1998-2005)") + theme_dark()
dane_young_sub <- dane_young[dane_young$tripduration < 28800   ,c("usertype","birth year")]
zagregowani_young_users <- aggregate(dane_young_sub, dane_young_sub["usertype"], length)
zagregowani_young_users <- zagregowani_young_users[1:2]
colnames(zagregowani_young_users) <- c("Typ użytkownika", "Ilość")
ggplot(zagregowani_young_users, aes(x=`Typ użytkownika`, y=`Ilość`)) + 
  geom_bar(stat = "identity", width=0.7,color="green", fill=rgb(0.1,0.2,0.3,0.4) )+
  ggtitle("Młodzież, ilość posiadających subskrybcje") + coord_flip() + theme_dark()

#brak weryfikacji wieku 

dane_old <- DT[c("tripduration","usertype","birth year")]
dane_old <- dane_old[dane_old$`birth year` < 1963, ]
dane_old <- dane_old[dane_old$`birth year` > 1955, ]
dane_old_idk <- dane_old[dane_old$tripduration < 28800   ,c("tripduration","birth year")]
dane_old_idk <- dane_old_idk[dane_old_idk$tripduration > 299,]
zagregowani_old <- aggregate(dane_old_idk, dane_old_idk["birth year"], mean)
zagregowani_old_ilosc <- aggregate(dane_old_idk, dane_old_idk["birth year"], length)
zagregowani_old_ilosc <- zagregowani_old_ilosc[1:2]
colnames(zagregowani_old_ilosc) <- c("birth year", "amount (in thousands)")
zagregowani_old_ilosc$`amount (in thousands)` <- zagregowani_old_ilosc$`amount (in thousands)`/1000
zagregowani_old$tripduration <- zagregowani_old$tripduration/60
zagregowani_old <- zagregowani_old[1:2]

colnames(zagregowani_old) <- c("Rok urodzenia", "Długość jazdy")
ggplot(zagregowani_old, aes(x=`Rok urodzenia`, y=`Długość jazdy`)) + 
  geom_bar(stat = "identity", width=0.7,color="green", fill=rgb(0.1,0.2,0.3,0.4)) +
  ylim(0,25)+ggtitle("Średnia długość jazdy (1956-1962)") + theme_dark()

colnames(zagregowani_old_ilosc) <- c("Rok urodzenia", "ilość (w tys.)")
ggplot(zagregowani_old_ilosc, aes(x=`Rok urodzenia`, y=`ilość (w tys.)`)) + 
  geom_bar(stat = "identity", width=0.7,color="green", fill=rgb(0.1,0.2,0.3,0.4) )+
  ggtitle("Ilość jeżdżących (1956-1962)") + theme_dark()
dane_old_sub <- dane_old[dane_old$tripduration < 28800   ,c("usertype","birth year")]
zagregowani_old_users <- aggregate(dane_old_sub, dane_old_sub["usertype"], length)
zagregowani_old_users <- zagregowani_old_users[1:2]
colnames(zagregowani_old_users) <- c("Typ użytkownika", "Ilość")
ggplot(zagregowani_old_users, aes(x=`Typ użytkownika`, y=`Ilość`)) + 
  geom_bar(stat = "identity", width=0.7,color="green", fill=rgb(0.1,0.2,0.3,0.4) )+
  ggtitle("Ilość posiadających subskrybcje (1956-1962)") + coord_flip() + theme_dark()

zagregowani <- rbind(zagregowani_old, zagregowani_young)
ggplot(zagregowani, aes(x=`birth year`, y=`tripduration`)) + 
  geom_bar(stat = "identity", width=0.5,color="black", fill=rgb(0.1,0.2,0.3,0.4)) +
  ylim(0,25) 

