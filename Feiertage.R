


##############################################################################################################
#title: "Feiertage
#Ziel: Tage vor und nach den Feiertagen in einer Variable zu speichern
#Autor: "Merle Oelbüttel"
#Datum des Erstellens: 10.01.2020
################################################################################################################

#Bibliotheken
library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)



neu_Feiertage <- read_csv("neu_Feiertage.csv", 
                          col_types = cols(Datum = col_date(format = "%d.%m.%Y")))

Feiertage<-filter (neu_Feiertage, Feiertag==1)


#erstellen einer Variablen mit allen Tagen, die einen Tag vor dem Feiertag sind
vor_Feiertag<-Feiertage$Datum-days(1) 
nach_Feiertag<-Feiertage$Datum+days(1)

#als dataframe speichern
vor_Feiertag<-as.data.frame(vor_Feiertag)
nach_Feiertag<-as.data.frame(nach_Feiertag)


#nach Feiertag data frame mit einer spalte erweitern und den wert 1 einfügen
vor_Feiertag$vor_feiertag=1
nach_Feiertag$nach_feiertag=1


#als csv im ws abspeichern
write.csv(vor_Feiertag, file = "vor_feiertag.csv")
write.csv(nach_Feiertag, file = "nach_feiertag.csv")

