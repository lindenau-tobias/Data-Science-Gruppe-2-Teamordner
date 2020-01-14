


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

#Einlsen der Daten

umsatz <- read_csv("umsatzdaten_gekuerzt.csv", 
                                 col_types = cols(Datum = col_date(format = "%Y-%m-%d")))
#überprüfen, ob die Werte durch Feiertage/ Ferien beeinflusst sind

besonderes<-read_delim("besondere_Tage.csv", 
                                   ";", escape_double = FALSE, col_types = cols(Datum = col_date(format = "%d.%m.%Y")), 
                                   trim_ws = TRUE)

Feiertage<-filter(besonderes, Feiertage==1)


#Variable für Weihnachten und Neujahr
## Neujahr
Neujahr<-as.data.frame(seq(as.Date("2013/1/1"), as.Date("2019/1/1"), "years"))


#Weihnachten als Feiertag einfügen 

Weihnachten_25<-as.data.frame(as.Date("2013-12-25") %m+% years(c(0:7)))
Weihnachten_26<-as.data.frame(as.Date("2013-12-26") %m+% years(c(0:7)))
Weihnachten_27<-as.data.frame(as.Date("2013-12-27") %m+% years(c(0:7)))

Weihnachten<-bind_rows(Weihnachten_25,Weihnachten_26, Weihnachten_27)

#Weihnachten der Feiertagetabelle hinzufügen
Feiertage<-bind_rows(Feiertage, Weihnachten, Neujahr)

#erstellen einer Variablen mit allen Tagen, die einen Tag vor dem Feiertag sind
vor_Feiertag<-Feiertage$Datum-days(1) 
vor_Feiertag<-as.data.frame(vor_Feiertag)

#alle Daten vor einem Feiertag
vor_Feiertag<-umsatz%>% filter(Datum %in% vor_Feiertag$vor_Feiertag)



#erstellen einer Variable mit den Tagen nach einem Feiertag
nach_Feiertag<-Feiertage$Datum+days(1)
nach_Feiertag<-as.data.frame(nach_Feiertag)

#alle Daten nach einem Feiertag
nach_Feiertag<-umsatz%>% filter(Datum %in% nach_Feiertag$nach_Feiertag)



#Versuchen, alle Tage wegzufiltern, die ein Feiertag vor einem Feiertag sind


