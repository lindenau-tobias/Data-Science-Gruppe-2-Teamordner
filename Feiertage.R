


##############################################################################################################
#title: "Lineare Regression"
#Ziel: Erstellen einer linearen Regression , nach http://r-statistics.co/Linear-Regression.html
#Autor: "Merle Oelbüttel"
#Datum des Erstellens: 06.01.2020
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

#erstellen einer Variablen mit allen Tagen, die einen Tag vor dem Feiertag sind
vor_Feiertag<-Feiertage$Datum-days(1)
vor_Feiertag<-as.data.frame(vor_Feiertag)

#alle Daten vor einem Feiertag
vor_Feiertag<-umsatz%>% filter(Datum %in% vor_Feiertag$vor_Feiertag)
