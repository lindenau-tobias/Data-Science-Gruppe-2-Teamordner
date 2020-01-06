


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
                   col_types = cols(Datum = col_datetime(format = " %Y-%m-%d   "))) #Eingeben des Datums

##Für Warengruppe 1
w1<-filter(umsatz, Warengruppe==1) #filtern Warengruppe 1
#scatter plot

scatter.smooth(x=w1$Datum, y=w1$Umsatz, main="Warengruppe 1: Umsatz ~ Datum")  # scatterplot
