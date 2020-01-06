


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

ggplot(w1, aes(x=Datum, y=Umsatz)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_label( 
   data=w1 %>% filter(Umsatz>=200), # alles mit einer Summe ab 2000€ gelabelt
    aes(label=Datum)
  )+
  xlab("Jahr")+
  ylab("Summe Umsatz (€)")
ggtitle("Umsatz über den gesamten Zeitraum von w1")# scatterplot


#Tabelle nach Umsatz sortieren
w1<- arrange(w1, desc(Umsatz))

#überprüfen, ob die Werte durch Feiertage/ Ferien beeinflusst sind

besonderes<-read_delim("Variablen.csv", 
                                   ";", escape_double = FALSE, col_types = cols(Datum = col_date(format = "%d.%m.%Y")), 
                                   trim_ws = TRUE)

Feiertage<-filter(besonderes, Feiertage==1)

#heraus
