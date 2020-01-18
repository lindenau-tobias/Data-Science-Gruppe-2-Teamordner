


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


#als dataframe speichern
vor_Feiertag<-as.data.frame(vor_Feiertag)

#alle Daten vor einem Feiertag
vor_Feiertag<-umsatz%>% filter(Datum %in% vor_Feiertag$vor_Feiertag)

#nach Feiertag data frame mit einer spalte erweitern und den wert 1 einfügen

vor_Feiertag$vor_feiertag=1




#ueberfluessige Spalten loeschen
vor_Feiertag$Umsatz<-NULL
vor_Feiertag$Warengruppe<-NULL

#als csv im ws abspeichern
write.csv(vor_Feiertag, file = "vor_feiertag.csv")

#Nach einem Feiertag

#erstellen einer Variable mit den Tagen nach einem Feiertag
nach_Feiertag<-Feiertage$Datum+days(1)
nach_Feiertag<-as.data.frame(nach_Feiertag)

#alle Daten nach einem Feiertag
nach_Feiertag<-umsatz%>% filter(Datum %in% nach_Feiertag$nach_Feiertag)

#nach Feiertag data frame mit einer spalte erweitern und den wert 1 einfügen

nach_Feiertag$nach_feiertag=1

#ueberfluessige Spalten loeschen
nach_Feiertag$Umsatz<-NULL
nach_Feiertag$Warengruppe<-NULL

#als csv im ws abspeichern
write.csv(nach_Feiertag, file = "nach_feiertag.csv")


#große Master-Tabelle in den Datensatz importieren

master_df <- read_csv("master_df", col_types = cols(Datum = col_date(format = "%Y-%m-%d")))

#die Wochentage danebenschreiben 
master_df$wochentagname <- weekdays(master_df$Datum)

#nur die Dienstage herausfiltern (Dienstage sind willkürlich gewählt)
Dienstag<- filter(master_df, wochentagname=="Dienstag")


#Tabelle für den Umsatz nach den unterschiedlichen Variablen
umsatz_Dienstag<- Dienstag %>%
  group_by( Ferien, vor_feiertag, nach_feiertag, KielerWoche, Temperaturklassen ) %>%
  summarise(
    n=n(), #ist praktisch, weil es die Anzahl z?hlt
    mean=mean(Gesamtumsatz),
    sd=sd(Gesamtumsatz),
    summe=sum(Gesamtumsatz)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))




# nach Warengruppe nach Jahren den Umsatz plotten
ggplot(ware_jahr) +
  geom_bar( aes(x=Warengruppe, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=Warengruppe, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ylab("Durchschnitt (€)")+
  facet_wrap(~jahr)+
  ggtitle("Umsatz nach Warengruppe und Jahr")
