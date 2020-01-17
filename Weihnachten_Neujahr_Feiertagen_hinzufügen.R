



##############################################################################################################
#title: "lineares model input "Vorbereitung"
#Ziel: Alle Outputs zusammentragen und eine neu csv erzeugen, welche als Input für unser lineares model verwendet werden kann
#Autor: "Tobias Lindenau"
#Datum des Erstellens: 14.01.2020
################################################################################################################

#Bibliotheken
library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)

#Einlsen der Daten


besonderes<-read_delim("besondere_Tage.csv", 
                       ";", escape_double = FALSE, col_types = cols(Datum = col_date(format = "%d.%m.%Y")), 
                       trim_ws = TRUE)

############## Variable für Weihnachten und Neujahr #########################

### NEUJAHR ###

Neujahr<-seq(as.Date("2013/1/1"), as.Date("2019/1/1"), "years")

#Neujahrstage data frame erzeugen
Neujahr<-data.frame(seq(as.Date("2013/1/1"), as.Date("2019/1/1"), "years"))

#Nuejahrs data frame mit einer spalte erweitern und den wert 1 einfügen
Neujahr$neujahrstage=1

#Datumsspalte in Datum umbennen
neujahrstage <- Neujahr %>% 
  rename(
    Datum = seq.as.Date..2013.1.1....as.Date..2019.1.1.....years..
  )

#Variable neujahrstage als csv im workspace abspeichern
write.csv(neujahrstage, file = "neujahrstage.csv")



### Weihnachten ###

##

# Weihnachtsfeiertag_25 (25.12.) data frame erzeugen #
weihnachtsfeiertag_25<-as.data.frame(as.Date("2013-12-25") %m+% years(c(0:7)))

#Weihnachtsfeiertag_25 data frame mit einer spalte erweitern und den wert 1 einfügen
weihnachtsfeiertag_25$weihnachtsfeiertag_25=1

#Datumsspalte in Datum umbennen
weihnachtsfeiertag_25 <- weihnachtsfeiertag_25 %>% 
  rename(
    Datum = `as.Date("2013-12-25") %m+% years(c(0:7))`
  )

#Variable weihnachtsfeiertag_25 als csv im workspace abspeichern
write.csv(weihnachtsfeiertag_25, file = "weihnachtsfeiertag_25.csv")

##

# Weihnachtsfeiertag_26 (26.12.) data frame erzeugen #
weihnachtsfeiertag_26<-as.data.frame(as.Date("2013-12-25") %m+% years(c(0:7)))

#Weihnachtsfeiertag_26 data frame mit einer spalte erweitern und den wert 1 einfügen
weihnachtsfeiertag_26$weihnachtsfeiertag_26=1

#Datumsspalte in Datum umbennen
weihnachtsfeiertag_26 <- weihnachtsfeiertag_26 %>% 
  rename(
    Datum = `as.Date("2013-12-26") %m+% years(c(0:7))`
  )

#Variable weihnachtsfeiertag_26 als csv im workspace abspeichern
write.csv(weihnachtsfeiertag_26, file = "weihnachtsfeiertag_26.csv")


## Weihnachtsfeiertage_all data frame erzeugen ##

Weihnachten_25<-as.data.frame(as.Date("2013-12-25") %m+% years(c(0:7)))
Weihnachten_26<-as.data.frame(as.Date("2013-12-26") %m+% years(c(0:7)))
Weihnachten_27<-as.data.frame(as.Date("2013-12-27") %m+% years(c(0:7)))

Weihnachten<-bind_rows(Weihnachten_25,Weihnachten_26, Weihnachten_27)


#Weihnachts data frame mit einer spalte erweitern und den wert 1 einfügen
Weihnachten$weihnachtsfeiertagetage_all=1

#Datumsspalte in Datum umbennen
weihnachtsfeiertage <- Neujahr %>% 
  rename(
    Datum = seq.as.Date..2013.1.1....as.Date..2019.1.1.....years..
  )

#Variable neujahrstage als csv im workspace abspeichern
write.csv(neujahrstage, file = "neujahrstage.csv")


##################################

Feiertage<-filter(besonderes, Feiertage==1)


#Variable für Weihnachten und Neujahr
## Neujahr
Neujahr<-data.frame(seq(as.Date("2013/1/1"), as.Date("2019/1/1"), "years"))


#Weihnachten als Feiertag einfügen 

Weihnachten_25<-as.data.frame(as.Date("2013-12-25") %m+% years(c(0:7)))
Weihnachten_26<-as.data.frame(as.Date("2013-12-26") %m+% years(c(0:7)))
Weihnachten_27<-as.data.frame(as.Date("2013-12-27") %m+% years(c(0:7)))

Weihnachten<-bind_rows(Weihnachten_25,Weihnachten_26, Weihnachten_27)

#Weihnachten der Feiertagetabelle hinzufügen
Feiertage<-bind_rows(Feiertage, Weihnachten)

#erstellen einer Variablen mit allen Tagen, die einen Tag vor dem Feiertag sind
vor_Feiertag<-Feiertage$Datum-days(1) 
vor_Feiertag<-as.data.frame(vor_Feiertag)

#alle Daten vor einem Feiertag
vor_Feiertag<-umsatz%>% filter(Datum %in% vor_Feiertag$vor_Feiertag)

#####EDIT:
#Vor einem Feiertag data frame mit einer spalte erweitern und den wert 1 einfügen
vor_Feiertag$vor_feiertag=1


#Variable vor_feiertag als csv im workspace abspeichern
write.csv(vor_Feiertag, file = "vor_feiertag.csv")
#####EDIT ende


#erstellen einer Variable mit den Tagen nach einem Feiertag
nach_Feiertag<-Feiertage$Datum+days(1)
nach_Feiertag<-as.data.frame(nach_Feiertag)

#alle Daten nach einem Feiertag
nach_Feiertag<-umsatz%>% filter(Datum %in% nach_Feiertag$nach_Feiertag)

#####EDIT:
#Nach einem Feiertag data frame mit einer spalte erweitern und den wert 1 einfügen
nach_Feiertag$nach_feiertag=1


#Variable vor_feiertag als csv im workspace abspeichern
write.csv(nach_Feiertag, file = "nach_feiertag.csv")
#####EDIT ende

#Versuchen, alle Tage wegzufiltern, die ein Feiertag vor einem Feiertag sind

# Als .csv im workspace abspeichern
#write.csv(Feiertage, file = "merge_um_wet_zeit.csv")
