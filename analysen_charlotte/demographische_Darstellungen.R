#Clear all
rm(list = ls())

#libraries
library(janitor)
library(stringr)
library(dplyr)
library(tidyr)
library(plyr)
library(xlsx)

#Daten einlesen
survey <- read.csv("survey_results.csv")

#Transformationen
survey2 <- t(survey)
survey3 <- row_to_names(survey2, row_number = 1)
survey4 <-as.data.frame(survey3)

#Berufe schön machen
#Alle Sonderzeichen ausser Komma und ' entfernen
Berufe <- data.frame("berufe" = gsub("\'", "", gsub("\"", "", (gsub("\\]", "",(gsub("\\[", "", survey4$Dem04)))))))

Berufe <- separate(data = Berufe, col = "berufe", sep = ",", into = c("Ausbildung_studium", "Fuehrungskraft",
                                                            "akademischer_Beruf","Techniker","Buerokraft",
                                                            "Dienstleistung_Verkaeufer","Land_Forstwirtschaft",
                                                            "Handwert","Anlagebediener_Monteur","Hilfskraft"))
#entferne whitespaces
Berufe <- as.data.frame(apply(Berufe,2,function(x)gsub('\\s+', '',x)))

#Zähle Jas
Jas <- ldply(Berufe, function(c) sum(c=="Ja"))
#Zähle Neins
Neins <- ldply(Berufe, function(c) sum(c=="Nein"))
#Zähle NaNs
NaNs <- ldply(Berufe, function(c) sum(c=="nan"))
#Zusammenfassen
Berufe_gezaehlt <- data.frame("Berufe" = Jas$.id, "Ja" = Jas$V1, "Nein" = Neins$V1, "NaN" = NaNs$V1)
Berufe_gezaehlt #78 Partizipienten

Jas

DF1 <- Berufe_gezaehlt[!Berufe_gezaehlt$Ja == 0,]
DF2 <- DF1[order(-DF1$Ja),]
counts <- DF2$Ja
names <- c("Ausbildung", "akademischer Beruf","F?hrungskraft", "B?rokraft", "DL/Verk?ufer", "Hilfskraft", "Techniker")

png(file=paste("BarPlotBerufe.png", sep=""),
    width=400, height=175)
barplot(counts, main="Berufe der Partizipienten",
        xlab="Berufe", ylab = "Anzahl", names.arg = names, col = "white")
dev.off()
#Geschlecht
Geschlecht <- data.frame("Geschlecht" = survey4$Dem02)
weiblich <- ldply(Geschlecht, function(c) sum(c=="weiblich"))
maenlich <- ldply(Geschlecht, function(c) sum(c=="männlich"))
Nans <- ldply(Geschlecht, function(c) sum(c==""))

Geschlecht_gezaehlt <- data.frame("Geschlecht" = weiblich$.id, "weiblich" = weiblich$V1, "maenlich" = maenlich$V1, "NaN" = Nans$V1)
Geschlecht_gezaehlt
#Alter
Alter <- as.numeric(as.character(survey4$Dem03))
Jahrgang_Werte <- data.frame("Arithmetisches_Mittel" =mean(Alter, na.rm = TRUE), "Median"= median(Alter, na.rm = TRUE), "Min" = min(Alter, na.rm = TRUE) , "Max"= max(Alter, na.rm = TRUE), "NA" = 4)

Jahrgang_Werte[nrow(Jahrgang_Werte) + 1,] = c(2020 - Jahrgang_Werte$Arithmetisches_Mittel[1],2020 - Jahrgang_Werte$Median[1], 2020 - Jahrgang_Werte$Min[1], 2020 - Jahrgang_Werte$Max[1], "NA")
str(Jahrgang_Werte)

#Darstellung
breaks = seq(from = 1950, to = 2005, by = 5)
png(file=paste("HistJahrgang.png", sep=""),
    width=350, height=270)
hist(Alter, breaks = breaks, main = "Jahrgang der Partizipienten", xlab = "Jahrgang", ylab = "H?ufigkeit")
dev.off()

#Bankberatungserfahrung
ErfahrungBB <- data.frame("ErfahrungBB" = survey4$ErfahrungBank)

#Zähle Jas
Jas <- ldply(ErfahrungBB, function(c) sum(c=="Ja"))
#Zähle Neins
Neins <- ldply(ErfahrungBB, function(c) sum(c=="Nein"))
#Zähle NaNs
NaNs <- ldply(ErfahrungBB, function(c) sum(c==""))

ErfahrungBB_gezaehlt <- data.frame("Berufe" = Jas$.id, "Ja" = Jas$V1, "Nein" = Neins$V1, "NaN" = NaNs$V1)

#zUSAMMENFASSUNG

Berufe_gezaehlt
Geschlecht_gezaehlt
str(Jahrgang_Werte)
ErfahrungBB_gezaehlt

write.xlsx(Berufe_gezaehlt, file="Demographie.xlsx", sheetName="Berufe", row.names=FALSE)
write.xlsx(Geschlecht_gezaehlt, file="Demographie.xlsx", sheetName="Geschlecht", append=TRUE, row.names=FALSE)
write.xlsx(Jahrgang_Werte, file="Demographie.xlsx", sheetName="Jahrgang", append=TRUE, row.names=FALSE)
write.xlsx(ErfahrungBB_gezaehlt, file="Demographie.xlsx", sheetName="Erfahrung", append=TRUE, row.names=FALSE)
