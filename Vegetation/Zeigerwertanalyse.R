
####gewichtete Landolt-Zeigerwert bestimmen####

####Vorgeplänkel####

#set working diectory

#install.packages("writexl")
library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)
library(permute)
library(vegan)

####Landolt-Zeigerwert-Tabelle erstellen####

#daten einlesen
zeigerwerte <- read.table("dwca-zeigerwerte-v1.2/ellenberg.txt",sep = "\t", header = TRUE)
artenliste <- read.csv("artenliste.csv", header=T)

#definieren welche Spalten ich brauche und übertragen möchte
zeiger_selected <- zeigerwerte %>%
  select(id,light,temperature,continental,wetness,alkalinity,nitrogen,salinity)

#Tabellen zusammenführen
zeigerwerte_filled <- artenliste %>%
  left_join(zeiger_selected, by = "id")


#bin bis hierher gekommen
#es stimmen wohl noch nciht alle artnamen in artenliste und zeigerwerte überein, weshalb es noch NAs gibt. das muss ich nochmal händisch überprüfen oder die zeigerwerte eben händisch nachtragen, wenn nix hilft



#anschauen obs geklappt hat und wie viele NAs es gibt
head(landoltdaten_filled)
sum(is.na(landoltdaten_filled$Temperaturzahl))  
#es gibt 28 Zeilen mit NAs -> händisch nachtragen und dann ordentlich definieren/ im Vortrag erwähnen das wirs so gemacht haben



#Als Excel speichern, um damit anschließend die gewichteten Zeigerwerte zuberechnen
write_xlsx(landoltdaten_filled, "Landolt-Zeigerwerte-gefüllt.xlsx")

#jetzt: händisch fehlende Zeigerwerte eintragen und dann anschließend gewichtete Zeigerwerte berechnen


####gewichtete Landolt-Zeigerwerte berechnen####

landoltdata <- read_xlsx("Landolt-Zeigerwerte-gefüllt-nachbearbeitet.xlsx")
vegdata <- read_xlsx("Vegetationsaufnahmen.xlsx")
str(vegdata)

#Problem jetzt ist, dass meine Daten im wide-format sind und nicht im long-format. Ich brauche die Daten allerdings im Longformat damit die Zuordnung besser klappt und ich die gewichteten Zeigerwerte ermitteln kann.

vegetation_long <- vegdata %>%
  pivot_longer(cols = -c(Name),  
               names_to = "Species",        
               values_to = "Coverage") %>%
  filter(Coverage > 0)

#Zeilen die Null sind werden entfernt, was den Datensatz erheblich kürzer machen sollte

str(vegetation_long)

#okay jetzt ist alles im long-Format und ich kann schauen, dass ich die gewichteten Zeigerwerte ermittel und in eine neue Excel-Tabelle überführe!((:

#gemeinsame Tabelle erstellen mit Vegetationsaufnahme und den Zeigerwerten
combined_data <- left_join(vegetation_long, landoltdata, by = "Species")

weighted_values <- combined_data %>%
  group_by(Name) %>%
  summarise(
    Weighted_Light = sum(Coverage * Lichtzahl, na.rm = TRUE) / sum(Coverage, na.rm = TRUE),
    Weighted_Temp = sum(Coverage * Temperaturzahl, na.rm = TRUE) / sum(Coverage, na.rm = TRUE),
    Weighted_Moisture = sum(Coverage * Feuchtezahl, na.rm = TRUE) / sum(Coverage, na.rm = TRUE),
    Weighted_Nutrient = sum(Coverage * Naehrstoffzahl, na.rm = TRUE) / sum(Coverage, na.rm = TRUE)
  )

#Tabelle mit gewichteten Zeigerwerten exportieren
write_xlsx(weighted_values, "Landolt_gewichtet.xlsx")


####species richness, Shannon Index, relative Häufigkeit und species evenness bestimmen####

comm_matrix <- vegetation_long %>%
  select(Name, Species, Coverage) %>%
  tidyr::pivot_wider(
    names_from = Species,
    values_from = Coverage,
    values_fill = 0
  ) %>%
  tibble::column_to_rownames("Name")


#berechnen von species richness, shannon Index, relativer Häufigkeit und Evenness

species_richness <- specnumber(comm_matrix)
shannon_index <- diversity(comm_matrix, index = "shannon")
relative_freq <- decostand(comm_matrix, method = "total")
species_evenness <- shannon_index / log(specnumber(comm_matrix))

# Ergebnisse in ein Dataframe packen:

results <- data.frame(
  Plot = rownames(comm_matrix),
  Richness = species_richness,
  Shannon = shannon_index,
  Evenness = species_evenness
)

print(results)

#als Excel speichern
write_xlsx(results, "Ergebnisse_species_richness.xlsx")

#mit diesen ganzen Ergebnissen kann ich jetzt die DCA sowie die LM´s durchführen und Diegramme erstellen

