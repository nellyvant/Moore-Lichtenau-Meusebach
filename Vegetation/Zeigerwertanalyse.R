
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

#Tabellen zusammenführen und aufräumen
zeigerwerte_filled <- artenliste %>%
  left_join(zeiger_selected, by = "id")
zeigerwerte_filled <- zeigerwerte_filled %>%
  rename(Art = id)
zeigerwerte_filled <- zeigerwerte_filled %>%
  select(-layer)
zeigerwerte_filled %>%
  count(Art) %>%
  filter(n > 1)
zeigerwerte_filled <- zeigerwerte_filled %>%
  distinct(Art, .keep_all = TRUE)
sum(is.na(zeigerwerte_filled$temperature))  
#es gibt 2 Zeilen mit NAs -> das sind aber Betula_HL, was nicht bis zur Art bestimmt wurde und Lycogala epidendrum, ein Schleimpilz
zeigerwerte_filled <- zeigerwerte_filled %>%
  drop_na()
str(zeigerwerte_filled); summary(zeigerwerte_filled)
write_xlsx(zeigerwerte_filled, "Zeigerwerte-gefüllt.xlsx")

####gewichtete Zeigerwerte berechnen####
vegdata <- read.csv("Vegetationsaufnahmen_R - Kopie.csv")
rownames(vegdata) <- vegdata$Name
vegdata$Name <- NULL
ncol(vegdata)

str(vegdata)

#Problem jetzt ist, dass meine Daten im wide-format sind und nicht im long-format. Ich brauche die Daten allerdings im Longformat damit die Zuordnung besser klappt und ich die gewichteten Zeigerwerte ermitteln kann.

art_cols <- grep("_", names(vegdata), value = TRUE)
veg_long <- vegdata %>%
  rownames_to_column("Name") %>%
  pivot_longer(cols = all_of(art_cols),
               names_to = "Art_full",
               values_to = "Deckung") %>%
  filter(Deckung > 0) %>%
  tidyr::separate(Art_full, into = c("Art", "Schicht"),
                  sep = "_(?=[^_]+$)")


#Zeilen die Null sind werden entfernt, was den Datensatz erheblich kürzer machen sollte

str(veg_long)
#die Artnamen in der veglong und zeigerwerte_filled stimmen nicht überein, sodass der join nicht funktioniert. Deshalb wird die veglong als .xlsx gespeichert und in Excel mit Suchen & Ersetzen mit den Artnamen entsprechend artenliste.csv bearbeitet und anschließend wieder eingelesen
write_xlsx(veg_long, "Veglong.xlsx")
vegdata2 <- read_csv("Veglong_artnamen_bearbeitet_neu.csv")
str(vegdata2)
vegdata2$Deckung <- as.numeric(gsub(",", ".", vegdata2$Deckung))
#okay jetzt ist alles im long-Format und ich kann schauen, dass ich die gewichteten Zeigerwerte ermittel und in eine neue Excel-Tabelle überführe!((:

#gemeinsame Tabelle erstellen mit Vegetationsaufnahme und den Zeigerwerten
zeigerdata <- read_xlsx("Zeigerwerte-gefüllt-clean.xlsx")
str(zeigerdata)
zeigerdata <- zeigerdata %>%
  mutate(across(c(light, temperature, continental, wetness, alkalinity, nitrogen, salinity), as.numeric))
combined_data <- left_join(vegdata2, zeigerdata, by = "Art")
str(combined_data)
weighted_values <- combined_data %>%
  group_by(plot.ID, Name, site) %>%
  summarise(
    Weighted_light = sum(Deckung * light, na.rm = TRUE) / sum(Deckung, na.rm = TRUE),
    Weighted_temperature = sum(Deckung * temperature, na.rm = TRUE) / sum(Deckung, na.rm = TRUE),
    Weighted_wetness = sum(Deckung * wetness, na.rm = TRUE) / sum(Deckung, na.rm = TRUE),
    Weighted_nitrogen = sum(Deckung * nitrogen, na.rm = TRUE) / sum(Deckung, na.rm = TRUE),
    Weighted_continental = sum(Deckung * continental, na.rm = TRUE) / sum(Deckung, na.rm = TRUE),
    Weighted_alkalinity = sum(Deckung * alkalinity, na.rm = TRUE) / sum(Deckung, na.rm = TRUE),
    Weighted_salinity = sum(Deckung * salinity, na.rm = TRUE) / sum(Deckung, na.rm = TRUE),
    .groups = "drop"
  )

#Tabelle mit gewichteten Zeigerwerten exportieren
write_xlsx(weighted_values, "Zeigerwerte_gewichtet.xlsx")

# bis hier bin ich gekommen
#für berechnungen von evenness, shannon und richness muss ich schauen, dass dopplungen aufgrund von layer berücksichtigt werden. es gibt ja teils Art1_HL und Art1_SL oder so


####species richness, Shannon Index, relative Häufigkeit und species evenness bestimmen####

comm_matrix <- veg_long %>%
  group_by(Name, Art) %>%
  summarise(Deckung = sum(Deckung), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = Art,
    values_from = Deckung,
    values_fill = 0
  ) %>%
  tibble::column_to_rownames("Name")
str(comm_matrix)

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

